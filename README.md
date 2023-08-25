Native Nginx logging from configuration files and Haskell handlers
==================================================================

[![Build Status](https://github.com/lyokha/nginx-log-plugin/workflows/CI/badge.svg)](https://github.com/lyokha/nginx-log-plugin/actions?query=workflow%3ACI)
[![Hackage](https://img.shields.io/hackage/v/ngx-export-log.svg?label=hackage%20%7C%20ngx-export-log&logo=haskell&logoColor=%239580D1)](https://hackage.haskell.org/package/ngx-export-log)

**Disclaimer**: this is not an Nginx module in the traditional sense! It
compiles to a shared library that gets loaded in Nginx using directive
`haskell load` from Nginx module
[*nginx-haskell-module*](https://github.com/lyokha/nginx-haskell-module). Let's
call this *plugin*. The plugin provides support for logging messages from
configuration files in the run-time using the native Nginx logging mechanism
available with directive `error_log`.

Table of contents
-----------------

- [Directives and custom handlers](#directives-and-custom-handlers)
- [An example](#an-example)
- [High-level directives log and log ultimate](#high-level-directives-log-and-log-ultimate)
- [Building and installation](#building-and-installation)

Directives and custom handlers
------------------------------

There are two flavours of logging directives. Directives `logStderr`,
`logEmerg`, `logAlert`, `logCrit`, `logErr`, `logWarn`, `logNotice`, `logInfo`,
and `logDebug` write to the *global* error log associated with the main
configuration level (i.e. the level outside of the *http* clause), while their
*R*-counterparts `logStderrR`, `logEmergR`, `logAlertR`, `logCritR`, `logErrR`,
`logWarnR`, `logNoticeR`, `logInfoR`, and `logDebugR` write to a specific for
the current location *http* error log. The *R* directives require the request
context, and therefore they are heavier in use and speed than the *simple* (or
*global*) logging directives, however they log the useful request context data
which is unavailable in the global context.

Haskell functions of the same names as the logging directives can be used in
custom Haskell handlers. Besides them, there are three generic functions `logG`,
`logR`, and `logM` which expect a log level of type `LogLevel` as their first
argument (this type contains values `LogStderr`, `LogEmerg`, `LogAlert`,
`LogCrit`, `LogErr`, `LogWarn`, `LogNotice`, `LogInfo`, and `LogDebug`).
Function `logM` is essentially `logR` with the request context explicitly
separated from the log message in the parameters. The *global* logging functions
can be used in custom *services* and *service hooks* (in terms of
*nginx-haskell-module*), while the *R* functions (including `logM`) are useful
in other synchronous and asynchronous handlers.

An example
----------

###### File *ngx_log.hs*

```haskell
{-# LANGUAGE TemplateHaskell, TupleSections, MagicHash #-}

module NgxLog where

import           NgxExport
import           NgxExport.Tools.Read (skipRPtr)

import           NgxExport.Log (logR, LogLevel (..))

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as C8L
import           Data.ByteString.Unsafe (unsafePackAddressLen)
import           Data.ByteString.Internal (accursedUnutterablePerformIO)
import           Data.Char
import           GHC.Prim

packLiteral :: Int -> GHC.Prim.Addr# -> ByteString
packLiteral l s = accursedUnutterablePerformIO $ unsafePackAddressLen l s

tee :: ByteString -> IO ContentHandlerResult
tee msg = do
    logR LogInfo msg
    return $ (, packLiteral 10 "text/plain"#, 200, []) $
        flip C8L.snoc '\n' $ L.fromStrict $ C8.dropWhile isSpace $ skipRPtr msg

ngxExportAsyncHandler 'tee
```

Here we used function `logR` with argument `LogInfo` to make asynchronous
content handler *tee* that echoes its argument both in the response body and the
error log. All Haskell handlers used in logging directives are exported
automatically.

###### File *nginx.conf*

```nginx
user                    nobody;
worker_processes        4;

events {
    worker_connections  1024;
}

error_log               /tmp/nginx-test-error-g.log info;

http {
    default_type        application/octet-stream;
    sendfile            on;

    error_log           /tmp/nginx-test-error.log info;
    access_log          /tmp/nginx-test-access.log;

    haskell load        /var/lib/nginx/ngx_log.so;

    server {
        listen          8010;
        server_name     main;

        haskell_run logInfo !$msgG "Write in global log!";

        location / {
            haskell_run logInfoR <~$msg '$_r_ptr
                    Got query "$args"';

            if ($arg_a) {
                haskell_run logInfoR <~$msg '$_r_ptr
                        Got a = "$arg_a"';
            }

            haskell_run logInfoR !$msgR '$_r_ptr
                    Request finished';

            echo Ok;
        }

        location /tee {
            haskell_async_content tee "$_r_ptr
                    Hello, world!";
        }
    }
}
```

There is the *global* error log */tmp/nginx-test-error-g.log* where directive
`logInfo` will write to, and an *http* error log */tmp/nginx-test-error.log*
declared inside the *http* clause where directives `logInfoR` will write to.
Notice that the *R* directives and handlers require variable `$_r_ptr` to
properly log messages: missing this variable may cause crashes of Nginx worker
processes! Notice also that we used a *strict volatile* variable `<~$msg` for
logging *early* messages inside locations: using a single regular strict early
variable (say `<!$msg`) in a single request which would have triggered *if*
blocks and rewrites between locations could lead to missing log messages.

Note that in *nginx-haskell-module* version *3.0* and higher, the *handler(r)*
syntax is available which means that directives using handler's arguments
with `$_r_ptr` in the beginning, for example

```nginx
            haskell_async_content tee "$_r_ptr
                    Hello, world!";
```

can be rewritten in a more nice-looking style like

```nginx
            haskell_async_content tee(r) "Hello, world!";
```

###### A simple test

Let's watch the log files,

```ShellSession
$ tail -f /tmp/nginx-test-*
```

and run a test in another terminal.

```ShellSession
$ curl 'http://localhost:8010/?a=hello&b=world'
Ok
$ curl 'http://localhost:8010/tee'
Hello, world!
```

In the first terminal the following lines should appear.

```ShellSession
==> /tmp/nginx-test-error.log <==
2020/10/21 12:33:28 [info] 20676#0: *1 Got query "a=hello&b=world", client: 127.0.0.1, server: main, request: "GET /?a=hello&b=world HTTP/1.1", host: "localhost:8010"
2020/10/21 12:33:28 [info] 20676#0: *1 Got a = "hello", client: 127.0.0.1, server: main, request: "GET /?a=hello&b=world HTTP/1.1", host: "localhost:8010"

==> /tmp/nginx-test-access.log <==
127.0.0.1 - - [21/Oct/2020:12:33:28 +0300] "GET /?a=hello&b=world HTTP/1.1" 200 13 "-" "curl/7.69.1"

==> /tmp/nginx-test-error-g.log <==
2020/10/21 12:33:28 [info] 20676#0: Write in global log!

==> /tmp/nginx-test-error.log <==
2020/10/21 12:33:28 [info] 20676#0: *1 Request finished, client: 127.0.0.1, server: main, request: "GET /?a=hello&b=world HTTP/1.1", host: "localhost:8010"
2020/10/21 12:33:28 [info] 20676#0: *1 client 127.0.0.1 closed keepalive connection
2020/10/21 12:33:30 [info] 20676#0: *2 Hello, world!, client: 127.0.0.1, server: main, request: "GET /tee HTTP/1.1", host: "localhost:8010"

==> /tmp/nginx-test-access.log <==
127.0.0.1 - - [21/Oct/2020:12:33:30 +0300] "GET /tee HTTP/1.1" 200 14 "-" "curl/7.69.1"

==> /tmp/nginx-test-error-g.log <==
2020/10/21 12:33:30 [info] 20676#0: Write in global log!

==> /tmp/nginx-test-error.log <==
2020/10/21 12:33:30 [info] 20676#0: *2 client 127.0.0.1 closed keepalive connection
```

See more [*examples of typical use cases and
gotchas*](https://github.com/lyokha/nginx-log-plugin/blob/1b3cc6f6f037f73671c1fea19299e45babeadb39/test/nginx.conf#L81).

High-level directives log and log ultimate
------------------------------------------

Directives `haskell_run log...` are overflowed with gory details including
embarrassing handler variables and variable `$_r_ptr`. Directives `log` and
`log ultimate` were introduced to hide these details by setting reasonable
defaults. For using them, an additional Nginx *dynamic module* must be built and
loaded.

```nginx
load_module             /var/lib/nginx/modules/ngx_log_plugin_module.so;
```

Location */* from the example shown in the previous section could be rewritten
as

```nginx
        location / {
            log info 'Got query "$args"';

            if ($arg_a) {
                log info 'Got a = "$arg_a"';
            }

            log ultimate info 'Request finished';

            if ($arg_c) {
                rewrite ^ /rewr last;
            }

            echo Ok;
        }
```

Directive `log` introduces handler variables `<!$_log_Rsrv0` through
`<!$_log_Rsrv3` in *server* clauses and `<~$_log_V0` through `<~$_log_V3` in
*location* and *location-if* clauses. Directive `log ultimate` introduces
handler variables `!$_log_Usrv0` through `!$_log_Usrv3` in *server* clauses and
`!$_log_U0` through `!$_log_U3` in *location* and *location-if* clauses. The
extra variables with suffixes *1* through *3* are used when the directives get
declared multiple times at a single configuration level.

Both directives `log` and `log ultimate` use the *R* log handlers which means
that there is no high-level log directive for writing into the global log.

Building and installation
-------------------------

The plugin contains Haskell and C parts, and thus, it requires *ghc*, *cabal*,
*gcc*, and a directory with the Nginx sources. The build tool also requires
[*patchelf*](https://github.com/NixOS/patchelf) and utility *nhm-tool* which is
shipped with package *ngx-export-distribution*.

Let's first install the Nginx module. For this, go to the directory with the
Nginx source code,

```ShellSession
$ cd /path/to/nginx/sources
```

compile,

```ShellSession
$ ./configure --add-dynamic-module=/path/to/nginx-log-plugin/sources --add-dynamic-module=/path/to/nginx-log-plugin/sources/module
$ make modules
```

and install *ngx_log_plugin.so*.

```ShellSession
$ export NGX_HS_INSTALL_DIR=/var/lib/nginx
$ sudo install -d $NGX_HS_INSTALL_DIR
$ sudo cp objs/ngx_log_plugin.so $NGX_HS_INSTALL_DIR/libngx_log_plugin.so
```

Notice that we added prefix *lib* to the module's name!

For using directives `log` and `log ultimate`, the dynamic module
*ngx_log_plugin_module* must also be installed.

```ShellSession
$ sudo install -d $NGX_HS_INSTALL_DIR/modules
$ sudo cp objs/ngx_log_plugin_module.so $NGX_HS_INSTALL_DIR/modules
```

Now let's build the Haskell code.

```ShellSession
$ cd -
$ cd simple
```

Then

```ShellSession
$ make PREFIX=$NGX_HS_INSTALL_DIR
$ sudo make PREFIX=$NGX_HS_INSTALL_DIR install
```

or simply

```ShellSession
$ make
$ sudo make install
```

if installation directory is */var/lib/nginx/*.

With ghc older than *8.10.6*, build with

```ShellSession
$ make LINKRTS=-lHSrts_thr-ghc$(ghc --numeric-version)
```

By default, package *ngx-export-log* gets installed from *Hackage*. To build it
locally, augment stanza *packages* inside
[*cabal.project*](simple/cabal.project) according to the commentary attached to
it.

