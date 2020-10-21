Native Nginx logging from configuration files
=============================================

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
- [Building and installation](#building-and-installation)

Directives and custom handlers
------------------------------

There are two flavours of logging directives. Directives `logStderr`,
`logEmerg`, `logAlert`, `logCrit`, `logErr`, `logWarn`, `logNotice`, `logInfo`,
and `logDebug` write to the global error log associated with the main
configuration level (i.e. the level outside of the *http* clause), while their
*R*-counterparts `logStderrR`, `logEmergR`, `logAlertR`, `logCritR`, `logErrR`,
`logWarnR`, `logNoticeR`, `logInfoR`, and `logDebugR` write to the specific for
the current location error log. The *R* directives require the request context,
and therefore they are heavier in use and speed than the *simple* directives and
should be avoided when Nginx logs all messages into a single destination.

Haskell functions of the same names as the logging directives can be used in
custom Haskell handlers.

An example
----------

###### File *ngx_log.hs*

```haskell
{-# LANGUAGE TemplateHaskell, TupleSections, MagicHash #-}

module NgxLog where

import           NgxExport

import           NgxExport.Log (logInfo)

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as C8L
import           Data.ByteString.Unsafe (unsafePackAddressLen)
import           Data.ByteString.Internal (accursedUnutterablePerformIO)
import           GHC.Prim
import           Control.Monad

packLiteral :: Int -> GHC.Prim.Addr# -> ByteString
packLiteral l s = accursedUnutterablePerformIO $ unsafePackAddressLen l s

tee :: ByteString -> IO ContentHandlerResult
tee msg = do
    void $ logInfo msg
    return $ (, packLiteral 10 "text/plain"#, 200, []) $
        flip C8L.snoc '\n' $ L.fromStrict msg

ngxExportAsyncHandler 'tee
```

Here we used function `logInfo` to make asynchronous content handler *tee* that
echoes its argument both in the response body and the global error log. All
Haskell handlers for using in logging directives are exported automatically.

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

    haskell load /var/lib/nginx/ngx_log.so;

    server {
        listen          8010;
        server_name     main;

        haskell_run logInfo !$msgG "Write in global log!";

        location / {
            haskell_run logInfoR <!$msg0 '$_r_ptr
                    Got query "$args"';

            if ($arg_a) {
                haskell_run logInfoR <!$msg1 '$_r_ptr
                        Got a = "$arg_a"';
            }

            haskell_run logInfoR !$msg2 '$_r_ptr
                    Request finished';

            echo Ok;
        }

        location /tee {
            haskell_async_content tee "Hello, world!";
        }
    }
}
```

There is a global error log */tmp/nginx-test-error-g.log* where directive
`logInfo` will write to, and an error log */tmp/nginx-test-error.log* declared
inside the *http* clause where directives `logInfoR` will write to. Notice that
the *R* directives require variable `$_r_ptr` to properly log messages: missing
this variable may lead to crashes of Nginx worker processes!

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
2020/10/21 10:19:24 [info] 7672#0: *1 Got query "a=hello&b=world", client: 127.0.0.1, server: main, request: "GET /?a=hello&b=world HTTP/1.1", host: "localhost:8010"
2020/10/21 10:19:24 [info] 7672#0: *1 Got a = "hello", client: 127.0.0.1, server: main, request: "GET /?a=hello&b=world HTTP/1.1", host: "localhost:8010"

==> /tmp/nginx-test-access.log <==
127.0.0.1 - - [21/Oct/2020:10:19:24 +0300] "GET /?a=hello&b=world HTTP/1.1" 200 13 "-" "curl/7.69.1"

==> /tmp/nginx-test-error-g.log <==
2020/10/21 10:19:24 [info] 7672#0: Write in global log!

==> /tmp/nginx-test-error.log <==
2020/10/21 10:19:24 [info] 7672#0: *1 Request finished while logging request, client: 127.0.0.1, server: main, request: "GET /?a=hello&b=world HTTP/1.1", host: "localhost:8010"
2020/10/21 10:19:24 [info] 7672#0: *1 client 127.0.0.1 closed keepalive connection

==> /tmp/nginx-test-error-g.log <==
2020/10/21 10:19:39 [info] 7672#0: Hello, world!

==> /tmp/nginx-test-access.log <==
127.0.0.1 - - [21/Oct/2020:10:19:39 +0300] "GET /tee HTTP/1.1" 200 14 "-" "curl/7.69.1"

==> /tmp/nginx-test-error-g.log <==
2020/10/21 10:19:39 [info] 7672#0: Write in global log!

==> /tmp/nginx-test-error.log <==
2020/10/21 10:19:39 [info] 7672#0: *2 client 127.0.0.1 closed keepalive connection
```

Building and installation
-------------------------

The plugin contains Haskell and C parts, and thus it requires *ghc*, *cabal*,
*gcc*, and a directory with the Nginx sources.

To install module *NgxExport.Log*, run

```ShellSession
$ cabal v1-install
```

(you may prefer the *new-style* cabal command *v2-install*).

Then go to the directory with the Nginx source code,

```ShellSession
$ cd /path/to/nginx/sources
```

compile,

```ShellSession
$ ./configure --add-dynamic-module=/path/to/nginx-log-plugin/sources
$ make modules
```

and install *ngx_log_plugin.so* being a superuser.

```ShellSession
# cp objs/ngx_log_plugin.so /var/lib/nginx/hslibs/libngx_log_plugin.so
```

When building a custom library, import the Haskell module.

```haskell
import NgxExport.Log ()
```

The custom library (such as *ngx_log.hs* from the example above) must be linked
against the C code.

```ShellSession
$ export NGX_MODULE_PATH=/var/lib/nginx/hslibs
$ ghc -Wall -O2 -dynamic -shared -fPIC -lHSrts_thr-ghc$(ghc --numeric-version) -L$NGX_MODULE_PATH -lngx_log_plugin custom.hs -o custom.so -fforce-recomp
```

It's time to collect all dependent libraries, patch *custom.so*, and install
everything. The custom library can be patched by utility
[*hslibdeps*](https://github.com/lyokha/nginx-haskell-module/blob/master/utils/README.md#utility-hslibdeps).

```ShellSession
$ export HSLIBS_INSTALL_DIR=/var/lib/nginx/hslibs
$ hslibdeps -t $HSLIBS_INSTALL_DIR custom.so
```

If values of *HSLIBS_INSTALL_DIR* and *NGX_MODULE_PATH* differ then the second
path must be added too.

```ShellSession
$ hslibdeps -t $NGX_MODULE_PATH custom.so
```

Copy library *custom.so* into directory */var/lib/nginx/* (this must correspond
to the directory specified in Nginx directive *haskell load*) being a superuser.

```ShellSession
# cp custom.so /var/lib/nginx
```

Then copy all dependent Haskell libraries into the target directory.

```ShellSession
# cp -v .hslibs/* $HSLIBS_INSTALL_DIR
```

