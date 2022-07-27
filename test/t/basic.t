# vi:filetype=

use Test::Nginx::Socket;

repeat_each(2);
plan tests => repeat_each() * (5 * blocks() + 4 - 1);

no_shuffle();
run_tests();

__DATA__

=== TEST 1: main
--- http_config
    haskell load $TEST_NGINX_LLIB_DIR/ngx_log.so;
--- config
        haskell_run logInfo !$msgG "Write in global log!";

        location /main {
            haskell_run logInfoR(r) <~$msg 'Got query "$args"';

            if ($arg_a) {
                haskell_run logInfoR(r) <~$msg 'Got a = "$arg_a"';
            }

            haskell_run logInfoR(r) !$msgR 'Request finished';

            if ($arg_c) {
                rewrite ^ /rewr last;
            }

            echo Ok;
        }

        location /rewr {
            haskell_run logInfoR(r) <~$msg '[In /rewr] Got query "$args"';

            if ($arg_a) {
                haskell_run logInfoR(r) <~$msg '[In /rewr] Got a = "$arg_a"';
            }

            echo "In /rewr";
        }

        location /tee {
            haskell_async_content tee(r) "Hello, world!";
        }

        location /log {
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
--- request
    GET /main
--- error_log eval
    ['Got query ""',
     'Request finished',
     'Write in global log!']
--- response_body
Ok
--- error_code: 200

=== TEST 2: main_args
--- request
    GET /main?a=hello&b=world
--- error_log eval
    ['Got query "a=hello&b=world"',
     'Got a = "hello"',
     'Request finished',
     'Write in global log!']
--- response_body
Ok
--- error_code: 200

=== TEST 3: main_rewr
--- request
    GET /main?a=hello&b=world&c=1
--- error_log eval
    ['Got query "a=hello&b=world&c=1"',
     '[In /rewr] Got query "a=hello&b=world&c=1"',
     '[In /rewr] Got a = "hello"',
     'Write in global log!']
--- response_body
In /rewr
--- error_code: 200

=== TEST 4: tee
--- request
    GET /tee
--- error_log eval
    ['Hello, world!',
     'Write in global log!']
--- response_body
Hello, world!
--- error_code: 200

=== TEST 5: log_args
--- request
    GET /log?a=hello&b=world
--- error_log eval
    ['Got query "a=hello&b=world"',
     'Got a = "hello"',
     'Request finished',
     'Write in global log!']
--- response_body
Ok
--- error_code: 200

=== TEST 6: log_rewr
--- request
    GET /main?a=hello&b=world&c=1
--- error_log eval
    ['Got query "a=hello&b=world&c=1"',
     '[In /rewr] Got query "a=hello&b=world&c=1"',
     '[In /rewr] Got a = "hello"',
     'Write in global log!']
--- response_body
In /rewr
--- error_code: 200

