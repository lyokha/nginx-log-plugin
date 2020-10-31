#include <ngx_config.h>
#include <ngx_core.h>
#include <ngx_http.h>

void plugin_ngx_http_haskell_log(void *cycle_data, ngx_uint_t level,
    u_char *msg, size_t len);
void plugin_ngx_http_haskell_log_r(void *request_data, ngx_uint_t level,
    u_char *msg, size_t len);


void
plugin_ngx_http_haskell_log(void *cycle_data, ngx_uint_t level,
    u_char *msg, size_t len)
{
    ngx_cycle_t         *cycle = cycle_data;
    ngx_str_t            msg_s = { len, msg };

    ngx_log_error(level, cycle->log, 0, "%V", &msg_s);
}


void
plugin_ngx_http_haskell_log_r(void *request_data, ngx_uint_t level,
    u_char *msg, size_t len)
{
    ngx_http_request_t  *r = request_data;
    ngx_str_t            msg_s = { len, msg };
    char                *action;

    if (r == NULL) {
        return;
    }

    action = r->connection->log->action;
    r->connection->log->action = NULL;
    ngx_log_error(level, r->connection->log, 0, "%V", &msg_s);
    r->connection->log->action = action;
}

