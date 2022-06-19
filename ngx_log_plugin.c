#include <ngx_config.h>
#include <ngx_core.h>
#include <ngx_http.h>

void plugin_ngx_http_haskell_log(ngx_cycle_t *cycle, ngx_uint_t level,
    u_char *msg, size_t len);
void plugin_ngx_http_haskell_log_r(ngx_http_request_t *r, ngx_uint_t level,
    u_char *msg, size_t len);


void
plugin_ngx_http_haskell_log(ngx_cycle_t *cycle, ngx_uint_t level,
    u_char *msg, size_t len)
{
    ngx_str_t            msg_s = { len, msg };

    ngx_log_error(level, cycle->log, 0, "%V", &msg_s);
}


void
plugin_ngx_http_haskell_log_r(ngx_http_request_t *r, ngx_uint_t level,
    u_char *msg, size_t len)
{
    ngx_str_t            msg_s = { len, msg };
    char                *action;
    ngx_uint_t           swap_action;

    action = r->connection->log->action;
    swap_action = action == NULL ? 0 : 1;

    if (swap_action) {
        r->connection->log->action = NULL;
    }
    ngx_log_error(level, r->connection->log, 0, "%V", &msg_s);
    if (swap_action) {
        r->connection->log->action = action;
    }
}

