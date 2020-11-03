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
    ngx_uint_t           swap_action;

    if (r == NULL) {
        return;
    }

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

