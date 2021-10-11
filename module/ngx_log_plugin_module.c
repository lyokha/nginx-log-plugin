#include "ngx_http_haskell_module.h"


typedef struct {
    ngx_uint_t  count:2;
    ngx_uint_t  count_ultimate:2;
} ngx_http_log_plugin_loc_conf_t;


static void *ngx_http_log_plugin_create_loc_conf(ngx_conf_t *cf);
static char *ngx_http_log_plugin_log(ngx_conf_t *cf, ngx_command_t *cmd,
    void *conf);


static ngx_command_t  ngx_http_log_plugin_commands[] = {

    { ngx_string("log"),
      NGX_HTTP_SRV_CONF|NGX_HTTP_LOC_CONF|NGX_HTTP_LIF_CONF|NGX_CONF_TAKE23,
      ngx_http_log_plugin_log,
      NGX_HTTP_LOC_CONF_OFFSET,
      0,
      NULL },

      ngx_null_command
};


static ngx_http_module_t  ngx_http_log_plugin_ctx = {
    NULL,                                  /* preconfiguration */
    NULL,                                  /* postconfiguration */

    NULL,                                  /* create main configuration */
    NULL,                                  /* init main configuration */

    NULL,                                  /* create server configuration */
    NULL,                                  /* merge server configuration */

    ngx_http_log_plugin_create_loc_conf,   /* create location configuration */
    NULL                                   /* merge location configuration */
};


ngx_module_t  ngx_log_plugin_module = {
    NGX_MODULE_V1,
    &ngx_http_log_plugin_ctx,              /* module context */
    ngx_http_log_plugin_commands,          /* module directives */
    NGX_HTTP_MODULE,                       /* module type */
    NULL,                                  /* init master */
    NULL,                                  /* init module */
    NULL,                                  /* init process */
    NULL,                                  /* init thread */
    NULL,                                  /* exit thread */
    NULL,                                  /* exit process */
    NULL,                                  /* exit master */
    NGX_MODULE_V1_PADDING
};


static void *
ngx_http_log_plugin_create_loc_conf(ngx_conf_t *cf)
{
    ngx_http_log_plugin_loc_conf_t  *lcf;

    lcf = ngx_pcalloc(cf->pool, sizeof(ngx_http_log_plugin_loc_conf_t));

    return lcf;
}


static char *
ngx_http_log_plugin_log(ngx_conf_t *cf, ngx_command_t *cmd, void *conf)
{
    ngx_http_log_plugin_loc_conf_t  *lcf = conf;

    ngx_str_t                       *value;
    ngx_conf_t                       cf_haskell_run;
    ngx_array_t                      cf_haskell_run_args;
    ngx_str_t                       *directive, *handler, *var, *msg;
    ngx_http_haskell_main_conf_t    *hmcf;
    ngx_http_haskell_loc_conf_t     *hlcf;
    char                            *var_name;
    ngx_uint_t                       n_args;
    ngx_uint_t                       ultimate = 0;

    value = cf->args->elts;

    if (cf->args->nelts == 4) {
        if (value[1].len != 8 || ngx_strncmp(value[1].data, "ultimate", 8) != 0)
        {
            ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                               "wrong declaration of directive log");
            return NGX_CONF_ERROR;
        }
        ultimate = 1;
    }

    if (ultimate) {
        if (lcf->count_ultimate == 4) {
            ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                               "too many declarations of directive "
                               "log ultimate at a single configuration level");
            return NGX_CONF_ERROR;
        }
    } else {
        if (lcf->count == 4) {
            ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                               "too many declarations of directive "
                               "log at a single configuration level");
            return NGX_CONF_ERROR;
        }
    }

    if (ngx_array_init(&cf_haskell_run_args, cf->pool, 4, sizeof(ngx_str_t))
        != NGX_OK)
    {
        ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                           "failed to allocate memory for directive log data");
        return NGX_CONF_ERROR;
    }

    directive = ngx_array_push(&cf_haskell_run_args);
    if (directive == NULL) {
        return NGX_CONF_ERROR;
    }

    ngx_str_set(directive, "haskell_run");

    handler = ngx_array_push(&cf_haskell_run_args);
    if (handler == NULL) {
        return NGX_CONF_ERROR;
    }

    n_args = 1 + ultimate;

    if (value[n_args].len == 5
        && ngx_strncmp(value[n_args].data, "debug", 5) == 0)
    {
        ngx_str_set(handler, "logDebugR");
    } else if (value[n_args].len == 4
        && ngx_strncmp(value[n_args].data, "info", 4) == 0)
    {
        ngx_str_set(handler, "logInfoR");
    } else if (value[n_args].len == 6
        && ngx_strncmp(value[n_args].data, "notice", 6) == 0)
    {
        ngx_str_set(handler, "logNoticeR");
    } else if (value[n_args].len == 4
        && ngx_strncmp(value[n_args].data, "warn", 4) == 0)
    {
        ngx_str_set(handler, "logWarnR");
    } else if (value[n_args].len == 3
        && ngx_strncmp(value[n_args].data, "err", 3) == 0)
    {
        ngx_str_set(handler, "logErrR");
    } else if (value[n_args].len == 4
        && ngx_strncmp(value[n_args].data, "crit", 4) == 0)
    {
        ngx_str_set(handler, "logCritR");
    } else if (value[n_args].len == 5
        && ngx_strncmp(value[n_args].data, "alert", 5) == 0)
    {
        ngx_str_set(handler, "logAlertR");
    } else if (value[n_args].len == 5
        && ngx_strncmp(value[n_args].data, "emerg", 5) == 0)
    {
        ngx_str_set(handler, "logEmergR");
    } else if (value[n_args].len == 6
        && ngx_strncmp(value[n_args].data, "stderr", 6) == 0)
    {
        ngx_str_set(handler, "logStderrR");
    } else {
        ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                           "unexpected log level \"%V\"", &value[n_args]);
        return NGX_CONF_ERROR;
    }

    var = ngx_array_push(&cf_haskell_run_args);
    if (var == NULL) {
        return NGX_CONF_ERROR;
    }

    if (ultimate) {
        if (cf->cmd_type & NGX_HTTP_SRV_CONF) {
            var_name = "!$msgUsrv";
            var->len = 10;
        } else {
            var_name = "!$msgU";
            var->len = 7;
        }
    } else {
        if (cf->cmd_type & NGX_HTTP_SRV_CONF) {
            var_name = "<!$msgRsrv";
            var->len = 11;
        } else {
            var_name = "<~$msgV";
            var->len = 8;
        }
    }

    var->data = ngx_pnalloc(cf->pool, var->len);
    if (var->data == NULL) {
        return NGX_CONF_ERROR;
    }
    ngx_memcpy(var->data, (u_char *) var_name, var->len - 1);
    var->data[var->len - 1] =
            '0' + (ultimate ? lcf->count_ultimate++ : lcf->count++);

    msg = ngx_array_push(&cf_haskell_run_args);
    if (msg == NULL) {
        return NGX_CONF_ERROR;
    }

    hmcf = ngx_http_conf_get_module_main_conf(cf, ngx_http_haskell_module);

    msg->len = hmcf->request_var_name.len + 2 + value[n_args + 1].len;
    msg->data = ngx_pnalloc(cf->pool, msg->len);
    if (msg->data == NULL) {
        return NGX_CONF_ERROR;
    }
    msg->data[0] = '$';
    ngx_memcpy(msg->data + 1,
               hmcf->request_var_name.data, hmcf->request_var_name.len);
    msg->data[hmcf->request_var_name.len + 1] = ' ';
    ngx_memcpy(msg->data + 1 + hmcf->request_var_name.len + 1,
               value[n_args + 1].data, value[n_args + 1].len);

    cf_haskell_run = *cf;
    cf_haskell_run.args = &cf_haskell_run_args;

    hlcf = ngx_http_conf_get_module_loc_conf(cf, ngx_http_haskell_module);

    return (ngx_http_haskell_run(&cf_haskell_run, NULL, hlcf));
}

