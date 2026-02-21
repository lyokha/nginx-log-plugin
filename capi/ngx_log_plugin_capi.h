#include <stddef.h>
#include <stdint.h>

void plugin_ngx_http_haskell_log(void *cycle, uintptr_t level,
    char *msg, size_t len);
void plugin_ngx_http_haskell_log_r(void *r, uintptr_t level,
    char *msg, size_t len);

