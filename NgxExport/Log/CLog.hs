{-# LANGUAGE CPP, CApiFFI #-}

module NgxExport.Log.CLog where

import           Foreign.C.Types
import           Foreign.C.String
import           Foreign.Ptr

type CLogType = Ptr () -> CUIntPtr -> CString -> CSize -> IO ()

#if defined(__GHCIDE__)
-- avoid interactive linking against C functions with HLS
#define C_LOG_STUB(f) f :: CLogType; f _ _ _ _ = return ()
C_LOG_STUB(c_log)
C_LOG_STUB(c_log_r)
#else
foreign import capi
    "ngx_log_plugin_capi.h plugin_ngx_http_haskell_log" c_log :: CLogType
foreign import capi
    "ngx_log_plugin_capi.h plugin_ngx_http_haskell_log_r" c_log_r :: CLogType
#endif

