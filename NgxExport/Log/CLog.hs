{-# LANGUAGE CApiFFI #-}

module NgxExport.Log.CLog where

import           Foreign.C.Types
import           Foreign.C.String
import           Foreign.Ptr

type CLogType = Ptr () -> CUIntPtr -> CString -> CSize -> IO ()

foreign import capi
    "ngx_log_plugin_capi.h plugin_ngx_http_haskell_log" c_log :: CLogType
foreign import capi
    "ngx_log_plugin_capi.h plugin_ngx_http_haskell_log_r" c_log_r :: CLogType

