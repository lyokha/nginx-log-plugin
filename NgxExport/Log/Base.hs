{-# LANGUAGE ForeignFunctionInterface, OverloadedStrings #-}

module NgxExport.Log.Base (LogLevel (..)
                          ,logG
                          ,logM
                          ,logR
                          ) where

import           NgxExport
import           NgxExport.Tools

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Unsafe as B
import           Foreign.C.Types
import           Foreign.C.String
import           Foreign.Ptr
import           Control.Arrow
import           Data.Char

-- BEWARE: the values must correspond to the macro definitions from ngx_log.h!
data LogLevel = LogStderr
              | LogEmerg
              | LogAlert
              | LogCrit
              | LogErr
              | LogWarn
              | LogNotice
              | LogInfo
              | LogDebug deriving Enum

foreign import ccall unsafe "plugin_ngx_http_haskell_log"
    c_log :: Ptr () -> CUIntPtr -> CString -> CSize -> IO ()

logG :: LogLevel -> ByteString -> IO ()
logG _ "" = return ()
logG l msg = do
    c <- ngxCyclePtr
    B.unsafeUseAsCStringLen msg $
        \(x, i) -> c_log c (fromIntegral $ fromEnum l) x $ fromIntegral i

foreign import ccall unsafe "plugin_ngx_http_haskell_log_r"
    c_log_r :: Ptr () -> CUIntPtr -> CString -> CSize -> IO ()

logM :: LogLevel -> Ptr () -> ByteString -> IO ()
logM l r msg = B.unsafeUseAsCStringLen msg $
    \(x, i) -> c_log_r r (fromIntegral $ fromEnum l) x $ fromIntegral i

logR :: LogLevel -> ByteString -> IO ()
logR _ "" = return ()
logR l msg = do
    let (r, v) = ngxRequestPtr &&& skipRPtr $ msg
    logM l r $ C8.dropWhile isSpace v

