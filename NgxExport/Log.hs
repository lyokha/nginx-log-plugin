{-# LANGUAGE TemplateHaskell, ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings, TypeApplications #-}

module NgxExport.Log where

import           NgxExport
import           NgxExport.Tools

import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Unsafe as B
import           Control.Arrow
import           Foreign.C.Types
import           Foreign.C.String
import           Foreign.Ptr
import           Foreign.Storable
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
logG l msg = do
    c <- ngxCyclePtr
    B.unsafeUseAsCStringLen msg $
        \(x , i) -> c_log c (fromIntegral $ fromEnum l) x $ fromIntegral i

foreign import ccall unsafe "plugin_ngx_http_haskell_log_r"
    c_log_r :: Ptr () -> CUIntPtr -> CString -> CSize -> IO ()

logR :: LogLevel -> ByteString -> IO ()
logR l msg = do
    let (r, v) = ngxRequestPtr &&& skipRPtr $ msg
    B.unsafeUseAsCStringLen (C8.dropWhile isSpace v) $
        \(x , i) -> c_log_r r (fromIntegral $ fromEnum l) x $ fromIntegral i
    where skipRPtr = B.drop $ sizeOf (undefined :: Word)

logStderr :: ByteString -> IO L.ByteString
logStderr msg = logG LogStderr msg >> return ""
ngxExportIOYY 'logStderr

logEmerg :: ByteString -> IO L.ByteString
logEmerg msg = logG LogEmerg msg >> return ""
ngxExportIOYY 'logEmerg

logAlert :: ByteString -> IO L.ByteString
logAlert msg = logG LogAlert msg >> return ""
ngxExportIOYY 'logAlert

logCrit :: ByteString -> IO L.ByteString
logCrit msg = logG LogCrit msg >> return ""
ngxExportIOYY 'logCrit

logErr :: ByteString -> IO L.ByteString
logErr msg = logG LogErr msg >> return ""
ngxExportIOYY 'logErr

logWarn :: ByteString -> IO L.ByteString
logWarn msg = logG LogWarn msg >> return ""
ngxExportIOYY 'logWarn

logNotice :: ByteString -> IO L.ByteString
logNotice msg = logG LogNotice msg >> return ""
ngxExportIOYY 'logNotice

logInfo :: ByteString -> IO L.ByteString
logInfo msg = logG LogInfo msg >> return ""
ngxExportIOYY 'logInfo

logDebug :: ByteString -> IO L.ByteString
logDebug msg = logG LogDebug msg >> return ""
ngxExportIOYY 'logDebug

logStderrR :: ByteString -> IO L.ByteString
logStderrR msg = logR LogStderr msg >> return ""
ngxExportIOYY 'logStderrR

logEmergR :: ByteString -> IO L.ByteString
logEmergR msg = logR LogEmerg msg >> return ""
ngxExportIOYY 'logEmergR

logAlertR :: ByteString -> IO L.ByteString
logAlertR msg = logR LogAlert msg >> return ""
ngxExportIOYY 'logAlertR

logCritR :: ByteString -> IO L.ByteString
logCritR msg = logR LogCrit msg >> return ""
ngxExportIOYY 'logCritR

logErrR :: ByteString -> IO L.ByteString
logErrR msg = logR LogErr msg >> return ""
ngxExportIOYY 'logErrR

logWarnR :: ByteString -> IO L.ByteString
logWarnR msg = logR LogWarn msg >> return ""
ngxExportIOYY 'logWarnR

logNoticeR :: ByteString -> IO L.ByteString
logNoticeR msg = logR LogNotice msg >> return ""
ngxExportIOYY 'logNoticeR

logInfoR :: ByteString -> IO L.ByteString
logInfoR msg = logR LogInfo msg >> return ""
ngxExportIOYY 'logInfoR

logDebugR :: ByteString -> IO L.ByteString
logDebugR msg = logR LogDebug msg >> return ""
ngxExportIOYY 'logDebugR

