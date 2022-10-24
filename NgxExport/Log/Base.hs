{-# LANGUAGE CPP, ForeignFunctionInterface, OverloadedStrings #-}

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

-- an ugly workaround of haskell-language-server's
-- [bug](https://github.com/haskell/haskell-language-server/issues/365),
-- this lets hls work with Log.hs without errors;
--
-- after more investigations:
--
-- this seems to be more generic buggy(?) behavior or realistic limitation
-- related to GHC, see [bug](https://github.com/haskell/cabal/issues/8466) and
-- [bug](https://gitlab.haskell.org/ghc/ghc/-/issues/20674): GHC tries to
-- dynamically resolve unknown symbol when running GHCI, haddock, hls, etc.;
--
-- but we do not have such an in-place library to link against as it's built by
-- Nginx as a dynamic module that gets loaded by dlopen(), even worse is that
-- the library will still have unresolved links which means that GHC won't
-- succeed at linking them;
--
-- this workaround provides stubs for c_log and c_log_r when building for hls
-- and haddock, GHCI can also run with the stubs when adding an extra option:
-- cabal repl --ghc-options=-D__GHCIDE__ --repl-options=-fobject-code

#if defined(__GHCIDE__) || defined(__HADDOCK_VERSION__)
#define C_LOG_STUB(f) \
f :: Ptr () -> CUIntPtr -> CString -> CSize -> IO (); \
f _ _ _ _ = return ()
#endif

-- | Log severity levels.
--
-- Being applied to a certain constructor, function 'fromEnum' returns the value
-- of the corresponding macro definition from /ngx_log.h/.
data LogLevel = LogStderr
              | LogEmerg
              | LogAlert
              | LogCrit
              | LogErr
              | LogWarn
              | LogNotice
              | LogInfo
              | LogDebug deriving Enum

#if defined(__GHCIDE__) || defined(__HADDOCK_VERSION__)
C_LOG_STUB(c_log)
#else
foreign import ccall unsafe "plugin_ngx_http_haskell_log"
    c_log :: Ptr () -> CUIntPtr -> CString -> CSize -> IO ()
#endif

-- | Logs a message to the global Nginx log.
logG :: LogLevel        -- ^ Log severity level
     -> ByteString      -- ^ Log message
     -> IO ()
logG _ "" = return ()
logG l msg = do
    c <- ngxCyclePtr
    B.unsafeUseAsCStringLen msg $
        \(x, i) -> c_log c (fromIntegral $ fromEnum l) x $ fromIntegral i

#if defined(__GHCIDE__) || defined(__HADDOCK_VERSION__)
C_LOG_STUB(c_log_r)
#else
foreign import ccall unsafe "plugin_ngx_http_haskell_log_r"
    c_log_r :: Ptr () -> CUIntPtr -> CString -> CSize -> IO ()
#endif

-- | Logs a message to the request's Nginx log.
--
-- This function accepts a pointer to the Nginx request object supposedly
-- unmarshalled from Nginx variable /$_r_ptr/.
logM :: LogLevel        -- ^ Log severity level
     -> Ptr ()          -- ^ Pointer to the Nginx request object
     -> ByteString      -- ^ Log message
     -> IO ()
logM l r msg = B.unsafeUseAsCStringLen msg $
    \(x, i) -> c_log_r r (fromIntegral $ fromEnum l) x $ fromIntegral i

-- | Logs a message to the request's Nginx log.
--
-- This function expects that the log message contains the value of Nginx
-- variable /$_r_ptr/ at the beginning of the log message. All whitespace
-- characters following the value of /$_r_ptr/ are skipped.
logR :: LogLevel        -- ^ Log severity level
     -> ByteString      -- ^ Log message
     -> IO ()
logR _ "" = return ()
logR l msg = do
    let (r, v) = ngxRequestPtr &&& skipRPtr $ msg
    logM l r $ C8.dropWhile isSpace v

