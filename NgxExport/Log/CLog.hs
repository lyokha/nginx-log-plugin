{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module NgxExport.Log.CLog where

import           Foreign.C.Types
import           Foreign.C.String
import           Foreign.Ptr

type CLogType = Ptr () -> CUIntPtr -> CString -> CSize -> IO ()

-- Some tools such as hls, haddock, and ghci run interactive linking against C
-- functions plugin_ngx_http_haskell_log() and plugin_ngx_http_haskell_log_r()
-- when loading Log.hs. In Log.hs, TH declarations from Log/Gen.hs which make
-- calls to those C functions, get instantiated. Obviously, linking fails as
-- soon as we don't have a library to expose the functions because such a
-- library is built by Nginx and we don't want to use Nginx at this step.
--
-- In this workaround, the C functions get replaced by stubs when running by
-- hls or haddock. This prevents interactive linking in Log.hs. It's easy to
-- detect that the code is being run by hls or haddock: the tools define their
-- own C macro declarations __GHCIDE__ and __HADDOCK_VERSION__ respectively. To
-- prevent interactive linking in ghci, pass one of the two macro declarations
-- (or macro NGX_CSTUB, see below) in an appropriate option, e.g.
--
-- cabal repl --ghc-options=-DNGX_CSTUB --repl-options=-fobject-code
--
-- In newer haddock releases (as of 2025), option __HADDOCK_VERSION__ gets no
-- longer set. In this case, build haddocks with
--
-- cabal haddock --haddock-for-hackage --haddock-options=--optghc=-DNGX_CSTUB

#if defined(__GHCIDE__) || defined(__HADDOCK_VERSION__) || defined(NGX_CSTUB)
#define C_LOG_STUB(f) f :: CLogType; f _ _ _ _ = return ()
C_LOG_STUB(c_log)
C_LOG_STUB(c_log_r)
#else
foreign import ccall unsafe "plugin_ngx_http_haskell_log" c_log :: CLogType
foreign import ccall unsafe "plugin_ngx_http_haskell_log_r" c_log_r :: CLogType
#endif

