{-# LANGUAGE TemplateHaskell, TupleSections, MagicHash #-}

module NgxLog where

import           NgxExport

import           NgxExport.Log (logInfo)

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as C8L
import           Data.ByteString.Unsafe (unsafePackAddressLen)
import           Data.ByteString.Internal (accursedUnutterablePerformIO)
import           GHC.Prim
import           Control.Monad

packLiteral :: Int -> GHC.Prim.Addr# -> ByteString
packLiteral l s = accursedUnutterablePerformIO $ unsafePackAddressLen l s

tee :: ByteString -> IO ContentHandlerResult
tee msg = do
    void $ logInfo msg
    return $ (, packLiteral 10 "text/plain"#, 200, []) $
        flip C8L.snoc '\n' $ L.fromStrict msg

ngxExportAsyncHandler 'tee

