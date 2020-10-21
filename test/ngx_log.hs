{-# LANGUAGE TemplateHaskell, TupleSections, MagicHash #-}

module NgxLog where

import           NgxExport
import           NgxExport.Tools (skipRPtr)

import           NgxExport.Log (logInfoR)

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as C8L
import           Data.ByteString.Unsafe (unsafePackAddressLen)
import           Data.ByteString.Internal (accursedUnutterablePerformIO)
import           Data.Char
import           GHC.Prim
import           Control.Monad

packLiteral :: Int -> GHC.Prim.Addr# -> ByteString
packLiteral l s = accursedUnutterablePerformIO $ unsafePackAddressLen l s

tee :: ByteString -> IO ContentHandlerResult
tee msg = do
    void $ logInfoR msg
    return $ (, packLiteral 10 "text/plain"#, 200, []) $
        flip C8L.snoc '\n' $ L.fromStrict $ C8.dropWhile isSpace $ skipRPtr msg

ngxExportAsyncHandler 'tee

