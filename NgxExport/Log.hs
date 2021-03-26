{-# LANGUAGE TemplateHaskell #-}

module NgxExport.Log (module NgxExport.Log.Base
                     ,module Gen
                     ) where

import           NgxExport

import           NgxExport.Log.Base
import           NgxExport.Log.Gen as Gen hiding (logFuncs)
import           NgxExport.Log.Gen (logFuncs)

import           Language.Haskell.TH

concat <$> mapM (ngxExportIOYY . mkName) logFuncs

