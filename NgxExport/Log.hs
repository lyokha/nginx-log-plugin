{-# LANGUAGE TemplateHaskell #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  NgxExport.Log
-- Copyright   :  (c) Alexey Radkov 2022
-- License     :  BSD-style
--
-- Maintainer  :  alexey.radkov@gmail.com
-- Stability   :  stable
-- Portability :  non-portable (requires Template Haskell)
--
-- Native Nginx logging from configuration files and Haskell handlers.
--
-----------------------------------------------------------------------------

module NgxExport.Log (module NgxExport.Log.Base, module Gen) where

import           NgxExport

import           NgxExport.Log.Base
import           NgxExport.Log.Gen as Gen hiding (logFuncs)
import           NgxExport.Log.Gen (logFuncs)

import           Language.Haskell.TH

concat <$> mapM (ngxExportIOYY . mkName) logFuncs

