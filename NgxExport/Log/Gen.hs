{-# LANGUAGE TemplateHaskell #-}

module NgxExport.Log.Gen where

import           NgxExport.Log.Base

import           Language.Haskell.TH
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import           Data.Char

do
    TyConI (DataD _ _ _ _ lCs _) <- reify ''LogLevel
    let lCons = map (\(NormalC con []) -> con) lCs
        lCons' = lCons ++ map (mkName . (++ "R") . nameBase) lCons
        flf = mkName "logFuncs"
        toFuncName "" = ""
        toFuncName (h : t) = toLower h : t
    sequence $
        [sigD flf [t|[String]|]
        ,funD flf [clause []
                        (normalB $ listE $
                            map (litE . stringL . toFuncName . nameBase) lCons'
                        ) []
                    ]
        ]
        ++
        concatMap
        (\con ->
             let c = toFuncName $ nameBase con
                 fl = mkName c
                 flr = mkName $ c ++ "R"
             in [sigD fl [t|ByteString -> IO L.ByteString|]
                ,funD fl [clause [varP $ mkName "msg"]
                             (normalB
                                 [|logG $(conE con) msg >> return L.empty|]
                             ) []
                         ]
                ,sigD flr [t|ByteString -> IO L.ByteString|]
                ,funD flr [clause [varP $ mkName "msg"]
                              (normalB
                                  [|logR $(conE con) msg >> return L.empty|]
                              ) []
                          ]
                ]
        ) lCons

