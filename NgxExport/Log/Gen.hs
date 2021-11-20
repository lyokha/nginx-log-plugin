{-# LANGUAGE TemplateHaskell, TupleSections, LambdaCase #-}

module NgxExport.Log.Gen where

import           NgxExport.Log.Base

import           Language.Haskell.TH
import           Control.Arrow
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import           Data.Char

do
    TyConI (DataD _ _ _ _ lCs _) <- reify ''LogLevel
    let lCons = map (\case
                         NormalC con [] -> con
                         - -> undefined
                    ) lCs
        lCons' = map ((, 'logG) . (id &&& toFuncName . nameBase)) lCons ++
            map ((, 'logR) . (id &&& toFuncName . (++ "R") . nameBase)) lCons
        toFuncName "" = ""
        toFuncName (h : t) = toLower h : t
        flf = mkName "logFuncs"
    sequence $
        [sigD flf [t|[String]|]
        ,funD flf [clause []
                      (normalB $
                          listE $ map (litE . stringL . snd . fst) lCons'
                      ) []
                  ]
        ]
        ++
        concatMap
        (\((con, fn), f) ->
             let fl = mkName fn
             in [sigD fl [t|ByteString -> IO L.ByteString|]
                ,funD fl [clause [varP $ mkName "msg"]
                             (normalB
                                 [|$(varE f) $(conE con) msg >> return L.empty|]
                             ) []
                         ]
                ]
        ) lCons'

