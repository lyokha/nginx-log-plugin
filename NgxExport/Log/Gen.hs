{-# LANGUAGE TemplateHaskell, TupleSections #-}

module NgxExport.Log.Gen where

import           NgxExport.Log.Base

import           Language.Haskell.TH
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import           Control.Arrow
import           Data.Char

do
    TyConI (DataD _ _ _ _ lCs _) <- reify ''LogLevel
    let lCons = map (\(NormalC con []) -> con) lCs
        lCons' = map ((, 'logG) . (id &&& id)) lCons ++
            map ((, 'logR) . (id &&& mkName . (++ "R") . nameBase)) lCons
        flf = mkName "logFuncs"
        toFuncName "" = ""
        toFuncName (h : t) = toLower h : t
    sequence $
        [sigD flf [t|[String]|]
        ,funD flf [clause []
                        (normalB $ listE $
                            map (litE . stringL . toFuncName . nameBase .
                                    snd . fst
                                ) lCons'
                        ) []
                    ]
        ]
        ++
        concatMap
        (\((con, con'), f) ->
             let fl = mkName $ toFuncName $ nameBase con'
             in [sigD fl [t|ByteString -> IO L.ByteString|]
                ,funD fl [clause [varP $ mkName "msg"]
                             (normalB
                                 [|$(varE f) $(conE con) msg >> return L.empty|]
                             ) []
                         ]
                ]
        ) lCons'

