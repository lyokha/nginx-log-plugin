{-# LANGUAGE CPP, TemplateHaskell, TupleSections, LambdaCase #-}

module NgxExport.Log.Gen where

import           NgxExport.Log.Base

import           Language.Haskell.TH
import           Control.Arrow
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import           Data.Char

#if MIN_VERSION_template_haskell(2,18,0)
#define FUND funD_doc
#else
#define FUND funD
#endif

do
    TyConI (DataD _ _ _ _ lCs _) <- reify ''LogLevel
    let lCons = map (\case
                         NormalC con [] -> con
                         _ -> undefined
                    ) lCs
        flr = mkName "logR"
        lCons' = map ((, 'logG) . (id &&& toFuncName . nameBase)) lCons ++
            map ((, flr) . (id &&& toFuncName . (++ "R") . nameBase)) lCons
        toFuncName (h : t) = toLower h : t
        toFuncName _ = undefined
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
                ,FUND fl [clause [varP $ mkName "msg"]
                             (normalB
                                 [|$(varE f) $(conE con) msg >> return L.empty|]
                             ) []
                         ]
#if MIN_VERSION_template_haskell(2,18,0)
                         (Just $ "Logs a message with severity '" ++
                             nameBase con ++ "' to the " ++
                                 (if f == flr
                                      then "request's "
                                      else "global "
                                 ) ++ "Nginx log.\n\n" ++
                                 "This is the core function of the /" ++ fn ++
                                 "/ handler."
                         ) [Just "Log message"]
#endif
                ]
        ) lCons'

