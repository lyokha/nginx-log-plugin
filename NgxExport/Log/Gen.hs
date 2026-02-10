{-# LANGUAGE CPP, TemplateHaskell, TupleSections #-}

module NgxExport.Log.Gen where

import           NgxExport.Log.Base

import           Language.Haskell.TH
import           Control.Arrow
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import           Data.List
import           Data.Char

#if MIN_VERSION_template_haskell(2,18,0)
#define FUND funD_doc
#else
#define FUND funD
#endif

do
    TyConI (DataD _ _ _ _ lCs _) <- reify ''LogLevel
    let lCons = uncurry (++) $
            (map (, 'logG) &&& map ((, 'logR) . second (++ "R")))
            [id &&& toFuncName . nameBase $ con | NormalC con [] <- lCs]
        toFuncName = maybe undefined (uncurry (:) . first toLower) . uncons
        flf = mkName "logFuncs"
    sequence $
        [sigD flf [t|[String]|]
        ,funD flf [clause []
                      (normalB $
                          listE $ map (litE . stringL . snd . fst) lCons
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
                         (Just $ let lType | f == 'logR = "request's"
                                           | otherwise = "global"
                                 in "Logs a message with severity '" ++
                                        nameBase con ++ "' to the " ++
                                        lType ++ " Nginx log.\n\n" ++
                                        "This is the core function of the /" ++
                                        fn ++ "/ handler."
                         ) [Just "Log message"]
#endif
                ]
        ) lCons

