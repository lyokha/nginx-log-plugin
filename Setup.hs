import Distribution.Simple
import Distribution.Simple.Setup

import Data.List

userHooks :: UserHooks
userHooks =
    simpleUserHooks { haddockHook = \desc lbi hooks flags -> do
                        let (oghc, ocstub) = ("ghc", "-DNGX_CSTUB")
                            hadflag = haddockProgramArgs flags
                            (h, t) = break ((oghc ==) . fst) hadflag
                            hadflag' =
                                case uncons t of
                                    Nothing ->
                                        (oghc, [ocstub]) : hadflag
                                    Just ((_, args), t') ->
                                        h ++ (oghc, ocstub : args) : t'
                            flags' = flags { haddockProgramArgs = hadflag' }
                        haddockHook simpleUserHooks desc lbi hooks flags'
                    }

main :: IO ()
main = defaultMainWithHooks userHooks

