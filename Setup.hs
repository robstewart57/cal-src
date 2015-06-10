
import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.Setup (BuildFlags)

import BNFC.GetCF (parseCF)
import BNFC.Options (Target(TargetHaskell), defaultOptions, lang)
import BNFC.Backend.Base ( writeFiles)
import BNFC.Backend.Haskell (makeHaskell)

main :: IO ()
main = defaultMainWithHooks simpleUserHooks { preBuild = calGenHook }

calGenHook :: Args -> BuildFlags -> IO HookedBuildInfo
calGenHook _ _ = do
  let opts = defaultOptions { lang = "CAL" }
  grammar <- readFile "CAL.cf"
  cf <- parseCF opts TargetHaskell grammar
  writeFiles "." $ makeHaskell opts cf
  return emptyHookedBuildInfo
