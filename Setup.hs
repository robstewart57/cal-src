
import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.Setup (BuildFlags)

import BNFC.GetCF
import BNFC.Options
import BNFC.Backend.Base (writeFiles)
import BNFC.Backend.Haskell

main :: IO ()
main = defaultMainWithHooks simpleUserHooks { preBuild = calGenHook }

calGenHook :: Args -> BuildFlags -> IO HookedBuildInfo
calGenHook _ _ = do
  let opts = defaultOptions { lang = "CAL" }
  grammar <- readFile "CAL.cf"
  cf <- parseCF opts TargetHaskell grammar
  writeFiles "." $ makeHaskell opts cf
  return emptyHookedBuildInfo
