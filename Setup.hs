
import Control.Monad (void)
import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.Setup (BuildFlags)
import System.Process

main = do
  void $ createProcess (proc "bnfc" ["-m","--haskell","CAL.cf"])
  defaultMain
  -- defaultMainWithHooks simpleUserHooks { preBuild = calGenHook }

-- calGenHook :: Args -> BuildFlags -> IO HookedBuildInfo
-- calGenHook _ _ = do
  -- currently just uses the `bnfc` executable, a better
  -- approach would be the use BNFC as a library.
  -- See: https://groups.google.com/forum/#!topic/bnfc-dev/9_37G02ErDw
  --
  -- For this reason, this is currently not in a fit state for hackage.
  -- void $ createProcess (proc "bnfc" ["-m","--haskell","CAL.cf"])
  -- void $ createProcess (proc "happy" ["-gca","ParCAL.y"])
  -- void $ createProcess (proc "alex" ["-g","LexCAL.x"])
  -- return emptyHookedBuildInfo
