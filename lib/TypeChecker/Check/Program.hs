module TypeChecker.Check.Program where
import           Abs                      (Program, Program' (Program))
import           Common                   (placeOfProgram, uCatch, withEnv)
import           Preprocessor.Stdlib      (runTypePrelude)
import           TypeChecker.Check.TopDef (typeCheckTopDef)
import           TypeChecker.TC           (TCChecker, mapTCEnv)


typeCheckProgram :: TCChecker Program ()
typeCheckProgram p@(Program pos topdefs) = uCatch (placeOfProgram p) $ do
  env' <- runTypePrelude
  (_, tds) <- withEnv env' $ mapTCEnv typeCheckTopDef topdefs
  return ((), Program pos tds)

