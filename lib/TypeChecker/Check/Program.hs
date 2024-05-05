module TypeChecker.Check.Program where
import           Abs                      (Program, Program' (Program))
import           Common                   (placeOfProgram, uCatch)
import           TypeChecker.Check.TopDef (typeCheckTopDef)
import           TypeChecker.TC           (TCChecker, mapTCEnv)


typeCheckProgram :: TCChecker Program ()
typeCheckProgram p@(Program pos topdefs) = uCatch (placeOfProgram p) $ do
  (_, tds) <- mapTCEnv typeCheckTopDef topdefs
  return ((), Program pos tds)

