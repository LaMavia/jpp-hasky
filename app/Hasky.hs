module Main where

import           Control.Monad        (when)
import           Prelude              (Either (..), FilePath, IO, Int, String,
                                       concat, getContents, mapM_, print,
                                       putStrLn, readFile, show, ($), (++), (.),
                                       (>), (>>), (>>=))
import           System.Environment   (getArgs)
import           System.Exit          (exitFailure)

import           Abs                  (Program)
import           Execution.Eval       (evalProgram)
import           Lex                  (Token, mkPosToken)
import           Par                  (myLexer, pProgram)
import           Preprocessor.Desugar (desugarProgram)
import           Preprocessor.Stdlib  (prependStdlib)
import           Print                (printTree)
import           Runtime              (runRT)
import           Skel                 ()

type Err        = Either String
type ParseFun   = [Token] -> Err Program
type Verbosity  = Int

putStrV :: Verbosity -> String -> IO ()
putStrV v s = when (v > 1) $ putStrLn s

runFile :: Verbosity -> ParseFun -> FilePath -> IO ()
runFile v p f = putStrLn f >> readFile f >>= run v p

run :: Verbosity -> ParseFun -> String -> IO ()
run v p s =
  case p ts of
    Left err -> do
      putStrLn "\nParse              Failed...\n"
      putStrV v "Tokens:"
      mapM_ (putStrV v . showPosToken . mkPosToken) ts
      putStrLn err
      exitFailure
    Right tree -> do
      let program = prependStdlib $ desugarProgram tree
      r <- runRT $ evalProgram program
      case r of
        Left err      -> print err
        Right (_, st) -> putStrLn "\n>>>>>>>>>>> STATE <<<<<<<<<<<<" >> print  st
  where
  ts = myLexer s
  showPosToken ((l,c),t) = concat [ show l, ":", show c, "\t", show t ]

showTree :: Int -> Program -> IO ()
showTree v tree = do
  putStrV v $ "\n[Abstract Syntax]\n\n" ++ show tree
  putStrV v $ "\n[Linearized tree]\n\n" ++ printTree tree

main :: IO ()
main = do
  args <- getArgs
  case args of
    []      -> getContents >>= run 2 pProgram
    "-s":fs -> mapM_ (runFile 0 pProgram) fs
    fs      -> mapM_ (runFile 2 pProgram) fs

