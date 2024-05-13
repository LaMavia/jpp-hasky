{-# LANGUAGE FlexibleInstances #-}

module Parser where
import qualified Abs
import           Data.String (IsString (..))
import           Lex         (Token)
import           Par         (myLexer, pProgram)

aux :: ([Token] -> Either _b a) -> String -> a
aux p s = ast
  where
    tokens = myLexer s
    Right ast = p tokens

instance IsString Abs.Program where
  fromString = aux pProgram

