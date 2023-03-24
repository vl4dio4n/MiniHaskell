
module REPLCommand where

import Lab2
import Parsing
import Control.Applicative (many, some, (<|>))
import Data.Functor (($>))

data REPLCommand
  = Quit
  | Load String
  | Eval String
  deriving(Show)


isQuit :: REPLCommand -> Bool
isQuit Quit = True
isQuit _ = False


isLoad :: REPLCommand -> Bool
isLoad (Load _) = True
isLoad _ = False


fromEval :: REPLCommand -> Maybe String
fromEval (Eval str) = Just str
fromEval _ = Nothing


quitParser :: Parser REPLCommand
quitParser = do
  (symbol ":q" <* endOfInput) <|> (symbol ":quit" <* endOfInput)
  return Quit


loadParser :: Parser REPLCommand
loadParser = do
  str <- (symbol ":load" *> (some space $> ()) *> identifier (satisfy (const True)) (satisfy (const True))) <|> 
    (symbol ":l" *> (some space $> ()) *> identifier (satisfy (const True)) (satisfy (const True))) 
  return (Load str)


evalParser :: Parser REPLCommand
evalParser = do
  chars <- whiteSpace *> many anychar
  return (Eval chars)


replCommand :: Parser REPLCommand
replCommand = quitParser <|> loadParser <|> evalParser 
