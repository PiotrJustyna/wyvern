{
module Parser where
import Lexer
}

%name                                       diagram
%tokentype                                  { Token }
%monad                                      { P } { thenP } { returnP }

%token
  action                                    { TokenAction $$ }
  soloId                                    { TokenSoloIdentifier $$ }
  '{'                                       { TokenOCB }
  '}'                                       { TokenCCB }

%%

block : {- empty -}                                           { [] }

{

happyError = \tks i -> error ("Parse error in line " ++ show (i::Int) ++ ".\n")

data ParseResult a
  = ParseOk a
  | ParseFail String

type P a = Int -> ParseResult a

thenP :: P a -> (a -> P b) -> P b
m `thenP` k = \l ->
  case m l of
    ParseFail s -> ParseFail s
    ParseOk a -> k a l

returnP :: a -> P a
returnP a = \l -> ParseOk a
}
