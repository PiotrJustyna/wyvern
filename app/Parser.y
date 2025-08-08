{
module Parser where
import Data.Char
import Lexer
import ID
import SkewerBlock
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

skewers :   headline                                              { [$1] }
            | skewers headline                                    { $2 : $1 }

headline :  action '{' skewer '}'                                 { $3 <> [toHeadline $1] }
            | action '{' skewer soloId '}'                        { toAddress $4 : $3 <> [toHeadline $1] }

skewer :    {- empty -}                                           { [] }
            | block                                               { [$1] }
            | skewer block                                        { $2 : $1 }

block :     action                                                { toAction $1 }
            | action '{' skewer '}' '{' skewer '}'                { toFork $1 $3 Nothing $6 Nothing }
            | action '{' skewer '}' '{' skewer soloId '}'         { toFork $1 $3 Nothing $6 (Just (ID $7)) }
            | action '{' skewer soloId '}' '{' skewer '}'         { toFork $1 $3 (Just (ID $4)) $7 Nothing }
            | action '{' skewer soloId '}' '{' skewer soloId '}'  { toFork $1 $3 (Just (ID $4)) $7 (Just (ID $8)) }

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
