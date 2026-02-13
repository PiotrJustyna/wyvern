{
module Parser where
import Data.Char
import Data.String.Utils (lstrip)
import ID
import Lexer
import Blocks
}

%name                                                     diagram
%tokentype                                                { Token }
%monad                                                    { P } { thenP } { returnP }
%error                                                    { happyError }

%token
  action                                                  { TokenAction _ $$ }
  soloId                                                  { TokenSoloIdentifier _ $$ }
  '{'                                                     { TokenOCB _ }
  '}'                                                     { TokenCCB _ }

%%

skewers :   headline                                      { [$1] }
            | skewers headline                            { $2 : $1 }
            | skewer                                      { [$1] }

skewer :    {- empty -}                                   { [] }
            | block                                       { [$1] }
            | skewer block                                { $2 : $1 }

headline :  action '{' skewer '}'                         { $3 <> [toHeadline $1] }
            | action '{' skewer soloId '}'                { toAddress $4 : $3 <> [toHeadline $1] }

block :     action                                        { toAction $1 }
            | action '{' skewer '}' '{' skewer '}'        { toFork $1 $3 $6 Nothing }
            | action '{' skewer '}' '{' skewer soloId '}' { toFork $1 $3 $6 (Just (ID $7)) }
            | action '{' skewer '}' '{' skewer soloId '}' { toFork $1 $3 $6 (Just (ID $7)) }
            | action '{' skewer '}' '{' skewer '}'        { toFork $1 $3 $6 Nothing }

{
toId :: String -> Maybe ID
toId t =
  case head $ words t of
    "#" -> Nothing
    x -> Just (ID x)

toContent :: String -> String
toContent t =
  let id = head $ words t
      idFreeContent = lstrip (drop (length id) t)
      idFreeContent' = drop 1 idFreeContent
  in  take (length idFreeContent' - 1) idFreeContent'

toAction :: String -> Block
toAction t =
    Action Nothing (show t)
    -- Action (toId t) (toContent t)

toFork :: String -> [Block] -> [Block] -> Maybe ID -> Block
toFork t l r rId = Fork (toId t) (toContent t) l r rId

toHeadline :: String -> Block
toHeadline t = Headline (toId t) (toContent t)

toAddress :: String -> Block
toAddress t = Address (toId t) (head $ words t)

happyError :: [Token] -> a
happyError [] = error ("Parse error - no input")
happyError (t:_) = error ("Parse error - provided input did not match any grammar production rules: " <> show t <> ".")

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
