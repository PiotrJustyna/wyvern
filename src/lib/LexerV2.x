{
module LexerV2
  (alexScanTokens,
  Token(TokenAction,
  TokenSoloIdentifier,
  TokenOCB,
  TokenCCB),
  AlexPosn(..),
  tokenPosition) where
}

%wrapper "posn"

$digit        = 0-9
$alpha        = [a-zA-Z]

$idChar       = [$alpha $digit \']
$contentChar  = [$alpha $digit $white \' \, \! \- \. \/ \? \= \< \> \[ \] \+ \( \)]

@id           = $idChar+
@content      = $contentChar+

tokens :-

  $white+                         ;
  @id [$white]+ \"@content\"      { tok (\p s -> TokenAction p s) }
  \"@content\"                    { tok (\p s -> TokenAction p ("# " <> s)) -- # is a placeholder id that will later be replaced by a unique identifier }
  @id                             { tok (\p s -> TokenSoloIdentifier p s) }
  \{                              { tok (\p _ -> TokenOCB p) }
  \}                              { tok (\p _ -> TokenCCB p) }

{
-- Each right-hand side has type :: AlexPosn -> String -> Token

-- Some action helpers:
tok f p s = f p s

data Token
  = TokenAction AlexPosn String
  | TokenSoloIdentifier AlexPosn String
  | TokenOCB AlexPosn
  | TokenCCB AlexPosn
  deriving (Eq, Show)

tokenPosition (TokenAction p _) = p
tokenPosition (TokenSoloIdentifier p _) = p
tokenPosition (TokenOCB p) = p
tokenPosition (TokenCCB p) = p
}
