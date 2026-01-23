{
module LexerV3
  (lexAll,
  runAlex,
  Token(TokenAction,
  TokenSoloIdentifier,
  TokenOCB,
  TokenCCB),
  AlexPosn(..)) where
}

%wrapper "monad"

$digit        = 0-9
$alpha        = [a-zA-Z]

$idChar       = [$alpha $digit \']
$contentChar  = [$alpha $digit $white \' \, \! \- \. \/ \? \= \< \> \[ \] \+ \( \)]

@id           = $idChar+
@content      = $contentChar+

tokens :-

  $white+                         ;
  @id [$white]+ \"@content\"      { tokenActionWithIdAction }
  \"@content\"                    { tokenActionAction }
  @id                             { tokenSoloIdentifierAction }
  \{                              { tokenOCBAction }
  \}                              { tokenCCBAction }

{
data Token
  = TokenAction AlexPosn String
  | TokenSoloIdentifier AlexPosn String
  | TokenOCB AlexPosn
  | TokenCCB AlexPosn
  | TokenEOF
  deriving (Eq, Show)

tokenOCBAction :: AlexInput -> Int -> Alex Token
tokenOCBAction (position, _previousCharacter, _bytes, _inputString) len = return $ TokenOCB position

tokenCCBAction :: AlexInput -> Int -> Alex Token
tokenCCBAction (position, _previousCharacter, _bytes, _inputString) len = return $ TokenCCB position

tokenActionWithIdAction :: AlexInput -> Int -> Alex Token
tokenActionWithIdAction (position, _previousCharacter, _bytes, inputString) len = return $ TokenAction position (take len inputString)

-- # is a placeholder id that will later be replaced by a unique identifier
tokenActionAction :: AlexInput -> Int -> Alex Token
tokenActionAction (position, _previousCharacter, _bytes, inputString) len = return $ TokenAction position ("# " <> take len inputString)

tokenSoloIdentifierAction :: AlexInput -> Int -> Alex Token
tokenSoloIdentifierAction (position, _previousCharacter, _bytes, inputString) len = return $ TokenSoloIdentifier position (take len inputString)

alexEOF :: Alex Token
alexEOF = return TokenEOF

lexAll :: Alex [Token]
lexAll = go
    where
        go = do
            t <- alexMonadScan
            case t of
                TokenEOF -> return [TokenEOF]
                _ -> (t:) <$> go
}
