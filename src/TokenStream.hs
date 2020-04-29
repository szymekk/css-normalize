{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module TokenStream
  (
  )
where

import Data.CSS.Syntax.Tokens as CSS
import qualified Data.List as DL
import Data.List.NonEmpty
import qualified Data.List.NonEmpty as NE
import Data.Proxy
import qualified Data.Text as T
import Text.Megaparsec

deriving instance Ord CSS.NumericValue

deriving instance Ord CSS.HashFlag

deriving instance Ord CSS.Token

instance Stream [CSS.Token] where
  type Token [CSS.Token] = CSS.Token
  type Tokens [CSS.Token] = [CSS.Token]
  tokenToChunk Proxy = pure
  tokensToChunk Proxy = id
  chunkToTokens Proxy = id
  chunkLength Proxy = DL.length
  chunkEmpty Proxy = null
  take1_ [] = Nothing
  take1_ (t : ts) = Just (t, ts)
  takeN_ n s
    | n <= 0 = Just ([], s)
    | null s = Nothing
    | otherwise = Just (DL.splitAt n s)
  takeWhile_ = DL.span

  -- signature for Megaparsec 8.0.0
  -- reachOffset :: Int -> PosState s -> (String, PosState s)
  reachOffset ::
    Int ->
    PosState [CSS.Token] ->
    (SourcePos, String, PosState [CSS.Token])
  reachOffset o PosState {..} =
    ( spos,
      case pstateLinePrefix ++ (T.unpack . serialize) pstateInput of
        "" -> "<empty line>"
        xs -> xs,
      PosState
        { pstateInput = post,
          pstateOffset = max pstateOffset o,
          pstateSourcePos = spos,
          pstateTabWidth = pstateTabWidth,
          pstateLinePrefix = pstateLinePrefix
        }
    )
    where
      (pre, post) = DL.splitAt (o - pstateOffset) pstateInput
      spos =
        case pre of
          [] -> pstateSourcePos
          _ ->
            let SourcePos n l c = pstateSourcePos
                tokensLength = mkPos . T.length . serialize
             in SourcePos n l (c <> tokensLength pre)

  showTokens :: Proxy s -> NonEmpty CSS.Token -> String
  showTokens Proxy = tokensPretty

tokensPretty :: NonEmpty CSS.Token -> String
tokensPretty (x :| []) = tokenPretty x
tokensPretty xs = "\"" <> concatMap f (NE.toList xs) <> "\""
  where
    f tok =
      case tokenPretty' tok of
        Nothing -> "{unknown}"
        Just pretty -> "<" <> pretty <> ">"

tokenPretty :: CSS.Token -> String
tokenPretty Whitespace = "space"
tokenPretty tok = "'" <> (T.unpack . serialize) [tok] <> "'"

tokenPretty' :: CSS.Token -> Maybe String
tokenPretty' tok = (Just . T.unpack . serialize) [tok]
