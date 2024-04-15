{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Parse (parseMarkdown, Error(..)) where

import Markdown
import Text.Parsec

-- data ParseError = ParseError String
--     deriving Show
data Error =
  ParseError String
  deriving (Show)

class ParseMarkdown a where
  parseMarkdown :: a -> Either Error MarkdownElement

instance ParseMarkdown String where
  parseMarkdown md =
    case parse parseMarkdownParsec "" md of
      Left err -> Left $ ParseError $ show err
      Right elements -> Right elements

parseMarkdownParsec :: Parsec String () MarkdownElement
parseMarkdownParsec = do
    -- elements <- mapM parseMarkdown md
    -- return $ Many elements
  elements <- parseMany
  eof
  return elements

-- instance ParseMarkdown [MarkdownElement] where
--     parseMarkdown elements = Right $ Markdown elements
parseMany :: Parsec String () MarkdownElement
parseMany = do
    -- elements <- many1 parseMarkdown
  elements <- many parseElement
    -- Flatten nested Many elements
    -- let elements' = concatMap (\element -> case element of
    --         Many elements -> elements
    --         _ -> [element]) elements
    -- Join adjacent Text elements into the same block
    -- let elements' = foldr (\element acc -> case (element, acc) of
    --         (Text text, Text accText:rest) -> Text (text ++ accText):rest
    --         _ -> element:acc) [Text ""] elements'
  return $ Many elements

parseStrictElement :: Parsec String () MarkdownElement
parseStrictElement = do
  element <-
    parseHeader
      <|> parseUnorderedList
      <|> parseOrderedList
      <|> parseInlineCode
      <|> parseDoubleInlineCode
      <|> parseCodeBlock
      <|> parseBlockQuote
      <|> parseHorizontalRule
      <|> parseLink
      <|> parseImage
      <|> parseBold
      <|> parseItalic
      <|> parseStrikethrough
      <|> parseTable
  return element

parseElement :: Parsec String () MarkdownElement
parseElement = do
  element <-
    parseHeader
      <|> parseUnorderedList
      <|> parseOrderedList
      <|> parseInlineCode
      <|> parseDoubleInlineCode
      <|> parseCodeBlock
      <|> parseBlockQuote
      <|> parseHorizontalRule
      <|> parseLink
      <|> parseImage
      <|> parseBold
      <|> parseItalic
      <|> parseStrikethrough
      <|> parseTable
      <|> parseNewLine
      <|> parseText
  return element

parseHeader :: Parsec String () MarkdownElement
parseHeader = do
  level <- length <$> many1 (char '#')
  _ <- space
  content <- manyTill anyChar newline
  return $ Header level (Text content)

parseUnorderedList :: Parsec String () MarkdownElement
parseUnorderedList = do
  elements <- many1 parseUnorderedListElement
  return $ UnorderedList elements
parseUnorderedListElement :: Parsec String () MarkdownElement
parseUnorderedListElement = do
  _ <- char '-'
  _ <- space
  content <- manyTill anyChar newline
  return $ case parseMarkdown content of
    Left _ -> Text content
    Right elements -> elements

parseOrderedList :: Parsec String () MarkdownElement
parseOrderedList = do
  elements <- many1 parseOrderedListElement
  return $ OrderedList elements
parseOrderedListElement :: Parsec String () MarkdownElement
parseOrderedListElement = do
  _ <- many1 digit
  _ <- char '.'
  _ <- space
  content <- manyTill anyChar newline
  return $ case parseMarkdown content of
    Left _ -> Text content
    Right elements -> elements

parseBlockQuote :: Parsec String () MarkdownElement
parseBlockQuote = do
  _ <- string "> "
  content <- manyTill anyChar newline
  return $ BlockQuote (Text content)

parseHorizontalRule :: Parsec String () MarkdownElement
parseHorizontalRule = do
  _ <- string "---"
  _ <- newline
  return HorizontalRule

parseLink :: Parsec String () MarkdownElement
parseLink = do
  _ <- char '['
  content <- manyTill (try parseStrictElement <|> parseText) (char ']')
  _ <- char '('
  url <- manyTill anyChar (char ')')
  return $ Link (Many content) url

parseImage :: Parsec String () MarkdownElement
parseImage = do
  _ <- string "!["
  altText <- manyTill anyChar (char ']')
  _ <- char '('
  url <- manyTill anyChar (char ')')
  return $ Image url altText

parseBold :: Parsec String () MarkdownElement
parseBold = do
  _ <- string "**"
  content <- manyTill anyChar (string "**")
  return
    $ Bold
        (case parseMarkdown content of
           Left _ -> Text content
           Right elements -> elements)

parseItalic :: Parsec String () MarkdownElement
parseItalic = do
  _ <- char '_'
  content <- manyTill anyChar (string "_")
  return
    $ Italic
        (case parseMarkdown content of
           Left _ -> Text content
           Right elements -> elements)

parseStrikethrough :: Parsec String () MarkdownElement
parseStrikethrough = do
  _ <- string "~~"
  content <- manyTill anyChar (string "~~")
  return $ Strikethrough (Text content)

parseCodeBlock :: Parsec String () MarkdownElement
parseCodeBlock = do
  -- Lookahead for "```" and fail if it's found
  _ <- string "```"
  -- Parse everything until the newline and discard the
  -- newline character
  _ <- manyTill anyChar newline
  content <- manyTill (parseEscaped <|> anyChar) (try (string "```"))
  return $ CodeBlock content

parseDoubleInlineCode :: Parsec String () MarkdownElement
parseDoubleInlineCode = do
  -- string "`` "
  -- Match `` followed by a space or bail
  notFollowedBy (string "```")
  _ <- string "``"
  _ <- space
  -- Followed by any character except an escaped backtick and a newline
  -- For matching an escaped backtick, we need to use try

  -- content <- manyTill (sequence [parseEscaped <|> anyChar]) (try (string "``") <|> sequence [newline] <|> (eof >> return ""))
  content <- manyTill (parseEscaped <|> anyChar) (try (string " ``" >> notFollowedBy (string "`")))
  return $ InlineCode content

parseInlineCode :: Parsec String () MarkdownElement
parseInlineCode = do
  notFollowedBy (string "``")
  _ <- char '`'
  -- Followed by any character except an escaped backtick and a newline
  -- For matching an escaped backtick, we need to use try

  content <- manyTill (parseEscaped <|> anyChar) (try (char '`'))
  return $ InlineCode content

parseTable :: Parsec String () MarkdownElement
parseTable = do
  headers <- parseTableRow
  parseAlignmentRow
  rows <- many1 parseTableRow
  return $ Table headers rows

parseAlignmentRow :: Parsec String () ()
parseAlignmentRow = do
  _ <- char '|'
    -- Parse many "-" characters followed by "|"
  _ <- manyTill (many1 (char '-') >> char '|') newline
  return ()

parseTableRow :: Parsec String () [MarkdownElement]
parseTableRow = do
  _ <- char '|'
  elements <- manyTill (try parseTableCell) (try newline)
  return elements

parseTableCell :: Parsec String () MarkdownElement
parseTableCell = do
  -- Delimited by optional space and "|"
  content <- manyTill parseElement (optional space >> char '|')
  return $ Many content
  -- content <- manyTill anyChar (char '|')
  -- return
  --   $ case parseMarkdown content of
  --       Left err -> Text content
  --       Right elements -> elements

parseEscaped :: Parsec String () Char
parseEscaped = do
  _ <- char '\\'
  escapedChar <- anyChar
  return escapedChar

parseText :: Parsec String () MarkdownElement
parseText = do
  notFollowedBy
    ((try parseStrictElement >> return ' ')
       <|> try newline
       <|> try (char '_')
       <|> try (char '*')
       <|> try (char '#')
       <|> try (char '`')
       <|> try (char '>')
       <|> try (char '~'))
  firstChar <- try parseEscaped <|> anyChar
  content <-
    manyTill
      (try parseEscaped <|> anyChar)
      (lookAhead
         ((try parseStrictElement >> return ' ')
            <|> (try eof >> return ' ')
            <|> try newline
            <|> try (char '_')
            <|> try (char '*')
            <|> try (char '|')
            <|> try (char '#')
            <|> try (char '`')
            <|> try (char '>')
            <|> try (char '~')))
  return $ Text ([firstChar] ++ content)

-- parseSpace :: Parsec String () MarkdownElement
-- parseSpace = do
--   _ <- space
--   return $ Text " "

parseNewLine :: Parsec String () MarkdownElement
parseNewLine = do
  _ <- newline
  return NewLine
