{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Parse where

import Data.Either
import Html
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
      <|> parseCodeBlock
      <|> parseBlockQuote
      <|> parseHorizontalRule
      <|> parseLink
      <|> parseImage
      <|> parseBold
      <|> parseItalic
      <|> parseStrikethrough
      <|> parseInlineCode
      <|> parseTable
  return element

parseElement :: Parsec String () MarkdownElement
parseElement = do
  element <-
    parseHeader
      <|> parseUnorderedList
      <|> parseOrderedList
      <|> parseCodeBlock
      <|> parseBlockQuote
      <|> parseHorizontalRule
      <|> parseLink
      <|> parseImage
      <|> parseBold
      <|> parseItalic
      <|> parseStrikethrough
      <|> parseInlineCode
      <|> parseTable
      <|> parseNewLine
      <|> parseText
  return element

parseHeader :: Parsec String () MarkdownElement
parseHeader = do
  level <- length <$> many1 (char '#')
  space
  content <- manyTill anyChar newline
  return $ Header level (Text content)

parseUnorderedList :: Parsec String () MarkdownElement
parseUnorderedList = do
  char '-'
  space
  content <- manyTill anyChar newline
  return $ UnorderedList [Text content]

parseOrderedList :: Parsec String () MarkdownElement
parseOrderedList = do
  index <- many1 digit
  char '.'
  space
  content <- manyTill anyChar newline
  return $ OrderedList [Text content]

parseCodeBlock :: Parsec String () MarkdownElement
parseCodeBlock = do
  string "```"
  newline
  content <- manyTill anyChar (try (string "```" >> newline))
  return $ CodeBlock content

parseBlockQuote :: Parsec String () MarkdownElement
parseBlockQuote = do
  string "> "
  content <- manyTill anyChar newline
  return $ BlockQuote (Text content)

parseHorizontalRule :: Parsec String () MarkdownElement
parseHorizontalRule = do
  string "---"
  newline
  return HorizontalRule

parseLink :: Parsec String () MarkdownElement
parseLink = do
  char '['
    -- content <- manyTill anyChar (char ']')
  content <- manyTill (try parseStrictElement <|> parseText) (char ']')
  char '('
  url <- manyTill anyChar (char ')')
  return $ Link (Many content) url
    -- return $ Link (case parseMarkdown content of
    --     Left err -> Text content
    --     Right elements -> elements) url

parseImage :: Parsec String () MarkdownElement
parseImage = do
  string "!["
  altText <- manyTill anyChar (char ']')
  char '('
  url <- manyTill anyChar (char ')')
  return $ Image url altText

parseBold :: Parsec String () MarkdownElement
parseBold = do
  string "**"
    -- content <- manyTill anyChar (string "**")
  content <- manyTill anyChar (string "**")
    -- Now run parseMany on content
    -- innerContent <- parse parseMany "" content
    -- innerContent <- parse parseMany "" content
  return
    $ Bold
        (case parseMarkdown content of
           Left err -> Text content
           Right elements -> elements)

parseItalic :: Parsec String () MarkdownElement
parseItalic = do
  char '_'
  content <- manyTill anyChar (string "_")
    -- Now run parseMany on content
    -- innerContent <- parse parseMany "" content
    -- return $ Italic (parseMarkdown content)
  return
    $ Italic
        (case parseMarkdown content of
           Left err -> Text content
           Right elements -> elements)

parseStrikethrough :: Parsec String () MarkdownElement
parseStrikethrough = do
  string "~~"
  content <- manyTill anyChar (string "~~")
  return $ Strikethrough (Text content)

parseInlineCode :: Parsec String () MarkdownElement
parseInlineCode = do
  char '`'
  content <- manyTill anyChar (char '`')
  return $ InlineCode content

parseTable :: Parsec String () MarkdownElement
parseTable = do
    -- headers <- manyTill (try . parseMarkdown) newline
    -- rows <- manyTill (try . parseMarkdown) newline
    -- Parse | **Header 1** | Header 2 |
  headers <- parseTableRow
    -- Now parse ---|---|---
    -- manyTill (try (char '|') >> char '-') newline
    -- Parse many "-" characters
  parseAlignmentRow
    -- Parse | Row 1, Column 1 | Row 1, Column 2 |
    -- Use parseTableRow
  rows <- many1 parseTableRow
  return $ Table headers rows

parseAlignmentRow :: Parsec String () ()
parseAlignmentRow = do
  char '|'
    -- Parse many "-" characters followed by "|"
  manyTill (many1 (char '-') >> char '|') newline
  return ()

parseTableRow :: Parsec String () [MarkdownElement]
parseTableRow = do
  char '|'
  elements <- manyTill (try parseTableCell) (try newline)
  return elements

parseTableCell :: Parsec String () MarkdownElement
parseTableCell = do
  content <- manyTill anyChar (char '|')
  return
    $ case parseMarkdown content of
        Left err -> Text content
        Right elements -> elements

parseEscaped :: Parsec String () Char
parseEscaped = do
  char '\\'
  escapedChar <- anyChar
  return escapedChar

parseText :: Parsec String () MarkdownElement
parseText = do
    -- content <- manyTill anyChar (lookAhead (try space <|> try newline <|> try (char '_') <|> try (char '*') <|> try (char '#') <|> try (char '`') <|> try (char '>') <|> try (char '~')))
    -- Parse until another element is found that isn't just text (and no *)
    -- BUT allow escapes
    -- notFollowedBy (try (string "**") <|> try (string "__") <|> try (string "~~") <|> try (string "```") <|> try (string "> ") <|> try (string "---") <|> try (string "[") <|> try (string "![") <|> try (string "`") <|> try (string "|") <|> try (string "# ") <|> try (string "- ") <|> try (string "1. ") <|> try (string "```") <|> try (string "---") <|> try (string "|") <|> try (string "![") <|> try (string "*") <|> try (string "_") <|> try (string "\\") <|> try newline)
    -- content <- manyTill (try parseEscaped <|> anyChar) (lookAhead ((try eof >> return ' ') <|> (try parseStrictElement >> return ' ') <|> try newline <|> try (char '_') <|> try (char '*') <|> try (char '#') <|> try (char '`') <|> try (char '>') <|> try (char '~')))
    -- Bail out if we see a newline or a special character
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
            <|> try (char '#')
            <|> try (char '`')
            <|> try (char '>')
            <|> try (char '~')))
  return $ Text ([firstChar] ++ content)

parseSpace :: Parsec String () MarkdownElement
parseSpace = do
  space
  return $ Text " "

parseNewLine :: Parsec String () MarkdownElement
parseNewLine = do
  newline
  return NewLine
