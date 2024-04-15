module MarkdownToHtml (ToMarkdown (..), ToHtml (..), Markdown(..), MarkdownElement(..), Html(..), HtmlElement(..), CompileToHtml(..)) where
-- module MarkdownToHtml where

import Html
import Markdown
-- import Parse
-- import Styles

class ToMarkdown a where
  toMarkdown :: a -> Markdown
  toMarkdownElement :: a -> MarkdownElement

instance ToMarkdown MarkdownElement where
  toMarkdown element = Markdown [element]
  toMarkdownElement element = element

instance ToMarkdown a => ToMarkdown [a] where
  toMarkdown = Markdown . map toMarkdownElement
  toMarkdownElement = Many . (map toMarkdownElement)

class ToHtml a where
  toHtml :: a -> Html
  toHtmlElement :: a -> HtmlElement

instance ToHtml HtmlElement where
  toHtml element = Html [element]
  toHtmlElement element = element

instance ToHtml a => ToHtml [a] where
  toHtml = Html . map toHtmlElement
  toHtmlElement = HtmlMany . (map toHtmlElement)

instance ToHtml MarkdownElement where
  toHtmlElement (Header level element) =
    HtmlHeader level $ toHtmlElement element
  toHtmlElement (UnorderedList elements) =
    HtmlUnorderedList $ map toHtmlElement elements
  toHtmlElement (OrderedList elements) =
    HtmlOrderedList $ map toHtmlElement elements
  toHtmlElement (Text text) = HtmlText text
  toHtmlElement (CodeBlock code) = HtmlCodeBlock code
  toHtmlElement (BlockQuote element) = HtmlBlockQuote $ toHtmlElement element
  toHtmlElement HorizontalRule = HtmlHorizontalRule
  toHtmlElement (Link text url) = HtmlLink (toHtmlElement text) url
  toHtmlElement (Image url altText) = HtmlImage url altText
  toHtmlElement (Bold element) = HtmlBold $ toHtmlElement element
  toHtmlElement (Italic element) = HtmlItalic $ toHtmlElement element
  toHtmlElement (Strikethrough element) =
    HtmlStrikethrough $ toHtmlElement element
  toHtmlElement (InlineCode code) = HtmlInlineCode code
  toHtmlElement (Many elements) = HtmlMany $ map toHtmlElement elements
    -- toHtmlElement (Table headers rows) = HtmlTable (map toHtmlElement headers) (map (map toHtmlElement) rows)
  toHtmlElement (Table headers rows) =
    HtmlTable (map toHtmlElement headers) (map (map toHtmlElement) rows)
  toHtmlElement NewLine = HtmlNewLine
  toHtml md = Html [toHtmlElement md]

instance ToHtml Markdown where
  toHtml (Markdown md) = Html $ map toHtmlElement md
  toHtmlElement (Markdown md) = HtmlMany $ map toHtmlElement md

class CompileToHtml a where
    -- The second argument is the list of style sheet contents
  compileToHtml :: a -> [String] -> String

instance CompileToHtml MarkdownElement where
  compileToHtml markdown styleSheets = do
    let contents = toHtmlElement markdown
    let styleSheetTags =
          concatMap
            (\styleSheet -> "<style>" ++ styleSheet ++ "</style>")
            styleSheets
    "<!DOCTYPE html><html><head>"
      ++ styleSheetTags
      ++ "</head><body class=\"markdown-body\">"
      ++ show (toHtml contents)
      ++ "</body></html>"
