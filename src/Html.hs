module Html (Html (..), HtmlElement (..)) where

data HtmlElement
  = HtmlHeader Int HtmlElement
  | HtmlUnorderedList [HtmlElement]
  | HtmlOrderedList [HtmlElement]
  | HtmlText String
  | HtmlCodeBlock String
  | HtmlBlockQuote HtmlElement
  | HtmlHorizontalRule
  | HtmlLink HtmlElement String
  | HtmlImage String String
  | HtmlBold HtmlElement
  | HtmlItalic HtmlElement
  | HtmlStrikethrough HtmlElement
  | HtmlInlineCode String
  | HtmlTable [HtmlElement] [[HtmlElement]]
  | HtmlNewLine
  | HtmlMany [HtmlElement]

instance Show HtmlElement where
  show (HtmlHeader level element) =
    "<h" ++ show level ++ ">" ++ show element ++ "</h" ++ show level ++ ">"
  show (HtmlUnorderedList elements) =
    "<ul>"
      ++ concatMap (\element -> "<li>" ++ show element ++ "</li>") elements
      ++ "</ul>"
  show (HtmlOrderedList elements) =
    "<ol>"
      ++ concatMap (\element -> "<li>" ++ show element ++ "</li>") elements
      ++ "</ol>"
  show (HtmlText text) = text
  show (HtmlCodeBlock code) = "<pre><code>" ++ code ++ "</code></pre>"
  show (HtmlBlockQuote element) =
    "<blockquote>" ++ show element ++ "</blockquote>"
  show HtmlHorizontalRule = "<hr/>"
  show (HtmlLink content url) =
    "<a href=\"" ++ url ++ "\">" ++ show content ++ "</a>"
  show (HtmlImage url altText) =
    "<img src=\"" ++ url ++ "\" alt=\"" ++ altText ++ "\">"
  show (HtmlBold element) = "<strong>" ++ show element ++ "</strong>"
  show (HtmlItalic element) = "<em>" ++ show element ++ "</em>"
  show (HtmlStrikethrough element) = "<del>" ++ show element ++ "</del>"
  show (HtmlInlineCode code) = "<code>" ++ code ++ "</code>"
  show (HtmlTable headers rows) =
    "<table><tr>"
      ++ concatMap (\header -> "<th>" ++ show header ++ "</th>") headers
      ++ "</tr>"
      ++ concatMap
           (\row ->
              "<tr>"
                ++ concatMap (\cell -> "<td>" ++ show cell ++ "</td>") row
                ++ "</tr>")
           rows
      ++ "</table>"
  show HtmlNewLine = "<br/>"
  show (HtmlMany elements) = concatMap show elements

data Html =
  Html [HtmlElement]

instance Show Html where
  show (Html html) = concatMap show html
