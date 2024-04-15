module Markdown
  ( MarkdownElement(..)
  , Markdown(..)
  ) where

data MarkdownElement
  = Header Int MarkdownElement
  | UnorderedList [MarkdownElement]
  | OrderedList [MarkdownElement]
  | Text String
  | CodeBlock String
  | BlockQuote MarkdownElement
  | HorizontalRule
  | Link MarkdownElement String
  | Image String String
  | Bold MarkdownElement
  | Italic MarkdownElement
  | Strikethrough MarkdownElement
  | InlineCode String
  | Table [MarkdownElement] [[MarkdownElement]]
  | Many [MarkdownElement]
  | NewLine

-- Implement show for MarkdownElement
instance Show MarkdownElement where
  show (Header level element) = replicate level '#' ++ " " ++ show element
  show (UnorderedList elements) =
    unlines $ map (\element -> "- " ++ show element) elements
  show (OrderedList elements) =
    unlines
      $ zipWith
          (\index element -> show (index :: Int) ++ ". " ++ show element)
          [1 ..]
          elements
  show (Text text) = text
  show (CodeBlock code) = "```" ++ code ++ "```"
  show (BlockQuote element) = do
    let elementString = show element
    unlines $ map (\line -> "> " ++ line) $ lines elementString
  show HorizontalRule = "---"
  show (Link text url) = "[" ++ show text ++ "](" ++ url ++ ")"
  show (Image url altText) = "![" ++ altText ++ "](" ++ url ++ ")"
  show (Bold elements) = "**" ++ show elements ++ "**"
  show (Italic elements) = "_" ++ show elements ++ "_"
  show (Strikethrough elements) = "~~" ++ show elements ++ "~~"
  show (InlineCode code) = "`" ++ code ++ "`"
  show (Table headers rows) = do
    let headerRow =
          "| " ++ unwords (map (\header -> show header ++ " |") headers)
    let separatorRow = "| " ++ unwords (map (\_ -> "--- |") headers)
    let rowsString =
          unlines
            $ map
                (\row ->
                   "| " ++ unwords (map (\element -> show element ++ " |") row))
                rows
    headerRow ++ "\n" ++ separatorRow ++ "\n" ++ rowsString
  show (Many elements) = do
        -- Join adjacent Text elements
    let elements' =
          foldr
            (\element acc ->
               case (element, acc) of
                 (Text text, Text accText:rest) -> Text (text ++ accText) : rest
                 _ -> element : acc)
            [Text ""]
            elements
    concatMap show elements' -- Join all elements on the same line
  show NewLine = "\n"

data Markdown =
  Markdown [MarkdownElement]

instance Show Markdown where
  show (Markdown md) = unlines $ map show md
