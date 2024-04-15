module Main
  ( main
  ) where

import MarkdownToHtml
import Options.Applicative
import Parse
import Styles

import System.Directory
import System.Exit

-- Import CMDArgs
data Args = Args
  { inputFile :: String
  , outputFile :: Maybe String
  , styleSheets :: [String]
  , verbose :: Bool
  , darkMode :: Bool
  } deriving (Show, Eq)

argsParser :: Parser Args
argsParser =
  Args
    <$> argument str (metavar "INPUT" <> help "Input file name")
    <*> optional
          (strOption
             (long "output"
                <> short 'o'
                <> metavar "OUTPUT"
                <> help "Output file name (optional)"))
    <*> many
          (strOption
             (long "stylesheet"
                <> short 's'
                <> metavar "STYLESHEET"
                <> help "Additional stylesheets"))
    <*> switch (long "verbose" <> short 'v' <> help "Enable verbose mode")
    <*> switch (long "dark" <> short 'd' <> help "Enable dark mode")

verbosePrint :: Show a => Args -> a -> IO ()
verbosePrint args s =
  if verbose args
    then do
    -- Remove quotes from show
      putStrLn $ filter (/= '"') (show s)
    else do
      return ()

argsInfo :: ParserInfo Args
argsInfo =
  info
    (argsParser <**> helper)
    (fullDesc
       <> progDesc "Compile Markdown to HTML with custom stylesheets."
       <> header
            "markdown-to-html - a simple Markdown to HTML compiler written in Haskell.")


main :: IO ()
main = do
  args <- execParser argsInfo
    -- Open the file
  file <- readFile (inputFile args)
  verbosePrint args $ "Opened " ++ (inputFile args) ++ " successfully."
    -- Parse the markdown
  case parseMarkdown file of
    Left (ParseError err) -> do
      putStrLn "Error: "
      print err
    Right elements -> do
      verbosePrint args "Parsed markdown successfully."
      let output =
            (case outputFile args of
               Just path -> path
               Nothing -> "output.html")
      -- Confirm all the stylesheets exist
      mapM_
        (\path -> do
           exists <- doesFileExist path
           if exists
             then return ()
             else do
               putStrLn $ "Error: Stylesheet " ++ path ++ " does not exist."
               exitFailure)
        (styleSheets args)
      -- Open all the stylesheets
      styles <- mapM readFile (styleSheets args)
      verbosePrint args $ "Writing to " ++ output
      -- If there are no stylesheets, use the default ones in Styles.hs
      if null (styles)
        then do
          verbosePrint args "No stylesheets provided. Using default styles."
          writeFile output $ compileToHtml elements (defaultStyles (darkMode args))
        else do
          verbosePrint args "Using custom stylesheets."
          writeFile output $ compileToHtml elements styles
      verbosePrint args "File written successfully."
