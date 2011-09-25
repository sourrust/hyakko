type Callback = IO ()

generateDocumentation :: FilePath -> IO ()

parse :: FilePath -> String -> [(String, String)]

highlight :: FilePath -> [(String, String)] -> Callback -> IO ()

generateHTML :: FilePath -> [(String, String)] -> IO ()

languages :: [(String, [(String, String)])]

getLanguage :: FilePath -> [(String, String)]

destination :: FilePath -> FilePath

ensureDirectory :: Callback -> IO ()

hyakkoTemplate :: [(String, String)] -> IO String

hyakkoStyles :: IO String

highlightStart, highlightEnd :: String

sources :: [FilePath]

main :: IO ()
