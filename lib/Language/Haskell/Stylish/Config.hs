--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.Stylish.Config
    ( Extensions
    , Config (..)
    , defaultConfigBytes
    , configFilePath
    , loadConfig
    ) where


--------------------------------------------------------------------------------
import           Control.Monad                                    (forM, mzero)
import           Data.Aeson                                       (FromJSON (..))
import qualified Data.Aeson                                       as A
import qualified Data.Aeson.Types                                 as A
import qualified Data.ByteString                                  as B
import           Data.List                                        (inits,
                                                                   intercalate)
import           Data.Map                                         (Map)
import qualified Data.Map                                         as M
import           Data.Maybe                                       (fromMaybe)
import           Data.Yaml                                        (decodeEither',
                                                                   prettyPrintParseException)
import           System.Directory
import           System.FilePath                                  (joinPath,
                                                                   splitPath,
                                                                   (</>))
import qualified System.IO                                        as IO (Newline (..),
                                                                         nativeNewline)


--------------------------------------------------------------------------------
import           Language.Haskell.Stylish.Step
import qualified Language.Haskell.Stylish.Step.Imports            as Imports
import qualified Language.Haskell.Stylish.Step.LanguagePragmas    as LanguagePragmas
import qualified Language.Haskell.Stylish.Step.SimpleAlign        as SimpleAlign
import qualified Language.Haskell.Stylish.Step.Squash             as Squash
import qualified Language.Haskell.Stylish.Step.Tabs               as Tabs
import qualified Language.Haskell.Stylish.Step.TrailingWhitespace as TrailingWhitespace
import qualified Language.Haskell.Stylish.Step.UnicodeSyntax      as UnicodeSyntax
import           Language.Haskell.Stylish.Verbose


--------------------------------------------------------------------------------
type Extensions = [String]


--------------------------------------------------------------------------------
data Config = Config
    { configSteps              :: [Step]
    , configColumns            :: Int
    , configLanguageExtensions :: [String]
    , configNewline            :: IO.Newline
    }


--------------------------------------------------------------------------------
instance FromJSON Config where
    parseJSON = parseConfig


--------------------------------------------------------------------------------
configFileName :: String
configFileName = ".stylish-haskell.yaml"


--------------------------------------------------------------------------------
defaultConfigBytes :: B.ByteString
defaultConfigBytes =
  "# stylish-haskell configuration file\n\
  \# ==================================\n\

  \# The stylish-haskell tool is mainly configured by specifying steps. These steps\n\
  \# are a list, so they have an order, and one specific step may appear more than\n\
  \# once (if needed). Each file is processed by these steps in the given order.\n\
  \steps:\n\
  \  # Convert some ASCII sequences to their Unicode equivalents. This is disabled\n\
  \  # by default.\n\
  \  # - unicode_syntax:\n\
  \  #     # In order to make this work, we also need to insert the UnicodeSyntax\n\
  \  #     # language pragma. If this flag is set to true, we insert it when it's\n\
  \  #     # not already present. You may want to disable it if you configure\n\
  \  #     # language extensions using some other method than pragmas. Default:\n\
  \  #     # true.\n\
  \  #     add_language_pragma: true\n\
  \\n\
  \  # Align the right hand side of some elements.  This is quite conservative\n\
  \  # and only applies to statements where each element occupies a single\n\
  \  # line. All default to true.\n\
  \  - simple_align:\n\
  \      cases: true\n\
  \      top_level_patterns: true\n\
  \      records: true\n\
  \\n\
  \  # Import cleanup\n\
  \  - imports:\n\
  \      # There are different ways we can align names and lists.\n\
  \      #\n\
  \      # - global: Align the import names and import list throughout the entire\n\
  \      #   file.\n\
  \      #\n\
  \      # - file: Like global, but don't add padding when there are no qualified\n\
  \      #   imports in the file.\n\
  \      #\n\
  \      # - group: Only align the imports per group (a group is formed by adjacent\n\
  \      #   import lines).\n\
  \      #\n\
  \      # - none: Do not perform any alignment.\n\
  \      #\n\
  \      # Default: global.\n\
  \      align: global\n\
  \\n\
  \      # The following options affect only import list alignment.\n\
  \      #\n\
  \      # List align has following options:\n\
  \      #\n\
  \      # - after_alias: Import list is aligned with end of import including\n\
  \      #   'as' and 'hiding' keywords.\n\
  \      #\n\
  \      #   > import qualified Data.List      as List (concat, foldl, foldr, head,\n\
  \      #   >                                          init, last, length)\n\
  \      #\n\
  \      # - with_alias: Import list is aligned with start of alias or hiding.\n\
  \      #\n\
  \      #   > import qualified Data.List      as List (concat, foldl, foldr, head,\n\
  \      #   >                                 init, last, length)\n\
  \      #\n\
  \      # - with_module_name: Import list is aligned `list_padding` spaces after\n\
  \      #   the module name.\n\
  \      #\n\
  \      #   > import qualified Data.List      as List (concat, foldl, foldr, head,\n\
  \      #                          init, last, length)\n\
  \      #\n\
  \      #   This is mainly intended for use with `pad_module_names: false`.\n\
  \      #\n\
  \      #   > import qualified Data.List as List (concat, foldl, foldr, head,\n\
  \      #                          init, last, length, scanl, scanr, take, drop,\n\
  \      #                          sort, nub)\n\
  \      #\n\
  \      # - new_line: Import list starts always on new line.\n\
  \      #\n\
  \      #   > import qualified Data.List      as List\n\
  \      #   >     (concat, foldl, foldr, head, init, last, length)\n\
  \      #\n\
  \      # Default: after_alias\n\
  \      list_align: after_alias\n\
  \\n\
  \      # Right-pad the module names to align imports in a group:\n\
  \      #\n\
  \      # - true: a little more readable\n\
  \      #\n\
  \      #   > import qualified Data.List       as List (concat, foldl, foldr,\n\
  \      #   >                                           init, last, length)\n\
  \      #   > import qualified Data.List.Extra as List (concat, foldl, foldr,\n\
  \      #   >                                           init, last, length)\n\
  \      #\n\
  \      # - false: diff-safe\n\
  \      #\n\
  \      #   > import qualified Data.List as List (concat, foldl, foldr, init,\n\
  \      #   >                                     last, length)\n\
  \      #   > import qualified Data.List.Extra as List (concat, foldl, foldr,\n\
  \      #   >                                           init, last, length)\n\
  \      #\n\
  \      # Default: true\n\
  \      pad_module_names: true\n\
  \\n\
  \      # Long list align style takes effect when import is too long. This is\n\
  \      # determined by 'columns' setting.\n\
  \      #\n\
  \      # - inline: This option will put as much specs on same line as possible.\n\
  \      #\n\
  \      # - new_line: Import list will start on new line.\n\
  \      #\n\
  \      # - new_line_multiline: Import list will start on new line when it's\n\
  \      #   short enough to fit to single line. Otherwise it'll be multiline.\n\
  \      #\n\
  \      # - multiline: One line per import list entry.\n\
  \      #   Type with constructor list acts like single import.\n\
  \      #\n\
  \      #   > import qualified Data.Map as M\n\
  \      #   >     ( empty\n\
  \      #   >     , singleton\n\
  \      #   >     , ...\n\
  \      #   >     , delete\n\
  \      #   >     )\n\
  \      #\n\
  \      # Default: inline\n\
  \      long_list_align: inline\n\
  \\n\
  \      # Align empty list (importing instances)\n\
  \      #\n\
  \      # Empty list align has following options\n\
  \      #\n\
  \      # - inherit: inherit list_align setting\n\
  \      #\n\
  \      # - right_after: () is right after the module name:\n\
  \      #\n\
  \      #   > import Vector.Instances ()\n\
  \      #\n\
  \      # Default: inherit\n\
  \      empty_list_align: inherit\n\
  \\n\
  \      # List padding determines indentation of import list on lines after import.\n\
  \      # This option affects 'long_list_align'.\n\
  \      #\n\
  \      # - <integer>: constant value\n\
  \      #\n\
  \      # - module_name: align under start of module name.\n\
  \      #   Useful for 'file' and 'group' align settings.\n\
  \      #\n\
  \      # Default: 4\n\
  \      list_padding: 4\n\
  \\n\
  \      # Separate lists option affects formatting of import list for type\n\
  \      # or class. The only difference is single space between type and list\n\
  \      # of constructors, selectors and class functions.\n\
  \      #\n\
  \      # - true: There is single space between Foldable type and list of it's\n\
  \      #   functions.\n\
  \      #\n\
  \      #   > import Data.Foldable (Foldable (fold, foldl, foldMap))\n\
  \      #\n\
  \      # - false: There is no space between Foldable type and list of it's\n\
  \      #   functions.\n\
  \      #\n\
  \      #   > import Data.Foldable (Foldable(fold, foldl, foldMap))\n\
  \      #\n\
  \      # Default: true\n\
  \      separate_lists: true\n\
  \\n\
  \      # Space surround option affects formatting of import lists on a single\n\
  \      # line. The only difference is single space after the initial\n\
  \      # parenthesis and a single space before the terminal parenthesis.\n\
  \      #\n\
  \      # - true: There is single space associated with the enclosing\n\
  \      #   parenthesis.\n\
  \      #\n\
  \      #   > import Data.Foo ( foo )\n\
  \      #\n\
  \      # - false: There is no space associated with the enclosing parenthesis\n\
  \      #\n\
  \      #   > import Data.Foo (foo)\n\
  \      #\n\
  \      # Default: false\n\
  \      space_surround: false\n\
  \\n\
  \  # Language pragmas\n\
  \  - language_pragmas:\n\
  \      # We can generate different styles of language pragma lists.\n\
  \      #\n\
  \      # - vertical: Vertical-spaced language pragmas, one per line.\n\
  \      #\n\
  \      # - compact: A more compact style.\n\
  \      #\n\
  \      # - compact_line: Similar to compact, but wrap each line with\n\
  \      #   `{-#LANGUAGE #-}'.\n\
  \      #\n\
  \      # Default: vertical.\n\
  \      style: vertical\n\

  \      # Align affects alignment of closing pragma brackets.\n\
  \      #\n\
  \      # - true: Brackets are aligned in same column.\n\
  \      #\n\
  \      # - false: Brackets are not aligned together. There is only one space\n\
  \      #   between actual import and closing bracket.\n\
  \      #\n\
  \      # Default: true\n\
  \      align: true\n\
  \\n\
  \      # stylish-haskell can detect redundancy of some language pragmas. If this\n\
  \      # is set to true, it will remove those redundant pragmas. Default: true.\n\
  \      remove_redundant: true\n\
  \\n\
  \  # Replace tabs by spaces. This is disabled by default.\n\
  \  # - tabs:\n\
  \  #     # Number of spaces to use for each tab. Default: 8, as specified by the\n\
  \  #     # Haskell report.\n\
  \  #     spaces: 8\n\
  \\n\
  \  # Remove trailing whitespace\n\
  \  - trailing_whitespace: {}\n\
  \\n\
  \  # Squash multiple spaces between the left and right hand sides of some\n\
  \  # elements into single spaces. Basically, this undoes the effect of\n\
  \  # simple_align but is a bit less conservative.\n\
  \  # - squash: {}\n\
  \\n\
  \# A common setting is the number of columns (parts of) code will be wrapped\n\
  \# to. Different steps take this into account. Default: 80.\n\
  \columns: 80\n\
  \\n\
  \# By default, line endings are converted according to the OS. You can override\n\
  \# preferred format here.\n\
  \#\n\
  \# - native: Native newline format. CRLF on Windows, LF on other OSes.\n\
  \#\n\
  \# - lf: Convert to LF (\"\\n\").\n\
  \#\n\
  \# - crlf: Convert to CRLF (\"\r\\n\").\n\
  \#\n\
  \# Default: native.\n\
  \newline: native\n\
  \\n\
  \# Sometimes, language extensions are specified in a cabal file or from the\n\
  \# command line instead of using language pragmas in the file. stylish-haskell\n\
  \# needs to be aware of these, so it can parse the file correctly.\n\
  \#\n\
  \# No language extensions are enabled by default.\n\
  \# language_extensions:\n\
  \  # - TemplateHaskell\n\
  \  # - QuasiQuotes"


--------------------------------------------------------------------------------
configFilePath :: Verbose -> Maybe FilePath -> IO (Maybe FilePath)
configFilePath _       (Just userSpecified) = return (Just userSpecified)
configFilePath verbose Nothing              = do
    current    <- getCurrentDirectory
    configPath <- getXdgDirectory XdgConfig "stylish-haskell"
    home       <- getHomeDirectory
    mbConfig   <- search $
        [d </> configFileName | d <- ancestors current] ++
        [configPath </> "config.yaml", home </> configFileName]

    return mbConfig
  where
    -- All ancestors of a dir (including that dir)
    ancestors :: FilePath -> [FilePath]
    ancestors = init . map joinPath . reverse . inits . splitPath

    search :: [FilePath] -> IO (Maybe FilePath)
    search []       = return Nothing
    search (f : fs) = do
        -- TODO Maybe catch an error here, dir might be unreadable
        exists <- doesFileExist f
        verbose $ f ++ if exists then " exists" else " does not exist"
        if exists then return (Just f) else search fs


--------------------------------------------------------------------------------
loadConfig :: Verbose -> Maybe FilePath -> IO Config
loadConfig verbose userSpecified = do
    mbFp <- configFilePath verbose userSpecified
    verbose $ "Loading configuration at " ++ fromMaybe "<embedded>" mbFp
    bytes <- maybe (return defaultConfigBytes) B.readFile mbFp
    case decodeEither' bytes of
        Left err     -> error $
            "Language.Haskell.Stylish.Config.loadConfig: " ++ prettyPrintParseException err
        Right config -> return config


--------------------------------------------------------------------------------
parseConfig :: A.Value -> A.Parser Config
parseConfig (A.Object o) = do
    -- First load the config without the actual steps
    config <- Config
        <$> pure []
        <*> (o A..:? "columns"             A..!= 80)
        <*> (o A..:? "language_extensions" A..!= [])
        <*> (o A..:? "newline"             >>= parseEnum newlines IO.nativeNewline)

    -- Then fill in the steps based on the partial config we already have
    stepValues <- o A..: "steps" :: A.Parser [A.Value]
    steps      <- mapM (parseSteps config) stepValues
    return config {configSteps = concat steps}
  where
    newlines =
        [ ("native", IO.nativeNewline)
        , ("lf",     IO.LF)
        , ("crlf",   IO.CRLF)
        ]
parseConfig _            = mzero


--------------------------------------------------------------------------------
catalog :: Map String (Config -> A.Object -> A.Parser Step)
catalog = M.fromList
    [ ("imports",             parseImports)
    , ("language_pragmas",    parseLanguagePragmas)
    , ("simple_align",        parseSimpleAlign)
    , ("squash",              parseSquash)
    , ("tabs",                parseTabs)
    , ("trailing_whitespace", parseTrailingWhitespace)
    , ("unicode_syntax",      parseUnicodeSyntax)
    ]


--------------------------------------------------------------------------------
parseSteps :: Config -> A.Value -> A.Parser [Step]
parseSteps config val = do
    map' <- parseJSON val :: A.Parser (Map String A.Value)
    forM (M.toList map') $ \(k, v) -> case (M.lookup k catalog, v) of
        (Just parser, A.Object o) -> parser config o
        _                         -> fail $ "Invalid declaration for " ++ k


--------------------------------------------------------------------------------
-- | Utility for enum-like options
parseEnum :: [(String, a)] -> a -> Maybe String -> A.Parser a
parseEnum _    def Nothing  = return def
parseEnum strs _   (Just k) = case lookup k strs of
    Just v  -> return v
    Nothing -> fail $ "Unknown option: " ++ k ++ ", should be one of: " ++
        intercalate ", " (map fst strs)


--------------------------------------------------------------------------------
parseSimpleAlign :: Config -> A.Object -> A.Parser Step
parseSimpleAlign c o = SimpleAlign.step
    <$> pure (configColumns c)
    <*> (SimpleAlign.Config
        <$> withDef SimpleAlign.cCases            "cases"
        <*> withDef SimpleAlign.cTopLevelPatterns "top_level_patterns"
        <*> withDef SimpleAlign.cRecords          "records")
  where
    withDef f k = fromMaybe (f SimpleAlign.defaultConfig) <$> (o A..:? k)


--------------------------------------------------------------------------------
parseSquash :: Config -> A.Object -> A.Parser Step
parseSquash _ _ = return Squash.step


--------------------------------------------------------------------------------
parseImports :: Config -> A.Object -> A.Parser Step
parseImports config o = Imports.step
    <$> pure (configColumns config)
    <*> (Imports.Options
        <$> (o A..:? "align" >>= parseEnum aligns (def Imports.importAlign))
        <*> (o A..:? "list_align" >>= parseEnum listAligns (def Imports.listAlign))
        <*> (o A..:? "pad_module_names" A..!= def Imports.padModuleNames)
        <*> (o A..:? "long_list_align"
            >>= parseEnum longListAligns (def Imports.longListAlign))
        -- Note that padding has to be at least 1. Default is 4.
        <*> (o A..:? "empty_list_align"
            >>= parseEnum emptyListAligns (def Imports.emptyListAlign))
        <*> o A..:? "list_padding" A..!= (def Imports.listPadding)
        <*> o A..:? "separate_lists" A..!= (def Imports.separateLists)
        <*> o A..:? "space_surround" A..!= (def Imports.spaceSurround))
  where
    def f = f Imports.defaultOptions

    aligns =
        [ ("global", Imports.Global)
        , ("file",   Imports.File)
        , ("group",  Imports.Group)
        , ("none",   Imports.None)
        ]

    listAligns =
        [ ("new_line",          Imports.NewLine)
        , ("with_module_name",  Imports.WithModuleName)
        , ("with_alias",        Imports.WithAlias)
        , ("after_alias",       Imports.AfterAlias)
        ]

    longListAligns =
        [ ("inline",             Imports.Inline)
        , ("new_line",           Imports.InlineWithBreak)
        , ("new_line_multiline", Imports.InlineToMultiline)
        , ("multiline",          Imports.Multiline)
        ]

    emptyListAligns =
        [ ("inherit", Imports.Inherit)
        , ("right_after", Imports.RightAfter)
        ]

--------------------------------------------------------------------------------
parseLanguagePragmas :: Config -> A.Object -> A.Parser Step
parseLanguagePragmas config o = LanguagePragmas.step
    <$> pure (configColumns config)
    <*> (o A..:? "style" >>= parseEnum styles LanguagePragmas.Vertical)
    <*> o A..:? "align" A..!= True
    <*> o A..:? "remove_redundant" A..!= True
  where
    styles =
        [ ("vertical",     LanguagePragmas.Vertical)
        , ("compact",      LanguagePragmas.Compact)
        , ("compact_line", LanguagePragmas.CompactLine)
        ]


--------------------------------------------------------------------------------
parseTabs :: Config -> A.Object -> A.Parser Step
parseTabs _ o = Tabs.step
    <$> o A..:? "spaces" A..!= 8


--------------------------------------------------------------------------------
parseTrailingWhitespace :: Config -> A.Object -> A.Parser Step
parseTrailingWhitespace _ _ = return TrailingWhitespace.step


--------------------------------------------------------------------------------
parseUnicodeSyntax :: Config -> A.Object -> A.Parser Step
parseUnicodeSyntax _ o = UnicodeSyntax.step
    <$> o A..:? "add_language_pragma" A..!= True
