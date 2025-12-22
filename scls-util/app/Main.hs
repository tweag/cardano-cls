{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Cardano.SCLS.Util.Check
import Cardano.SCLS.Util.Checksum
import Cardano.SCLS.Util.Debug
import Cardano.SCLS.Util.Info
import Cardano.SCLS.Util.Result
import Cardano.SCLS.Util.Tool
import Cardano.Types.Namespace (Namespace (..))
import Cardano.Types.Namespace qualified as Namespace
import Data.Text (Text)
import Data.Text qualified as T
import Options.Applicative
import System.Exit (exitWith)

-- | Command-line options
data Options = Options
  { optCommand :: Command
  }

data Command
  = Checksum ChecksumCmd
  | Check FilePath
  | Info FilePath
  | ListNamespaces FilePath
  | Split FilePath FilePath
  | Merge FilePath [FilePath]
  | Extract FilePath FilePath ExtractOptions
  | Debug CommandDebug

data CommandDebug
  = GenerateDebugFile FilePath [(Namespace, Maybe Int)]
  | PrintHex FilePath Text Int

parseOptions :: Parser Options
parseOptions =
  Options
    <$> hsubparser
      ( command
          "checksum"
          ( info
              ( fmap Checksum $
                  ChecksumCmd
                    <$> fileArg
                    <*> optional namespace
                    <*> noVerify
                    <*> quietSwitch
              )
              (progDesc "Verify root hash of chunks")
          )
          <> command
            "info"
            ( info
                (Info <$> fileArg)
                (progDesc "Display SCLS file information")
            )
          <> command
            "list-ns"
            ( info
                (ListNamespaces <$> fileArg)
                (progDesc "List all namespaces in the file")
            )
          <> command
            "split"
            ( info
                (Split <$> fileArg <*> dirArg)
                (progDesc "Split SCLS file into separate files by namespace")
            )
          <> command
            "merge"
            ( info
                (Merge <$> fileArg <*> some (argument str (metavar "FILES")))
                (progDesc "Merge multiple SCLS files into one (last file is output)")
            )
          <> command
            "extract"
            ( info
                (Extract <$> fileArg <*> (argument str (metavar "OUTPUT_FILE" <> help "Output file for extracted data")) <*> extractOptions)
                (progDesc "Extract specific data into a new SCLS file")
            )
          <> command
            "check"
            ( info
                (Check <$> fileArg)
                (progDesc "Check the integrity and validity of an SCLS file")
            )
          <> command "debug" (info (Debug <$> debugCommand) (progDesc "Debugging utilities"))
      )
 where
  quietSwitch =
    switch
      ( long "quiet"
          <> short 'q'
          <> help "Suppress output except"
      )
  noVerify =
    switch
      ( long "no-verify"
          <> help "Skip verification step"
      )
  fileArg =
    argument
      str
      (metavar "FILE" <> help "Path to SCLS file")
  dirArg =
    argument
      str
      (metavar "OUTPUT_DIR" <> help "Output directory for split files")
  namespace =
    strOption
      ( long "namespace"
          <> short 'n'
          <> metavar "NAMESPACE"
          <> help "Namespace identifier"
      )
  namespaceArg =
    argument
      str
      (metavar "NAMESPACE" <> help "Namespace identifier")
  extractOptions :: Parser ExtractOptions
  extractOptions =
    ExtractOptions
      <$> namespaceOption
  namespaceOption =
    optional $
      option
        parseNamespaceList
        ( long "namespaces"
            <> short 'n'
            <> metavar "NAMESPACES"
            <> help "Comma-separated list of namespaces to extract"
        )
  parseNamespaceList :: ReadM [Namespace]
  parseNamespaceList = eitherReader $ \ns ->
    Right $ map (Namespace.fromText . T.strip) (T.split (== ',') (T.pack ns))
  debugCommand :: Parser CommandDebug
  debugCommand =
    hsubparser
      ( command
          "generate"
          ( info
              (GenerateDebugFile <$> fileArg <*> namespaceEntriesOption)
              (progDesc "Generate a debug SCLS file with random data")
          )
          <> command
            "print-hex"
            ( info
                ( PrintHex
                    <$> fileArg
                    <*> namespaceArg
                    <*> option
                      auto
                      ( long "entry"
                          <> short 'e'
                          <> metavar "ENTRY_NO"
                          <> help "Entry number to print in hex"
                      )
                )
                (progDesc "Print a specific entry in hex format")
            )
      )
   where
    namespaceEntriesOption :: Parser [(Namespace, Maybe Int)]
    namespaceEntriesOption =
      many $
        option
          parseNamespaceEntries
          ( long "namespace"
              <> metavar "NAMESPACE[:COUNT]"
              <> help "Namespace and optional number of entries to generate, default is 16"
          )
    parseNamespaceEntries :: ReadM (Namespace, Maybe Int)
    parseNamespaceEntries = eitherReader $ \arg ->
      case T.split (== ':') (T.pack arg) of
        [ns] -> Right (Namespace.fromText (T.strip ns), Nothing)
        [ns, countText] ->
          case reads (T.unpack (T.strip countText)) :: [(Int, String)] of
            [(count, "")] -> Right (Namespace.fromText (T.strip ns), Just count)
            _ -> Left $ "Invalid count: " ++ T.unpack countText
        _ -> Left $ "Invalid namespace entry: " ++ arg

-- | Main entry point
main :: IO ()
main = do
  opts <-
    execParser $
      info
        (parseOptions <**> helper)
        ( fullDesc
            <> progDesc "SCLS Utility Tool is used to inspect and manipulate SCLS files."
            <> header "scls-util - A utility for working with SCLS files"
        )
  result <- runCommand (optCommand opts)
  exitWith $ toErrorCode result

-- | Execute the selected command
runCommand :: Command -> IO Result
runCommand = \case
  Checksum checksumCmd -> runChecksumCmd checksumCmd
  Info file -> displayInfo file
  ListNamespaces file -> listNamespaces file
  Split file outputDir -> splitFile file outputDir
  Merge _ [] -> do
    putStrLn "Error: No files provided"
    pure OtherError
  Merge outputFile allFiles ->
    mergeFiles outputFile allFiles
  Extract file outputDir options ->
    extract file outputDir options
  Check file -> check file
  Debug debugCmd -> case debugCmd of
    GenerateDebugFile outputFile namespaceEntries -> generateDebugFile outputFile namespaceEntries
    PrintHex file chunkNo entryNo -> printHexEntries file chunkNo entryNo
