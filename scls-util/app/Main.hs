{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Cardano.SCLS.Util.Checksum
import Cardano.SCLS.Util.Debug
import Cardano.SCLS.Util.File qualified as File
import Cardano.SCLS.Util.Info qualified as Info
import Cardano.SCLS.Util.Result
import Cardano.SCLS.Util.Verify
import Cardano.Types.Namespace (Namespace (..))
import Cardano.Types.Namespace qualified as Namespace
import Control.Monad.Catch (MonadCatch, SomeException (..), catch)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Logger
import Control.Monad.Trans.Resource
import Data.Text (Text)
import Data.Text qualified as T
import Options.Applicative
import System.Exit (exitWith)
import System.IO (hPutStrLn, stderr)

-- | Command-line options
data Options = Options
  { optCommand :: Command
  , optQuiet :: Bool
  , optNamespaceKeySizes :: [(Namespace, Int)]
  }

data Command
  = Checksum ChecksumCmd
  | Verify FilePath
  | File FilePath File.FileCmd
  | Debug CommandDebug
  | Info Info.InfoCmd

data CommandDebug
  = GenerateDebugFile FilePath [(Namespace, Maybe Int)]
  | PrintHex FilePath Text Int

parseOptions :: Parser Options
parseOptions =
  Options
    <$> ( hsubparser
            ( command
                "checksum"
                ( info
                    ( fmap Checksum $
                        ChecksumCmd
                          <$> fileArg
                          <*> optional namespace
                          <*> noVerify
                    )
                    (progDesc "Display and verify root hash or namespace hashes")
                )
                <> command
                  "verify"
                  ( info
                      (Verify <$> fileArg)
                      (progDesc "Check the integrity and validity of an SCLS file")
                  )
            )
            <|> hsubparser
              ( command
                  "file"
                  ( info
                      (File <$> fileArg <*> hsubparser fileCmd)
                      (progDesc "File manipulation commands")
                  )
                  <> command
                    "info"
                    ( info
                        ( Info
                            <$> hsubparser
                              ( command
                                  "namespaces"
                                  ( info
                                      (pure Info.Namespaces)
                                      (progDesc "List all supported namespaces")
                                  )
                                  <> command
                                    "cddl"
                                    ( info
                                        (Info.CDDL <$> namespaceArg)
                                        (progDesc "Display CDDL specification for a given namespace")
                                    )
                              )
                        )
                        (progDesc "Display information about SCLS format")
                    )
                  <> command "debug" (info (Debug <$> debugCommand) (progDesc "Debugging utilities"))
                  <> commandGroup "Command groups"
                  <> hidden
              )
        )
    <*> quietSwitch
    <*> namespaceKeySizeArg
 where
  fileCmd :: Mod CommandFields File.FileCmd
  fileCmd =
    command
      "list-ns"
      ( info
          (pure File.ListNamespaces)
          (progDesc "List all namespaces in the file")
      )
      <> command
        "split"
        ( info
            (File.Split <$> dirArg <*> splitOptions)
            (progDesc "Split SCLS file into separate files by namespace")
        )
      <> command
        "merge"
        ( info
            (File.Merge <$> many (argument str (metavar "FILES")))
            (progDesc "Merge multiple SCLS files into one")
        )
      <> command
        "extract"
        ( info
            (File.Extract <$> (argument str (metavar "OUTPUT_FILE" <> help "Output file for extracted data")) <*> extractOptions)
            (progDesc "Extract specific data into a new SCLS file")
        )
      <> command
        "info"
        ( info
            (pure File.Info)
            (progDesc "Display SCLS file information")
        )
      <> command
        "unpack"
        ( info
            ( File.Unpack
                <$> fileArg
                <*> namespaceArg
                <*> unpackOptions
            )
            (progDesc "Unpack data from a specific namespace into a raw file")
        )
  quietSwitch =
    switch
      ( long "quiet"
          <> short 'q'
          <> help "Suppress output"
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
  namespaceKeySizeArg =
    many $
      option
        parseNamespaceKeySize
        ( long "namespace-keysize"
            <> metavar "NAMESPACE:SIZE"
            <> help "Namespace and its key size in bytes"
        )
  parseNamespaceKeySize :: ReadM (Namespace, Int)
  parseNamespaceKeySize = eitherReader $ \arg ->
    case T.split (== ':') (T.pack arg) of
      [ns, countText] ->
        case reads (T.unpack (T.strip countText)) :: [(Int, String)] of
          [(count, "")] -> Right (Namespace.fromText (T.strip ns), count)
          _ -> Left $ "Invalid size: " ++ T.unpack countText
      _ -> Left $ "Invalid namespace keysize: " ++ arg

  extractOptions :: Parser File.ExtractOptions
  extractOptions =
    File.ExtractOptions
      <$> namespaceOption
      <*> switch (long "no-verify" <> help "Skip hash verification after extraction")
  splitOptions :: Parser File.SplitOptions
  splitOptions =
    File.SplitOptions
      <$> switch (long "no-verify" <> help "Skip hash verification after split")
  unpackOptions =
    pure File.UnpackOptions
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
  result <-
    if optQuiet opts
      then runNoLoggingT $
        catch
          do runCommand (optNamespaceKeySizes opts) (optCommand opts)
          \(SomeException e) -> do
            liftIO $ hPutStrLn stderr $ "Error: " <> show e
            pure OtherError
      else runStderrLoggingT $
        catch
          do runCommand (optNamespaceKeySizes opts) (optCommand opts)
          \(SomeException e) -> do
            logErrorN $ "Error: " <> T.pack (show e)
            pure OtherError
  exitWith $ toErrorCode result

-- | Execute the selected command
runCommand :: (MonadLogger m, MonadCatch m, MonadIO m, MonadUnliftIO m) => [(Namespace, Int)] -> Command -> m Result
runCommand namespaceKeySizes = \case
  Checksum checksumCmd -> runChecksumCmd checksumCmd
  File fileName fileCmd -> File.runFileCmd namespaceKeySizes fileName fileCmd
  Info infoCmd -> Info.runInfoCmd infoCmd
  Verify file -> check file
  Debug debugCmd -> case debugCmd of
    GenerateDebugFile outputFile namespaceEntries -> generateDebugFile outputFile namespaceEntries
    PrintHex file chunkNo entryNo -> printHexEntries file chunkNo entryNo
