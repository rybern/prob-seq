{-# LANGUAGE RecordWildCards #-}
module CLI where

import ConstructionFiles
import SparseTransFiles
import Sampling

import Control.Monad
import System.Exit
import Options.Applicative
import Data.Semigroup ((<>))

data Options = Options {
    inputFile :: FilePath
  , stOutput :: Maybe FilePath
  , stpOutput :: Maybe FilePath
  , sampling :: Maybe Int
  }

runCLI :: IO ()
runCLI = execParser opts >>= runOptions
  where opts = info (cliParser <**> helper)
          (fullDesc
           <> progDesc "[program description]"
           <> header "[short description header]")

runOptions :: Options -> IO ()
runOptions (Options {..}) = do
  when (stOutput == Nothing
        && stpOutput == Nothing
        && sampling == Nothing) $
     die "No output or actions - exiting"

  seq <- readSeqFile (inputFile)

  case stOutput of
    Nothing -> return ()
    Just fp -> writeSTFile fp seq

  case stpOutput of
    Nothing -> return ()
    Just fp -> writeSTPFile fp seq

  case sampling of
    Nothing -> return ()
    Just n -> replicateM_ n $ sample seq >>= print

  return ()

maybeOption nilval option settings = (\v -> if v == nilval then Nothing else Just v) <$> option settings

autoOption = option auto

cliParser :: Parser Options
cliParser = Options
            <$> strOption
            ( long "input"
              <> short 'i'
              <> metavar "FILE"
              <> help "Input file describing a probabilistic sequence. Accepts sparse matrix format (.st or .stp extensions) or sequence builder file (.stb extension).")
            <*> maybeOption "" strOption
            ( long "stOutput"
              <> metavar "ST_FILE"
              <> value ""
              <> help "Output file describing a sparse transition distribution. This format may lose information, but should be interpretable to HMM programs.")
            <*> maybeOption "" strOption
            ( long "stpOutput"
              <> metavar "STP_FILE"
              <> value ""
              <> help "Output file describing a sparse transition distribution. This format should keep all information, but may not be interpretable to HMM programs.")
            <*> maybeOption 0 autoOption
            ( long "sample"
              <> short 's'
              <> metavar "N_SAMPLES"
              <> value 0
              <> help "Samples from the input sequence N_SAMPLES number of times. Prints each sample in a new line to stdout.")
