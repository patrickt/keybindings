{-# LANGUAGE BlockArguments #-}

import Control.Exception
import Data.Function
import Data.List
import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util
import System.Directory
import System.Posix.Files

main :: IO ()
main = shakeArgs shakeOptions {shakeFiles = "_build"} $ do
  want ["keymacs.edn"]

  phony "clean" do
    putInfo "Removing keymacs.edn"
    removeFilesAfter "." ["keymacs.edn"]

  "keymacs.edn" %> \out -> do
    alwaysRerun
    home <- liftIO getHomeDirectory
    files <- getDirectoryFiles (home </> "Downloads") ["*.edn"]
    assert (not (null files)) (pure ())

    let lastModified t = modificationTime <$> getFileStatus (home </> "Downloads" </> t)
    times <- liftIO (traverse lastModified files)
    let newest = fst (maximumBy (compare `on` snd) (zip files times))

    copyFile' (home </> "Downloads" </> newest) out
