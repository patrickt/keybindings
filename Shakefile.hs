{-# LANGUAGE BlockArguments #-}

import Control.Exception
import Control.Monad.IO.Class
import Data.Function
import Data.List
import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util
import System.Directory
import Data.Foldable
import System.Posix.Files

mostRecentlyDownloaded :: FilePattern -> Action FilePath
mostRecentlyDownloaded pat = do
  home <- liftIO getHomeDirectory
  files <- getDirectoryFiles (home </> "Downloads") [pat]
  assert (not (null files)) (pure ())

  let lastModified t = modificationTime <$> getFileStatus (home </> "Downloads" </> t)
  times <- liftIO (traverse lastModified files)
  let newest = fst (maximumBy (compare `on` snd) (zip files times))
  pure newest

main :: IO ()
main = shakeArgs shakeOptions {shakeFiles = "_build", shakeVerbosity = Verbose} $ do

  want ["keymacs.edn", "custom.hex"]

  phony "clean" do
    putInfo "Removing keymacs.edn"
    removeFilesAfter "." ["keymacs.edn"]

  "custom.hex" %> \out -> do
    alwaysRerun
    home <- liftIO getHomeDirectory
    let qmk = home </> "src/qmk_firmware"
    let keymacs = qmk
    newest <- mostRecentlyDownloaded "firmware-*.zip"

    cmd_ "unzip -o" [home </> "Downloads" </> newest, "-d", keymacs]
    cmd_ "make -C" [qmk, "keymacs/a620n88a/teensy_2:custom"]
    cmd_ "open -a" ["Teensy"]
    copyFileChanged (qmk </> "keymacs_a620n88a_teensy_2_custom.hex") out


  "keymacs.edn" %> \out -> do
    alwaysRerun
    home <- liftIO getHomeDirectory
    newest <- mostRecentlyDownloaded "*.edn"
    copyFile' (home </> "Downloads" </> newest) out
