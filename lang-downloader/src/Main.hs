-- Main.hs ---

-- Copyright (C) 2019 Nerd Ed

-- Author: Nerd Ed <nerded.nerded@gmail.com>

-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License
-- as published by the Free Software Foundation; either version 3
-- of the License, or (at your option) any later version.

-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program. If not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main
  ( main
  ) where


import           Data.Bitraversable
import qualified Data.ByteString.Char8        as BS
import           Network.HTTP.Client          (Request, parseUrlThrow)
import           Network.HTTP.Download        (download)
import           Path
import           RIO
import           RIO.PrettyPrint
import           RIO.PrettyPrint.StylesUpdate

data DownloadApp = DownloadApp !StylesUpdate !LogFunc

instance HasStylesUpdate DownloadApp where
  stylesUpdateL f (DownloadApp x y) = (\x' -> DownloadApp x' y) <$> f x

instance HasLogFunc DownloadApp where
  logFuncL f (DownloadApp x y) = DownloadApp x <$> f y

instance HasTerm DownloadApp where
  useColorL f x = f True $> x
  termWidthL f x = f 80 $> x

runDownloadApp :: MonadIO m => RIO DownloadApp a -> m a
runDownloadApp m = liftIO $ do
  lo <- logOptionsHandle stderr False
  withLogFunc lo $ \lf ->
    runRIO (DownloadApp (StylesUpdate []) lf) m

baseURL :: String
baseURL = "http://dofusretro.cdn.ankama.com/lang"

versionsURL :: String
versionsURL = baseURL <> "/versions_fr.txt"

baseSwfDirectory :: Path Abs Dir
baseSwfDirectory = $(mkRelDir "data/dofus/lang/swf")

parseFileUrl :: BS.ByteString -> Maybe (Path Abs File, Request)
parseFileUrl x = go $ BS.unpack <$> BS.split ',' x
  where
    go :: [String] -> Maybe (Path Abs File, Request)
    go [name, lang, revision] =
      let fileName = name <> "_" <> lang <> "_" <> revision <> ".swf"
          fullURL = baseURL <> "/swf/" <> fileName
      in bitraverse (fmap (baseSwfDirectory </>) . parseRelFile) parseUrlThrow (fileName, fullURL)
    go _ = Nothing

main :: IO ()
main = do
  let versionsFileUrl = parseUrlThrow versionsURL
      versionsFilePath = parseAbsFile "/tmp/dofus_versions.txt"
  void $ runDownloadApp $ case (versionsFileUrl, versionsFilePath) of
    (Just req, Just path) -> do
      logInfo $ "Downloading version file: " <> displayShow versionsFileUrl
      void $ download req path
      versionsFileContent <- liftIO $ BS.readFile (toFilePath path)
      let files = filter (/= mempty) $ BS.split '|' $ BS.drop 3 versionsFileContent
          filesUrl = traverse parseFileUrl files
      case filesUrl of
        Nothing -> logInfo "Failed to parse swf file url"
        Just swfFiles -> do
          logInfo "Downloading swf files"
          void $ traverse (\(name, url) -> logWarn ("Downloading: " <> displayShow name) *> download url name) swfFiles
    _ -> logInfo "Impossible to parse version url"
