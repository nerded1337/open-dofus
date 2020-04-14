-- Map.hs ---

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
{-# LANGUAGE TypeApplications  #-}

module OpenDofus.Database.SWF.Map
  ( loadMaps
  )
where

import           Data.Aeson
import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8         as BS
import           Data.Either
import           Data.Foldable
import qualified Data.HashMap.Strict           as H
import qualified Data.IntMap.Strict            as M
import           Data.Maybe
import qualified Data.Text                     as T
import qualified Data.Text.Read                as T
import qualified Data.Vector                   as V
import           System.Directory
import           OpenDofus.Database.Game.Map
import qualified OpenDofus.Database.SWF.Reader as SWF
import           OpenDofus.Prelude       hiding ( Map )

parseFileName :: BS.ByteString -> Either String String
parseFileName = parseOnly go
 where
  go = decimal @Int *> char '_' *> many1 (satisfy isDigit) <* string "X.swf"

getMapsDate
  :: FilePath
  -> IO (M.IntMap (String, Int, Int, Int, Int, Int, Bool, Int, T.Text))
getMapsDate path = fold <$> (traverse go =<< listDirectory path)
 where
  go fileName = SWF.loadData (path <> "/" <> fileName) $ \mapInfos -> do
    let get i = fromMaybe (error (show i)) $ H.lookup i mapInfos
        mdate =
          fromRight (error "Invalid map file name") $ parseFileName $ BS.pack
            fileName
        mId               = SWF.unsafeInt "id" $ get "id"
        width             = SWF.unsafeInt "width" $ get "width"
        height            = SWF.unsafeInt "height" $ get "height"
        backgroundNum     = SWF.unsafeInt "backgroundNum" $ get "backgroundNum"
        ambianceId        = SWF.unsafeInt "ambianceId" $ get "ambianceId"
        musicId           = SWF.unsafeInt "musicId" $ get "musicId"
        bOutdoor          = SWF.unsafeBool "bOutdoor" $ get "bOutdoor"
        capabilities      = SWF.unsafeInt "capabilities" $ get "capabilities"
        mapCompressedData = SWF.unsafeString "mapData" $ get "mapData"
    pure $ M.singleton
      mId
      ( mdate
      , width
      , height
      , backgroundNum
      , ambianceId
      , musicId
      , bOutdoor
      , capabilities
      , mapCompressedData
      )

getMap
  :: Int -> (String, Int, Int, Int, Int, Int, Bool, Int, T.Text) -> Value -> Map
getMap mid (d, w, h, bn, aid, _, out, cap, dat) (Object m) =
  let get i = fromMaybe (error (show i)) $ H.lookup i m
      x  = SWF.unsafeInt "x" $ get "x"
      y  = SWF.unsafeInt "y" $ get "y"
      sa = SWF.unsafeInt "sa" $ get "sa"
  in  Map (MapId $ fromIntegral mid)
          (T.pack d)
          (MapSubAreaPK $ MapSubAreaId $ fromIntegral sa)
          x
          y
          w
          h
          bn
          aid
          out
          cap
          dat
          Nothing
getMap _ _ x = error $ "Unhandled map: " <> show x

getSuperArea :: Int -> Value -> MapSuperArea
getSuperArea suId (String x) =
  MapSuperArea (MapSuperAreaId $ fromIntegral suId) x
getSuperArea _ x = error $ "Unhandled map super area: " <> show x

getArea :: Int -> Value -> MapArea
getArea aId (Object area) = MapArea
  (MapAreaId $ fromIntegral aId)
  (SWF.unsafeString "n" $ fromMaybe (error "n") $ H.lookup "n" area)
  ( MapSuperAreaPK
  $ MapSuperAreaId
  $ fromIntegral
  $ SWF.unsafeInt "sua"
  $ fromMaybe (error "sua")
  $ H.lookup "sua" area
  )
getArea _ x = error $ "Unhandled area: " <> show x

getSubArea :: Int -> Value -> (MapSubArea, V.Vector MapSubAreaNeighbour)
getSubArea saId (Object subArea) =
  let subArea' = MapSubArea
        (MapSubAreaId $ fromIntegral saId)
        (SWF.unsafeString "n" $ fromMaybe (error "n") $ H.lookup "n" subArea)
        ( MapAreaPK
        $ MapAreaId
        $ fromIntegral
        $ SWF.unsafeInt "a"
        $ fromMaybe (error "a")
        $ H.lookup "a" subArea
        )
        mempty
      neighbours =
          MapSubAreaNeighbour (MapSubAreaPK (MapSubAreaId $ fromIntegral saId))
            .   MapSubAreaPK
            .   MapSubAreaId
            <$> (fromIntegral . SWF.unsafeInt "v" <$> SWF.unsafeArray
                  "v"
                  (fromMaybe (error "v") $ H.lookup "v" subArea)
                )
  in  (subArea', neighbours)
getSubArea _ x = error $ "Unhandled sub area: " <> show x

loadMaps
  :: FilePath
  -> FilePath
  -> IO
       ( V.Vector Map
       , V.Vector MapSuperArea
       , V.Vector MapArea
       , V.Vector (MapSubArea, V.Vector MapSubAreaNeighbour)
       )
loadMaps mapFilesPath mapsPath = do
  mapsInfos <- getMapsDate mapFilesPath
  SWF.loadData mapsPath $ \obj -> do
    let (Object base      ) = fromMaybe (error "MA") $ H.lookup "MA" obj
        (Object maps      ) = fromMaybe (error "m") $ H.lookup "m" base
        (Object superAreas) = fromMaybe (error "sua") $ H.lookup "sua" base
        (Object areas     ) = fromMaybe (error "a") $ H.lookup "a" base
        (Object subAreas  ) = fromMaybe (error "sa") $ H.lookup "sa" base
        maps'               = V.fromList $ catMaybes $ H.elems $ H.mapWithKey
          (\k v ->
            let mid = (fst <$> fromRight (error "Invalid k") $ T.decimal k)
            in  getMap mid <$> M.lookup mid mapsInfos <*> pure v
          )
          maps
        superAreas' = V.fromList $ H.elems $ H.mapWithKey
          (\k v ->
            let aid = (fst <$> fromRight (error "Invalid k") $ T.decimal k)
            in  getSuperArea aid v
          )
          superAreas
        areas' = V.fromList $ H.elems $ H.mapWithKey
          (\k v ->
            let aid = (fst <$> fromRight (error "Invalid k") $ T.decimal k)
            in  getArea aid v
          )
          areas

        subAreas' = V.fromList $ H.elems $ H.mapWithKey
          (\k v ->
            let aid = (fst <$> fromRight (error "Invalid k") $ T.decimal k)
            in  getSubArea aid v
          )
          subAreas
    pure (maps', superAreas', areas', subAreas')

