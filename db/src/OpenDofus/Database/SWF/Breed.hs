-- Breed.hs ---

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

{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

module OpenDofus.Database.SWF.Breed
  ( loadBreeds
  )
where

import           Data.Aeson
import           Data.Either
import qualified Data.HashMap.Strict           as H
import           Data.Maybe
import qualified Data.Text                     as T
import qualified Data.Text.Read                as T
import qualified Data.Vector                   as V
import           OpenDofus.Database.Game.Breed
import           OpenDofus.Database.Game.Spell
import qualified OpenDofus.Database.SWF.Reader as SWF
import           OpenDofus.Prelude

loadBreeds
  :: FilePath
  -> IO [(Breed, V.Vector BreedCharacteristicCost, V.Vector BreedSpell)]
loadBreeds fp = SWF.loadData fp $ \obj -> do
  let (Object breeds) = fromMaybe (error "G") $ H.lookup "G" obj
  pure $ H.elems $ H.mapWithKey
    (\k v ->
      let bid = (fst <$> fromRight (error "Invalid k") $ T.decimal k)
      in  getBreed bid v
    )
    breeds

getBreed
  :: Int
  -> Value
  -> (Breed, V.Vector BreedCharacteristicCost, V.Vector BreedSpell)
getBreed bid (Object b) =
  let str i = SWF.unsafeString i $ fromMaybe (error i) $ H.lookup (T.pack i) b
      arr i = SWF.unsafeArray i $ fromMaybe (error i) $ H.lookup (T.pack i) b
      getStep element (Array v) = BreedCharacteristicCost
        (BreedPK (BreedId bid))
        element
        (SWF.unsafeInt "costFloor" $ V.unsafeIndex v 0)
        (SWF.unsafeInt "costValue" $ V.unsafeIndex v 1)
        (maybe 1 (SWF.unsafeInt "costBonus") $ v V.!? 2)
      getStep _ x = error $ "Unhandled breed cost step: " <> show x
      getCharacteristic i = getStep i <$> arr ("b" <> show i)
      breed = Breed (BreedId bid) (str "sn") (str "ln") (str "sd") (str "d")
      spells =
          BreedSpell (BreedPK (BreedId bid))
            .   SpellPK
            .   SpellId
            .   SWF.unsafeInt "spell"
            <$> arr "s"
      costs = getCharacteristic =<< V.fromList [10, 11, 12, 13, 14, 15]
  in  (breed, costs, spells)

getBreed _ x = error $ "Unhandled breed: " <> show x
