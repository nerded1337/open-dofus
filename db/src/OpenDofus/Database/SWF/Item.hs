-- Item.hs ---

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

module OpenDofus.Database.SWF.Item
  ( loadItems
  ) where

import           Data.Aeson
import           Data.Either
import qualified Data.HashMap.Strict           as H
import           Data.Maybe
import qualified Data.Text                     as T
import qualified Data.Text.Read                as T
import qualified Data.Vector                   as V

import           OpenDofus.Data.Constructible
import           OpenDofus.Database.Game.Effect
import           OpenDofus.Database.Game.Item
import qualified OpenDofus.Database.SWF.Reader as SWF
import           OpenDofus.Prelude

getType :: Int -> Value -> ItemType
getType tid (Object itemType) =
  let idx i = fromMaybe (error $ show i) $ H.lookup i itemType
   in ItemType
        (ItemTypeId tid)
        (ItemSuperTypePK $ ItemSuperTypeId $ SWF.unsafeInt "superType" $ idx "t")
        (SWF.unsafeString "name" $ idx "n")
        (fst . zoneFromPattern . SWF.unsafeString "effectZone" <$>
         H.lookup "z" itemType)

getType x y = error $ "Unhandled type: " <> show x <> ", " <> show y

getSlots :: Int -> Value -> V.Vector ItemSlot
getSlots sid (Array slots) =
  ItemSlot (ItemSuperTypePK $ ItemSuperTypeId sid) . ItemSlotId . SWF.unsafeInt "slotId" <$> slots
getSlots x y = error $ "Unhandled slot: " <> show x <> ", " <> show y

getItem :: Int -> Maybe T.Text -> Value -> Item
getItem iid stats (Object item) =
  let uIndex i =
        fromMaybe (error $ "index not found: " <> show i) $ H.lookup i item
      getWeaponInfos (Array arr) =
        let int x = SWF.unsafeInt "weaponInfos" $ V.unsafeIndex arr x
            boolean x = SWF.unsafeBool "weaponInfos" $ V.unsafeIndex arr x
         in WeaponInfos
              (int 0)
              (int 1)
              (int 2)
              (int 3)
              (int 4)
              (int 5)
              (boolean 6)
              (boolean 7)
      getWeaponInfos x = error $ "Unhandled weapon infos: " <> show x
   in Item
        (ItemId iid)
        (ItemTypePK $ ItemTypeId $ SWF.unsafeInt "type" $ uIndex "t")
        (SWF.unsafeString "name" $ uIndex "n")
        (SWF.unsafeString "description" $ uIndex "d")
        (SWF.unsafeInt "gfx" $ uIndex "g")
        (SWF.unsafeInt "level" $ uIndex "l")
        (SWF.unsafeInt "weight" $ uIndex "w")
        (SWF.unsafeInt "price" $ uIndex "p")
        (SWF.unsafeString "conditions" <$> H.lookup "c" item)
        (fromMaybe False $ SWF.unsafeBool "isCursed" <$> H.lookup "m" item)
        (fromMaybe False $ SWF.unsafeBool "isEnhanceable" <$> H.lookup "fm" item)
        (fromMaybe False $ SWF.unsafeBool "needsTwoHands" <$> H.lookup "tw" item)
        (fromMaybe False $ SWF.unsafeBool "isEthereal" <$> H.lookup "et" item)
        (fromMaybe False $ SWF.unsafeBool "isHidden" <$> H.lookup "h" item)
        (fromMaybe False $ SWF.unsafeBool "isUsable" <$> H.lookup "u" item)
        (fromMaybe False $ SWF.unsafeBool "isTargetable" <$> H.lookup "ut" item)
        (SWF.unsafeInt "animation" <$> H.lookup "an" item)
        (getWeaponInfos <$> H.lookup "e" item)
        stats
getItem x _ i = error $ "Unhandled item: " <> show x <> ", " <> show i

loadItems ::
     FilePath
  -> FilePath
  -> IO ( V.Vector ItemSuperType
        , V.Vector ItemSlot
        , V.Vector ItemType
        , V.Vector Item)
loadItems itemsPath itemstatsPath = do
  SWF.loadData itemstatsPath $ \statsObj -> do
    let (Array stats) = fromMaybe (error "ISTA") $ H.lookup "ISTA" statsObj
    SWF.loadData itemsPath $ \obj -> do
      let (Object objects) = fromMaybe (error "I") $ H.lookup "I" obj
          (Object superTypes) = fromMaybe (error "st") $ H.lookup "st" objects
          (Object types) = fromMaybe (error "t") $ H.lookup "t" objects
          (Array slots) = fromMaybe (error "ss") $ H.lookup "ss" objects
          (Object items) = fromMaybe (error "u") $ H.lookup "u" objects
          superTypes' =
            V.fromList $
            H.elems $
            H.mapWithKey
              (\k _ ->
                 let stid = (fst <$> fromRight (error "Invalid k") $ T.decimal k)
                 in ItemSuperType stid)
              superTypes
          slots' =
            fold $ V.ifoldr' (\i v vs -> getSlots (i + 1) v :- vs) V.empty slots
          types' =
            V.fromList $
            H.elems $
            H.mapWithKey
              (\k v ->
                 let tid = (fst <$> fromRight (error "Invalid k") $ T.decimal k)
                  in getType tid v)
              types
          items' =
            V.fromList $
            H.elems $
            H.mapWithKey
              (\k v ->
                 let iid = (fst <$> fromRight (error "Invalid k") $ T.decimal k)
                  in getItem
                       iid
                       (SWF.unsafeString "stats" <$> stats V.!? iid)
                       v)
              items
      pure (superTypes', slots', types', items')
