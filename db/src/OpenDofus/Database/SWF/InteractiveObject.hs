-- InteractiveObject.hs ---

-- Copyright (C) 2020 Nerd Ed

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

module OpenDofus.Database.SWF.InteractiveObject
  ( loadInteractiveObjects
  )
where

import           Data.Aeson
import           Data.Either
import qualified Data.HashMap.Strict           as H
import           Data.Maybe
import qualified Data.Text                     as T
import qualified Data.Text.Read                as T
import           OpenDofus.Database.Game.InteractiveObject
import qualified OpenDofus.Database.SWF.Reader as SWF
import           OpenDofus.Prelude

loadInteractiveObjects
  :: FilePath -> IO ([InteractiveObject], [InteractiveObjectGfx])
loadInteractiveObjects fp = SWF.loadData fp $ \obj -> do
  let (Object io    ) = fromMaybe (error "IO") $ H.lookup "IO" obj
      (Object gfx   ) = fromMaybe (error "g") $ H.lookup "g" io
      (Object ioDesc) = fromMaybe (error "d") $ H.lookup "d" io
  pure
    ( H.elems $ H.mapWithKey
      (\k v ->
        let i = (fst <$> fromRight (error "Invalid k") $ T.decimal k)
        in  getInteractiveObject i v
      )
      ioDesc
    , H.elems $ H.mapWithKey
      (\k v ->
        let i = (fst <$> fromRight (error "Invalid k") $ T.decimal k)
        in  getInteractiveObjectGfx i v
      )
      gfx
    )

getInteractiveObjectGfx :: Int -> Value -> InteractiveObjectGfx
getInteractiveObjectGfx gfxId ioid = InteractiveObjectGfx
  (InteractiveObjectGfxId gfxId)
  (InteractiveObjectPK $ InteractiveObjectId $ SWF.unsafeInt "ioid" ioid)

getInteractiveObject :: Int -> Value -> InteractiveObject
getInteractiveObject ioid (Object b) =
  let getType :: Int -> InteractiveObjectType
      getType 1  = InteractiveObjectTypeResource
      getType 2  = InteractiveObjectTypeCraftPlan
      getType 3  = InteractiveObjectTypeZaap
      getType 4  = InteractiveObjectTypeFountainOfYouth
      getType 5  = InteractiveObjectTypeDoor
      getType 6  = InteractiveObjectTypeStorage
      getType 7  = InteractiveObjectTypeCookingPot
      getType 10 = InteractiveObjectTypeZaapi
      getType 12 = InteractiveObjectTypeCraftmenList
      getType 13 = InteractiveObjectTypePaddock
      getType 14 = InteractiveObjectTypeLever
      getType 15 = InteractiveObjectTypeClassStatue
      getType _  = error "impossible"
      int i = SWF.unsafeInt i $ fromMaybe (error i) $ H.lookup (T.pack i) b
      str i = SWF.unsafeString i $ fromMaybe (error i) $ H.lookup (T.pack i) b
  in  InteractiveObject (InteractiveObjectId ioid) (getType $ int "t") (str "n")
getInteractiveObject _ _ = error "impossible"
