{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

-- Skill.hs ---

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

module OpenDofus.Database.SWF.Skill
  ( loadSkills,
  )
where

import Data.Aeson
import Data.Either
import qualified Data.HashMap.Strict as H
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Read as T
import qualified Data.Vector as V
import OpenDofus.Database.Game.InteractiveObject
import OpenDofus.Database.Game.Item
import OpenDofus.Database.Game.Job
import OpenDofus.Database.Game.Skill
import qualified OpenDofus.Database.SWF.Reader as SWF
import OpenDofus.Prelude

loadSkills :: FilePath -> IO [(Skill, V.Vector SkillCraft)]
loadSkills fp = SWF.loadData fp $ \obj -> do
  let (Object sk) = fromMaybe (error "SK") $ H.lookup "SK" obj
  pure $
    H.elems $
      H.mapWithKey
        ( \k v ->
            let i = (fst <$> fromRight (error "Invalid k") $ T.decimal k)
             in getSkill i v
        )
        sk

getSkill :: Word32 -> Value -> (Skill, V.Vector SkillCraft)
getSkill skid (Object b) =
  let int i = SWF.unsafeInt i $ fromMaybe (error i) $ H.lookup (T.pack i) b
      str i = SWF.unsafeString i $ fromMaybe (error i) $ H.lookup (T.pack i) b
      getCraft iid = SkillCraft (SkillPK $ SkillId skid) (ItemPK $ ItemId iid)
   in ( Skill
          (SkillId skid)
          (str "d")
          (JobPK $ JobId $ int "j")
          (InteractiveObjectPK $ InteractiveObjectId $ int "io")
          (ItemPK $ ItemId . SWF.unsafeInt "i" <$> H.lookup (T.pack "i") b)
          (SWF.unsafeString "c" <$> H.lookup (T.pack "c") b),
        getCraft . SWF.unsafeInt "cl"
          <$> SWF.unsafeArray
            "cl"
            (fromMaybe (Array mempty) $ H.lookup "cl" b)
      )
getSkill _ _ = error "impossible"
