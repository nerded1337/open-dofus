-- Spell.hs ---

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

module OpenDofus.Database.SWF.Spell
  ( loadSpells
  )
where

import           Data.Aeson
import           Data.Either
import qualified Data.HashMap.Strict           as H
import           Data.Maybe
import qualified Data.Text                     as T
import qualified Data.Text.Read                as T
import qualified Data.Vector                   as V
import           Database.Beam
import           OpenDofus.Data.Constructible
import           OpenDofus.Database.Game.Effect
import           OpenDofus.Database.Game.Spell
import qualified OpenDofus.Database.SWF.Reader as SWF
import           OpenDofus.Database.Types
import           OpenDofus.Prelude

loadSpells :: FilePath -> IO (H.HashMap T.Text (Spell, [SpellLevel]))
loadSpells filePath = SWF.loadData filePath $ \obj -> do
  let (Object spells) = fromMaybe (error "Impossible") $ H.lookup "S" obj
  pure $ H.mapWithKey
    (\k v -> getSpell (fst $ fromRight (error "Invalid k") $ T.decimal k) v)
    spells

getSpellLevel :: Spell -> Int -> Value -> Maybe SpellLevel
getSpellLevel spell spellLevel (Array level) =
  let zones = SWF.unsafeString "zones" $ V.unsafeIndex level 15
      getEffects (Array effects) = fst $ V.foldr'
        (\e (es, z) ->
          let (zone, z') = zoneFromPattern z in (getEffect zone e :- es, z')
        )
        (mempty, zones)
        effects
      getEffects _ = V.empty
      getEffect zone (Array effect) = SpellLevelEffect
        (SWF.unsafeInt "effectType" $ V.unsafeIndex effect 0)
        (SWF.safeInt $ V.unsafeIndex effect 1)
        (SWF.safeInt $ V.unsafeIndex effect 2)
        (SWF.safeInt $ V.unsafeIndex effect 3)
        (SWF.unsafeInt "effectRemainingTurn" $ V.unsafeIndex effect 4)
        (SWF.unsafeInt "effectProbability" $ V.unsafeIndex effect 5)
        (SWF.unsafeString "effectParam4" <$> effect V.!? 6)
        zone
      getEffect _ x = error $ "Unhandled effect type: " <> show x
      indexInt s i = SWF.unsafeInt s $ V.unsafeIndex level i
      indexBool s i = SWF.unsafeBool s $ V.unsafeIndex level i
  in  Just $ SpellLevel
        (pk spell)
        spellLevel
        (PgArray $ getEffects $ V.unsafeIndex level 0)
        (PgArray $ getEffects $ V.unsafeIndex level 1)
        (indexInt "apCost" 2)
        (indexInt "rangeMin" 3)
        (indexInt "rangeMax" 4)
        (indexInt "criticalHit" 5)
        (indexInt "criticalFailure" 6)
        (indexBool "lineOnly" 7)
        (indexBool "lineOfSight" 8)
        (indexBool "freeCell" 9)
        (indexBool "canBoostRange" 10)
        (indexInt "classId" 11)
        (indexInt "launchCountByTurn" 12)
        (indexInt "launchCountByPlayerTurn" 13)
        (indexInt "delayBetweenLaunch" 14)
        (PgArray $ SWF.unsafeInt "requiredStates" <$> SWF.unsafeArray
          "requiredStates"
          (V.unsafeIndex level 16)
        )
        (PgArray $ SWF.unsafeInt "forbiddenStates" <$> SWF.unsafeArray
          "forbiddenStates"
          (V.unsafeIndex level 17)
        )
        (indexInt "minPlayerLevel" 18)
        (indexBool "criticalFailureEndsTurn" 19)
getSpellLevel _ _ _ = Nothing

getSpell :: Int -> Value -> (Spell, [SpellLevel])
getSpell sid (Object content) =
  let
    spell = Spell
      (SpellId sid)
      (SWF.unsafeString "name" $ H.lookupDefault (String "") "n" content)
      (SWF.unsafeString "description" $ H.lookupDefault (String "") "d" content)
    makeLevel l = getSpellLevel spell l
      $ H.lookupDefault (Object mempty) ("l" <> T.pack (show l)) content
    levels = catMaybes
      [ makeLevel 1
      , makeLevel 2
      , makeLevel 3
      , makeLevel 4
      , makeLevel 5
      , makeLevel 6
      ]
  in
    (spell, levels)

getSpell sid x = error $ "Unhandled spell type: " <> show sid <> ", " <> show x
