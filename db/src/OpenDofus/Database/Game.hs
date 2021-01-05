{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

-- Game.hs ---

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

module OpenDofus.Database.Game
  ( module X,
    GameDb (..),
    GameQuery (..),
    GameDbConn (..),
    breed,
    breedCharacteristicCost,
    breedSpell,
    effect,
    spell,
    spellLevel,
    itemSuperType,
    itemType,
    itemSlot,
    item,
    mapSuperArea,
    mapArea,
    mapSubArea,
    mapSubAreaNeighbour,
    map,
    job,
    skill,
    skillCraft,
    interactiveObject,
    interactiveObjectGfx,
    character,
    characterPosition,
    characterLook,
    characterCharacteristic,
    characterSpell,
    gameDbChecked,
    gameDb,
    gameDbMigrations,
  )
where

import Database.Beam
import Database.Beam.Migrate
import Database.Beam.Postgres
import OpenDofus.Database.Auth.Account as X
import OpenDofus.Database.Game.Breed as X
import OpenDofus.Database.Game.Character as X
import OpenDofus.Database.Game.Effect as X
import OpenDofus.Database.Game.InteractiveObject as X
import OpenDofus.Database.Game.Item as X
import OpenDofus.Database.Game.Job as X
import OpenDofus.Database.Game.Map as X
import OpenDofus.Database.Game.Skill as X
import OpenDofus.Database.Game.Spell as X
import OpenDofus.Database.Types as X
import OpenDofus.Prelude

newtype GameQuery a = GameQuery
  { unGameQuery :: Pg a
  }
  deriving newtype (Functor, Applicative, Monad, IsPg)

newtype GameDbConn = GameDbConn
  { unGameDbConn :: Connection
  }

instance HasConnType (GameDb f) where
  type ConnTypeOf (GameDb f) = GameDbConn

instance HasQueryType GameDbConn where
  type QueryTypeOf GameDbConn = GameQuery

data GameDb f = GameDb
  { _dbBreed :: !(f (TableEntity BreedT)),
    _dbBreedCharacteristicCost :: !(f (TableEntity BreedCharacteristicCostT)),
    _dbBreedSpell :: !(f (TableEntity BreedSpellT)),
    _dbEffect :: !(f (TableEntity EffectT)),
    _dbSpell :: !(f (TableEntity SpellT)),
    _dbSpellLEvel :: !(f (TableEntity SpellLevelT)),
    _dbItemSuperType :: !(f (TableEntity ItemSuperTypeT)),
    _dbItemType :: !(f (TableEntity ItemTypeT)),
    _dbItemSlot :: !(f (TableEntity ItemSlotT)),
    _dbItem :: !(f (TableEntity ItemT)),
    _dbMapSuperArea :: !(f (TableEntity MapSuperAreaT)),
    _dbMapArea :: !(f (TableEntity MapAreaT)),
    _dbMapSubArea :: !(f (TableEntity MapSubAreaT)),
    _dbMapSubAreaNeighbour :: !(f (TableEntity MapSubAreaNeighbourT)),
    _dbMap :: !(f (TableEntity MapT)),
    _dbJob :: !(f (TableEntity JobT)),
    _dbSkill :: !(f (TableEntity SkillT)),
    _dbSkillCraft :: !(f (TableEntity SkillCraftT)),
    _dbInteractiveObject :: !(f (TableEntity InteractiveObjectT)),
    _dbInteractiveObjectGfx :: !(f (TableEntity InteractiveObjectGfxT)),
    _dbCharacter :: !(f (TableEntity CharacterT)),
    _dbCharacterPosition :: !(f (TableEntity CharacterPositionT)),
    _dbCharacterLook :: !(f (TableEntity CharacterLookT)),
    _dbCharacterCharacteristic :: !(f (TableEntity CharacterCharacteristicT)),
    _dbCharacterSpell :: !(f (TableEntity CharacterSpellT))
  }
  deriving stock (Generic)
  deriving anyclass (Database Postgres)

GameDb
  (TableLens breed)
  (TableLens breedCharacteristicCost)
  (TableLens breedSpell)
  (TableLens effect)
  (TableLens spell)
  (TableLens spellLevel)
  (TableLens itemSuperType)
  (TableLens itemType)
  (TableLens itemSlot)
  (TableLens item)
  (TableLens mapSuperArea)
  (TableLens mapArea)
  (TableLens mapSubArea)
  (TableLens mapSubAreaNeighbour)
  (TableLens map)
  (TableLens job)
  (TableLens skill)
  (TableLens skillCraft)
  (TableLens interactiveObject)
  (TableLens interactiveObjectGfx)
  (TableLens character)
  (TableLens characterPosition)
  (TableLens characterLook)
  (TableLens characterCharacteristic)
  (TableLens characterSpell) =
    dbLenses

gameDbChecked :: CheckedDatabaseSettings Postgres GameDb
gameDbChecked = evaluateDatabase gameDbMigrations

gameDb :: DatabaseSettings Postgres GameDb
gameDb = unCheckDatabase gameDbChecked

gameDbMigrations ::
  MigrationSteps Postgres () (CheckedDatabaseSettings Postgres GameDb)
gameDbMigrations = migrationStep "Initial commit" initialMigration

initialMigration ::
  () -> Migration Postgres (CheckedDatabaseSettings Postgres GameDb)
initialMigration _ =
  GameDb
    <$> createTable
      "breed"
      ( Breed
          (field "id" int notNull)
          (field "small_name" (varchar (Just 20)) notNull)
          (field "long_name" (varchar (Just 50)) notNull)
          (field "small_description" (varchar (Just 200)) notNull)
          (field "description" (varchar Nothing) notNull)
      )
    <*> createTable
      "breed_characteristic_cost"
      ( BreedCharacteristicCost
          (BreedPK $ field "breed_id" int notNull)
          (field "element" int notNull)
          (field "floor" int notNull)
          (field "value" int notNull)
          (field "bonus" int notNull)
      )
    <*> createTable
      "breed_spell"
      ( BreedSpell
          (BreedPK $ field "breed_id" int notNull)
          (SpellPK $ field "spell_id" int notNull)
      )
    <*> createTable
      "effect"
      ( Effect
          (field "id" int notNull)
          (field "type" enumType notNull)
          (field "description" (varchar Nothing) notNull)
          (field "has_jet" boolean notNull)
          (field "show_in_tooltip" boolean notNull)
          (field "operator" (maybeType enumType))
          (field "characteristic" enumType)
      )
    <*> createTable
      "spell"
      ( Spell
          (field "id" int notNull)
          (field "name" (varchar (Just 100)) notNull)
          (field "description" (varchar Nothing) notNull)
      )
    <*> createTable
      "spell_level"
      ( SpellLevel
          (SpellPK $ field "spell_id" int notNull)
          (field "level" int notNull)
          (field "normal_effects" binaryFieldType notNull)
          (field "critical_effects" binaryFieldType notNull)
          (field "ap_cost" int notNull)
          (field "range_min" int notNull)
          (field "range_max" int notNull)
          (field "critical_hit" int notNull)
          (field "critical_failure" int notNull)
          (field "line_only" boolean notNull)
          (field "line_of_sight" boolean notNull)
          (field "free_ceel" boolean notNull)
          (field "can_boost_range" boolean notNull)
          (field "class_id" int notNull)
          (field "launch_count_by_turn" int notNull)
          (field "launch_count_by_player_turn" int notNull)
          (field "delay_between_launch" int notNull)
          (field "required_states" binaryFieldType notNull)
          (field "forbidden_states" binaryFieldType notNull)
          (field "min_player_level" int notNull)
          (field "critical_failure_ends_turn" boolean notNull)
      )
    <*> createTable "item_super_type" (ItemSuperType (field "id" int notNull))
    <*> createTable
      "item_type"
      ( ItemType
          (field "id" int notNull)
          (ItemSuperTypePK $ field "super_type_id" int notNull)
          (field "name" (varchar (Just 50)) notNull)
          (field "effectZone" (maybeType binaryFieldType))
      )
    <*> createTable
      "item_slot"
      ( ItemSlot
          (ItemSuperTypePK $ field "super_type_id" int notNull)
          (field "id" int notNull)
      )
    <*> createTable
      "item"
      ( Item
          (field "id" int notNull)
          (ItemTypePK $ field "type_id" int notNull)
          (field "name" (varchar (Just 100)) notNull)
          (field "description" (varchar Nothing) notNull)
          (field "gfx" int notNull)
          (field "level" int notNull)
          (field "weight" int notNull)
          (field "price" int notNull)
          (field "conditions" (maybeType (varchar (Just 200))))
          (field "is_cursed" boolean notNull)
          (field "is_enhanceable" boolean notNull)
          (field "needs_two_hands" boolean notNull)
          (field "is_ethereal" boolean notNull)
          (field "is_hidden" boolean notNull)
          (field "is_usable" boolean notNull)
          (field "is_targetable" boolean notNull)
          (field "animation" (maybeType int))
          (field "weapon_infos" (maybeType binaryFieldType))
          (field "statistics" (maybeType (varchar Nothing)))
      )
    <*> createTable
      "super_area"
      ( MapSuperArea
          (field "id" int notNull)
          (field "name" (varchar (Just 100)) notNull)
      )
    <*> createTable
      "area"
      ( MapArea
          (field "id" int notNull)
          (field "name" (varchar (Just 100)) notNull)
          (MapSuperAreaPK $ field "super_area_id" int notNull)
      )
    <*> createTable
      "sub_area"
      ( MapSubArea
          (field "id" int notNull)
          (field "name" (varchar (Just 100)) notNull)
          (MapAreaPK $ field "area_id" int notNull)
          (field "musics" binaryFieldType notNull)
      )
    <*> createTable
      "sub_area_neighbour"
      ( MapSubAreaNeighbour
          (MapSubAreaPK $ field "origin_sub_area_id" int notNull)
          (MapSubAreaPK $ field "destination_sub_area_id" int notNull)
      )
    <*> createTable
      "map"
      ( Map
          (field "id" int notNull)
          (field "date" (coerceType $ varchar (Just 50)) notNull)
          (MapSubAreaPK $ field "sub_area_id" int notNull)
          (field "x" int notNull)
          (field "y" int notNull)
          (field "width" int notNull)
          (field "height" int notNull)
          (field "backgroum_num" int notNull)
          (field "ambiance_id" int notNull)
          (field "is_outdoor" boolean notNull)
          (field "capabilities" int notNull)
          (field "compressed_data" (coerceType $ varchar Nothing) notNull)
          (field "data_key" (coerceType $ maybeType $ varchar Nothing))
      )
    <*> createTable
      "job"
      ( Job
          (field "id" int notNull)
          (JobPK $ field "sub_job_id" (maybeType int))
          (field "name" (varchar $ Just 50) notNull)
      )
    <*> createTable
      "skill"
      ( Skill
          (field "id" int notNull)
          (field "description" (varchar $ Just 50) notNull)
          (JobPK $ field "job_id" int notNull)
          (InteractiveObjectPK $ field "interactive_object_id" int notNull)
          (ItemPK $ field "item_id" (maybeType int))
          (field "criterion" (maybeType $ varchar $ Just 30))
      )
    <*> createTable
      "skill_craft"
      ( SkillCraft
          (SkillPK $ field "skill_id" int notNull)
          (ItemPK $ field "item_id" int notNull)
      )
    <*> createTable
      "interactive_object"
      ( InteractiveObject
          (field "id" int notNull)
          (field "type" enumType notNull)
          (field "name" (varchar $ Just 150) notNull)
      )
    <*> createTable
      "interactive_object_gfx"
      ( InteractiveObjectGfx
          (field "id" int notNull)
          (InteractiveObjectPK $ field "interactive_object_id" int notNull)
      )
    <*> createTable
      "character"
      ( Character
          (field "id" int notNull)
          (field "name" (coerceType (varchar (Just 20))) notNull unique)
          (BreedPK $ field "breed_id" int notNull)
          (AccountPK $ field "account_id" (coerceType uuid) notNull)
          (field "level" int notNull)
          (field "max_level" int notNull)
          (field "experience" int notNull)
      )
    <*> createTable
      "character_position"
      ( CharacterPosition
          (CharacterPK $ field "character_id" int notNull)
          (MapPK $ field "map_id" int notNull)
          (field "cell_id" int notNull)
      )
    <*> createTable
      "character_look"
      ( CharacterLook
          (CharacterPK $ field "character_id" int notNull)
          (field "gfx_id" int notNull)
          (field "gfx_size" int notNull)
          (field "sex" (coerceType boolean) notNull)
          (field "first_color" int notNull)
          (field "second_color" int notNull)
          (field "third_color" int notNull)
      )
    <*> createTable
      "character_characteristic"
      ( CharacterCharacteristic
          (CharacterPK $ field "character_id" int notNull)
          (EffectPK $ field "effect_id" int notNull)
          (field "source" enumType notNull)
          (field "value" int notNull)
      )
    <*> createTable
      "character_spell"
      ( CharacterSpell
          (CharacterPK $ field "character_id" int notNull)
          ( SpellLevelPK
              (SpellPK $ field "spell_id" int notNull)
              (field "spell_level" int notNull)
          )
      )
