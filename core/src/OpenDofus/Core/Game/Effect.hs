{-# LANGUAGE TemplateHaskell #-}

-- Effect.hs ---

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

module OpenDofus.Core.Game.Effect
  ( EffectDice (..),
    HasEffectDice (..),
    EffectInstance (..),
    HasEffectInstance (..),
    parseEffect,
    parseDice,
  )
where

import Data.Attoparsec.Text
import OpenDofus.Prelude

data EffectDice = EffectDice
  { _effectDiceMultiplier :: {-# UNPACK #-} !Int32,
    _effectDiceMaximum :: {-# UNPACK #-} !Int32,
    _effectDiceConstant :: {-# UNPACK #-} !Int32
  }

makeClassy ''EffectDice

data EffectInstance = EffectInstance
  { _effectInstanceId :: {-# UNPACK #-} !Int32,
    _effectInstanceParam1 :: {-# UNPACK #-} !Int32,
    _effectInstanceParam2 :: {-# UNPACK #-} !Int32,
    _effectInstanceParam3 :: {-# UNPACK #-} !Int32,
    _effectInstanceDice :: {-# UNPACK #-} !EffectDice
  }

makeClassy ''EffectInstance

parseDice :: Parser EffectDice
parseDice =
  EffectDice
    <$> signed decimal
    <*> (char 'd' *> signed decimal)
    <*> (char '+' *> signed decimal)
{-# INLINE parseDice #-}

parseEffect :: Parser EffectInstance
parseEffect = EffectInstance <$> hex <*> hex <*> hex <*> hex <*> parseDice
  where
    hex = char '#' *> hexadecimal
{-# INLINE parseEffect #-}
