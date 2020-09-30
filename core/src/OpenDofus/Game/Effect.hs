-- Effect.hs ---

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

{-# LANGUAGE TemplateHaskell #-}

module OpenDofus.Game.Effect
  ( EffectDice(..)
  , HasEffectDice(..)
  , EffectInstance(..)
  , HasEffectInstance(..)
  , parseEffect
  , parseDice
  )
where

import           Data.Attoparsec.Text
import           OpenDofus.Prelude

data EffectDice =
  EffectDice
    { _effectDiceMultiplier :: {-# UNPACK #-}!Int
    , _effectDiceMaximum    :: {-# UNPACK #-}!Int
    , _effectDiceConstant   :: {-# UNPACK #-}!Int
    }

makeClassy ''EffectDice

data EffectInstance =
  EffectInstance
    { _effectInstanceId     :: {-# UNPACK #-}!Int
    , _effectInstanceParam1 :: {-# UNPACK #-}!Int
    , _effectInstanceParam2 :: {-# UNPACK #-}!Int
    , _effectInstanceParam3 :: {-# UNPACK #-}!Int
    , _effectInstanceDice   :: {-# UNPACK #-}!EffectDice
    }

makeClassy ''EffectInstance

parseDice :: Parser EffectDice
parseDice =
  EffectDice
    <$> signed decimal
    <*> (char 'd' *> signed decimal)
    <*> (char '+' *> signed decimal)

parseEffect :: Parser EffectInstance
parseEffect = EffectInstance <$> hex <*> hex <*> hex <*> hex <*> parseDice
  where hex = char '#' *> hexadecimal
