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

{-# LANGUAGE OverloadedStrings #-}

module OpenDofus.Database.SWF.Effect
  ( loadEffects
  )
where

import           Data.Aeson
import           Data.Either
import qualified Data.HashMap.Strict           as H
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text                     as T
import qualified Data.Text.Read                as T
import           OpenDofus.Data.Constructible
import           OpenDofus.Database.Game.Effect
import qualified OpenDofus.Database.SWF.Reader as SWF
import           OpenDofus.Prelude       hiding ( First
                                                , getFirst
                                                )

loadEffects :: FilePath -> IO [Effect]
loadEffects filePath = SWF.loadData filePath $ \obj -> do
  let (Object effects) = fromMaybe (error "E") $ H.lookup "E" obj
      (Object edmg   ) = fromMaybe (error "EDMG") $ H.lookup "EDMG" obj
      (Object ehel   ) = fromMaybe (error "EHEL") $ H.lookup "EHEL" obj
      etype eid = fromMaybe Special $ getFirst $ foldMap
        First
        [ Damage <$ H.lookup (T.pack $ show eid) edmg
        , Heal <$ H.lookup (T.pack $ show eid) ehel
        ]
  pure $ H.elems $ H.mapWithKey
    (\k v ->
      let eid = (fst <$> fromRight (error "Invalid k") $ T.decimal k)
      in  getEffect eid (etype eid) v
    )
    effects

getEffect :: Int -> EffectType -> Value -> Effect
getEffect eid etype (Object effect) =
  let
    getOperator (String ('+' :- _)) = Just Plus
    getOperator (String ('-' :- _)) = Just Minus
    getOperator (String ('/' :- _)) = Just Divide
    getOperator (String (x :- _)) =
      error $ "Unknown effect operator: " <> show x
    getOperator _ = Nothing
  in
    Effect
      (EffectId eid)
      etype
      ( SWF.unsafeString "description"
      $ fromMaybe (error "description")
      $ H.lookup "d" effect
      )
      (maybe False (SWF.unsafeBool "hasJet") $ H.lookup "j" effect)
      (maybe False (SWF.unsafeBool "hasJet") $ H.lookup "t" effect)
      (getOperator =<< H.lookup "o" effect)
      (maybe 0 (SWF.unsafeInt "characteristic") $ H.lookup "c" effect)

getEffect _ _ x = error $ "Unhandled effect: " <> show x
