-- Job.hs ---

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

{-# LANGUAGE OverloadedStrings #-}

module OpenDofus.Database.SWF.Job
  ( loadJobs
  )
where


import           Data.Aeson
import           Data.Either
import qualified Data.HashMap.Strict           as H
import           Data.Maybe
import qualified Data.Text                     as T
import qualified Data.Text.Read                as T
import           OpenDofus.Database.Game.Job
import qualified OpenDofus.Database.SWF.Reader as SWF
import           OpenDofus.Prelude

loadJobs :: FilePath -> IO [Job]
loadJobs fp = SWF.loadData fp $ \obj -> do
  let (Object j) = fromMaybe (error "J") $ H.lookup "J" obj
  pure $ H.elems $ H.mapWithKey
    (\k v ->
      let i = (fst <$> fromRight (error "Invalid k") $ T.decimal k)
      in  getJob i v
    )
    j

getJob :: Int -> Value -> Job
getJob ioid (Object b) =
  let int i = SWF.unsafeInt i $ fromMaybe (error i) $ H.lookup (T.pack i) b
      str i = SWF.unsafeString i $ fromMaybe (error i) $ H.lookup (T.pack i) b
      getJobId 0 = Nothing
      getJobId x = Just x
  in  Job (JobId ioid) (JobPK $ JobId <$> getJobId (int "s")) (str "n")
getJob _ _ = error "impossible"
