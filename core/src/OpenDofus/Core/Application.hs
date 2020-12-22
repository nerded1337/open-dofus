-- Application.hs ---

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

module OpenDofus.Core.Application
  ( runApp,
    debug,
    info,
    warn,
    fatal,
    critical,
    debugShow,
    infoShow,
    warnShow,
    fatalShow,
    criticalShow,
  )
where

import qualified Data.Text as T
import OpenDofus.Prelude
import qualified Z.Data.Builder.Base as Z
import qualified Z.IO.Logger as Z

runApp :: MonadIO m => IO () -> m ()
runApp = liftIO . Z.withDefaultLogger
{-# INLINE runApp #-}

debug :: HasCallStack => MonadIO m => T.Text -> m ()
debug = liftIO . Z.debug . Z.string7 . T.unpack
{-# INLINE debug #-}

info :: (HasCallStack, MonadIO m) => T.Text -> m ()
info = liftIO . Z.info . Z.string7 . T.unpack
{-# INLINE info #-}

warn :: (HasCallStack, MonadIO m) => T.Text -> m ()
warn = liftIO . Z.warning . Z.string7 . T.unpack
{-# INLINE warn #-}

fatal :: (HasCallStack, MonadIO m) => T.Text -> m ()
fatal = liftIO . Z.fatal . Z.string7 . T.unpack
{-# INLINE fatal #-}

critical :: (HasCallStack, MonadIO m) => T.Text -> m ()
critical = liftIO . Z.critical . Z.string7 . T.unpack
{-# INLINE critical #-}

debugShow :: (HasCallStack, MonadIO m) => Show a => a -> m ()
debugShow = liftIO . Z.debug . Z.string7 . show
{-# INLINE debugShow #-}

infoShow :: (HasCallStack, MonadIO m) => Show a => a -> m ()
infoShow = liftIO . Z.info . Z.string7 . show
{-# INLINE infoShow #-}

warnShow :: (HasCallStack, MonadIO m) => Show a => a -> m ()
warnShow = liftIO . Z.warning . Z.string7 . show
{-# INLINE warnShow #-}

fatalShow :: (HasCallStack, MonadIO m) => Show a => a -> m ()
fatalShow = liftIO . Z.fatal . Z.string7 . show
{-# INLINE fatalShow #-}

criticalShow :: (HasCallStack, MonadIO m) => Show a => a -> m ()
criticalShow = liftIO . Z.critical . Z.string7 . show
{-# INLINE criticalShow #-}
