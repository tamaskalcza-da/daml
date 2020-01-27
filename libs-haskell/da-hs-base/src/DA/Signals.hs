-- Copyright (c) 2020 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE CPP #-}

-- | Module that turns SIGTERM into a UserInterrupt exception in Unix.
module DA.Signals
    ( installSignalHandlers
    , withCloseOnStdin
    ) where

import Control.Exception
import Control.Concurrent.Async
import System.IO
import Control.Monad
import Control.Concurrent

#ifndef mingw32_HOST_OS
import System.Posix.Signals
#endif

-- | Turn SIGTERM into a UserInterrput exception in Unix. Does nothing on Windows.
installSignalHandlers :: IO ()

#ifdef mingw32_HOST_OS
installSignalHandlers = pure ()
#else
installSignalHandlers = do
    mainThread <- myThreadId
    void $ installHandler sigTERM (Catch $ throwTo mainThread UserInterrupt) Nothing
#endif

withCloseOnStdin :: IO a -> IO a
withCloseOnStdin a = do
    mainThread <- myThreadId
    -- On Windows we probably need to close stdin explicitely to make sure that
    -- isEOF does not block forever.
    withAsync (go mainThread) (const $ a `finally` hClose stdin)
  where go mainThread = forever $ do
            b <- isEOF
            when b $ throwTo mainThread UserInterrupt
