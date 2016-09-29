{-
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE RankNTypes #-}
module Host4 (
    go4
  ) where

import Data.Maybe (isJust)
import Control.Monad (unless, void, forever)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Identity (Identity(..))
import Control.Monad.IO.Class (liftIO)
import Data.IORef (readIORef)
import System.IO

import Data.Dependent.Sum

import Reflex
import Reflex.Host.Class

data Output t = Output {
    oeWrite :: Event t String
  , oeQuit  :: Event t ()
  }

type SampleApp4 t m = (Reflex t, MonadHold t m, MonadFix m)
                  => Event t String
                  -> PostBuildT t m (Output t)

host :: (forall t m. SampleApp4 t m)
     -> IO ()
host myGuest = do

  runSpiderHost $ do
    (eOpen, eOpenTriggerRef) <- newEventWithTriggerRef
    (eRead, eReadTriggerRef) <- newEventWithTriggerRef

    Output eWrite eQuit <- runHostFrame $ runPostBuildT (myGuest eRead) eOpen

    hWrite <- subscribeEvent eWrite
    hQuit  <- subscribeEvent eQuit

    let
      readPhase = do
        mWrite <- readEvent hWrite >>= sequence
        mQuit  <- readEvent hQuit >>= sequence
        return (mWrite, mQuit)

      handleOutputs mWrite mQuit = do
        case mWrite of
          Nothing -> return ()
          Just w -> liftIO $ putStrLn $ "> " ++ w
        return $ isJust mQuit

      loop = do
        input <- liftIO getLine

        mEReadTrigger <- liftIO $ readIORef eReadTriggerRef
        (mWrite, mQuit) <- case mEReadTrigger of
          Nothing ->
            return (Nothing, Nothing)
          Just eTrigger ->
            fireEventsAndRead [eTrigger :=> Identity input] readPhase

        quit <- handleOutputs mWrite mQuit

        unless quit
          loop

    mEOpenTrigger <- liftIO $ readIORef eOpenTriggerRef
    (mWrite, mQuit) <- case mEOpenTrigger of
      Nothing ->
        return (Nothing, Nothing)
      Just eTrigger ->
        fireEventsAndRead [eTrigger :=> Identity ()] readPhase
    quit <- handleOutputs mWrite mQuit

    unless quit
      loop

guest :: SampleApp4 t m
guest eRead = do
  eOpen <- getPostBuild
  let
    eMessage =       ffilter (/= "/quit") eRead
    eQuit    = () <$ ffilter (== "/quit") eRead
    eWrite   = leftmost [
        "Hi"  <$ eOpen
      ,          eMessage
      , "Bye" <$ eQuit
      ]
  return $ Output eWrite eQuit

go4 :: IO ()
go4 = do
  hSetBuffering stdin LineBuffering
  host guest
