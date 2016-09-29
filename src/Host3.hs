{-
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE RankNTypes #-}
module Host3 (
    go3
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

data Input t = Input {
    ieOpen :: Event t ()
  , ieRead :: Event t String
  }

data Output t = Output {
    oeWrite :: Event t String
  , oeQuit  :: Event t ()
  }

type SampleApp3 t m = (Reflex t, MonadHold t m, MonadFix m)
                  => Input t
                  -> m (Output t) 

host :: (forall t m. SampleApp3 t m)
     -> IO ()
host myGuest = do

  runSpiderHost $ do
    (eOpen, eOpenTriggerRef) <- newEventWithTriggerRef
    (eRead, eReadTriggerRef) <- newEventWithTriggerRef

    Output eWrite eQuit <- runHostFrame $ myGuest $ Input eOpen eRead

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
          Nothing -> do
            return (Nothing, Nothing)
          Just eTrigger -> do
            fireEventsAndRead [eTrigger :=> Identity input] readPhase

        quit <- handleOutputs mWrite mQuit

        unless quit
          loop

    mEOpenTrigger <- liftIO $ readIORef eOpenTriggerRef
    (mWrite, mQuit) <- case mEOpenTrigger of
      Nothing -> do
        return (Nothing, Nothing)
      Just eTrigger -> do
        fireEventsAndRead [eTrigger :=> Identity ()] readPhase
    quit <- handleOutputs mWrite mQuit

    unless quit
      loop

guest :: SampleApp3 t m
guest (Input eOpen eRead) = do
  let
    eMessage =       ffilter (/= "/quit") eRead
    eQuit    = () <$ ffilter (== "/quit") eRead
    eWrite   = leftmost [
        "Hi"  <$ eOpen
      ,          eMessage
      , "Bye" <$ eQuit
      ]
  return $ Output eWrite eQuit

go3 :: IO ()
go3 = do
  hSetBuffering stdin LineBuffering
  host guest
