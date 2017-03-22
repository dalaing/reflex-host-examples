{-
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
module Host6 (
    go6
  , Input(..)
  , Output(..)
  , SampleApp6
  ) where

import Data.Maybe (isJust)
import Control.Monad (unless, void, forever)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Identity (Identity(..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Ref
import Control.Monad.Primitive (PrimMonad)
import Data.IORef (readIORef)
import System.IO

import Data.Dependent.Sum

import Reflex
import Reflex.Host.Class
import Reflex.PerformEvent.Base

data Input t = Input {
    ieOpen :: Event t ()
  , ieRead :: Event t String
  }

data Output t = Output {
    oeWrite :: Event t String
  , oeQuit  :: Event t ()
  }

type SampleApp6 t m = (Reflex t, MonadHold t m, MonadFix m)
                  => Input t
                  -> m (Output t)

type SampleApp6IO t m = ( Reflex t
                      , MonadHold t m
                      , MonadFix m
                      , MonadRef m
                      , Ref m ~ Ref IO
                      , ReflexHost t
                      , MonadRef (HostFrame t)
                      , Ref (HostFrame t) ~ Ref IO
                      , MonadIO (HostFrame t)
                      , PrimMonad (HostFrame t)
                      )
                  => Output t
                  -> PerformEventT t m (Event t ())

host :: (forall t m. SampleApp6 t m)
     -> (forall t m. SampleApp6IO t m)
     -> IO ()
host myGuest myGuestIO =
  runSpiderHost $ do
    (eOpen, eOpenTriggerRef) <- newEventWithTriggerRef
    (eRead, eReadTriggerRef) <- newEventWithTriggerRef

    out <- runHostFrame $ myGuest $ Input eOpen eRead
    (eQuit, FireCommand fire) <- hostPerformEventT $ myGuestIO out

    hQuit  <- subscribeEvent eQuit

    let
      readPhase =
        readEvent hQuit >>= sequence

      loop = do
        input <- liftIO getLine

        mEReadTrigger <- liftIO $ readIORef eReadTriggerRef
        mQuit <- case mEReadTrigger of
          Nothing ->
            return []
          Just eTrigger ->
            fire [eTrigger :=> Identity input] readPhase

        let quit = any isJust mQuit

        unless quit
          loop

    mEOpenTrigger <- liftIO $ readIORef eOpenTriggerRef
    mQuit <- case mEOpenTrigger of
      Nothing ->
        return []
      Just eTrigger ->
        fire [eTrigger :=> Identity ()] readPhase
    let quit = any isJust mQuit

    unless quit
      loop

guest :: SampleApp6 t m
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

guestIO :: SampleApp6IO t m
guestIO (Output eWrite eQuit) = do
  performEvent_ $ (\x -> liftIO . putStrLn $ "> " ++ x) <$> eWrite
  return eQuit

go6 :: IO ()
go6 = do
  hSetBuffering stdin LineBuffering
  host guest guestIO
