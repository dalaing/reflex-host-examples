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
module Host5 (
    go5
  ) where

import Data.Maybe (isJust)
import Control.Monad (unless, void, forever)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Identity (Identity(..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Ref
import Data.IORef (readIORef)
import System.IO

import Data.Dependent.Sum

import Reflex
import Reflex.Host.Class
import Reflex.PerformEvent.Base

type SampleApp5 t m = ( Reflex t
                      , MonadHold t m
                      , MonadFix m
                      , MonadRef m
                      , Ref m ~ Ref IO
                      , ReflexHost t
                      , MonadRef (HostFrame t)
                      , Ref (HostFrame t) ~ Ref IO
                      , MonadIO (HostFrame t)
                      )
                  => Event t String
                  -> PostBuildT t (PerformEventT t m) (Event t ())

host :: (forall t m. SampleApp5 t m)
     -> IO ()
host myGuest = do

  runSpiderHost $ do
    (eOpen, eOpenTriggerRef) <- newEventWithTriggerRef
    (eRead, eReadTriggerRef) <- newEventWithTriggerRef

    (eQuit, FireCommand fire) <- hostPerformEventT $ runPostBuildT (myGuest eRead) eOpen
    hQuit  <- subscribeEvent eQuit

    let
      readPhase = do
        mQuit  <- readEvent hQuit >>= sequence
        return mQuit

      handleOutputs mQuit = do
        liftIO . putStrLn $ "mQuit: " ++ show mQuit
        return $ any isJust mQuit

      loop = do
        input <- liftIO getLine

        mEReadTrigger <- liftIO $ readIORef eReadTriggerRef
        mQuit <- case mEReadTrigger of
          Nothing ->
            return []
          Just eTrigger ->
            fire [eTrigger :=> Identity input] readPhase

        quit <- handleOutputs mQuit

        unless quit
          loop

    mEOpenTrigger <- liftIO $ readIORef eOpenTriggerRef
    mQuit <- case mEOpenTrigger of
      Nothing ->
        return []
      Just eTrigger ->
        fire [eTrigger :=> Identity ()] readPhase
    quit <- handleOutputs mQuit

    unless quit
      loop

guest :: SampleApp5 t m
guest eRead = do
  eOpen <- getPostBuild

  let
    eMessage =       ffilter ((/= "/") . take 1) eRead
    eCat     = () <$ ffilter (== "/cat") eRead
    eQuit    = () <$ ffilter (== "/quit") eRead

  eCatOut <- performEvent $ liftIO (readFile "README.md") <$ eCat

  let
    eWrite   = leftmost [
        "Hi"  <$ eOpen
      ,          eMessage
      ,          eCatOut
      , "Bye" <$ eQuit
      ]

  performEvent_ $ (\x -> liftIO . putStrLn $ "> " ++ x) <$> eWrite

  return eQuit

go5 :: IO ()
go5 = do
  hSetBuffering stdin LineBuffering
  host guest
