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
{-# LANGUAGE RecursiveDo #-}
module Host6 (
    go6
  , guest
  , interpret
  , mkInterpretable
  , InputCmd(..)
  , OutputCmd(..)
  ) where

import Data.Maybe (isJust)
import Control.Monad (unless, void, forever)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Identity (Identity(..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Ref
import Data.IORef (readIORef)
import System.IO

import Data.GADT.Compare
import Data.Dependent.Sum
import Data.Dependent.Map

import Reflex
import Reflex.Host.Class
import Reflex.PerformEvent.Base

data Input t = Input {
    ieOpen :: Event t ()
  , ieRead :: Event t String
  }

data InputCmd =
    Open
  | Read String
  deriving (Eq, Ord, Show)

data GInputCmd a where
  ICOpen :: GInputCmd ()
  ICRead :: GInputCmd String

liftInputCmd :: InputCmd -> DSum GInputCmd Identity
liftInputCmd Open     = ICOpen :=> Identity ()
liftInputCmd (Read s) = ICRead :=> Identity s

instance GEq GInputCmd where
  geq a b = case (a, b) of
    (ICOpen, ICOpen) -> Just Refl
    (ICRead, ICRead) -> Just Refl
    _ -> Nothing

instance GCompare GInputCmd where
  gcompare a b = case (a, b) of
    (ICOpen, ICOpen) -> GEQ
    (ICOpen, ICRead) -> GLT
    (ICRead, ICOpen) -> GGT
    (ICRead, ICRead) -> GEQ

fanInput :: Reflex t => Event t InputCmd -> Input t
fanInput e =
  let
    es = fan $ (fromList . pure . liftInputCmd) <$> e
  in
    Input (select es ICOpen) (select es ICRead)

data Output t = Output {
    oeWrite :: Event t String
  , oeQuit  :: Event t ()
  }

data OutputCmd =
    Write String
  | Quit
  deriving (Eq, Ord, Show)

data GOutputCmd a where
  OCWrite :: GOutputCmd String
  OCQuit  :: GOutputCmd ()

lowerOutputCmd :: DSum GOutputCmd Identity -> OutputCmd
lowerOutputCmd g = case g of
  OCWrite :=> Identity s -> Write s
  OCQuit :=> Identity () -> Quit

instance GEq GOutputCmd where
  geq a b = case (a, b) of
    (OCWrite, OCWrite) -> Just Refl
    (OCQuit, OCQuit) -> Just Refl
    _ -> Nothing

instance GCompare GOutputCmd where
  gcompare a b = case (a, b) of
    (OCWrite, OCWrite) -> GEQ
    (OCWrite, OCQuit) -> GLT
    (OCQuit, OCWrite) -> GGT
    (OCQuit, OCQuit) -> GEQ

mergeOutput :: Reflex t => Output t -> Event t [OutputCmd]
mergeOutput (Output eWrite eQuit) =
  fmap (fmap lowerOutputCmd . toList) .
  merge $
  fromList [OCWrite :=> eWrite, OCQuit :=> eQuit]

type Interpretable t m a b = (Reflex t, MonadHold t m, MonadFix m)
                           => Event t a
                           -> m (Event t b)

mkInterpretable :: Reflex t => SampleApp6 t m -> Interpretable t m InputCmd [OutputCmd]
mkInterpretable guest = \i -> do
  o <- guest . fanInput $ i
  return $ mergeOutput o

interpret :: (forall t m. Interpretable t m a b) -> [Maybe a] -> IO [Maybe b]
interpret guest inputs =
  runSpiderHost $ do
    (eIn, eInTriggerRef) <- newEventWithTriggerRef
    eOut <- runHostFrame $ guest eIn
    hOut <- subscribeEvent eOut

    let
      handleFrame i = do
        mt <- readRef eInTriggerRef
        case mt of
          Nothing -> return Nothing
          Just t -> fireEventsAndRead (maybe [] (\x -> [t :=> Identity x]) i) $ do
            mOut <- readEvent hOut
            sequence mOut

    traverse handleFrame inputs

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
                      )
                  => Output t
                  -> PerformEventT t m (Event t ())

host :: (forall t m. SampleApp6 t m)
     -> (forall t m. SampleApp6IO t m)
     -> IO ()
host myGuest myGuestIO = do

  runSpiderHost $ do
    (eOpen, eOpenTriggerRef) <- newEventWithTriggerRef
    (eRead, eReadTriggerRef) <- newEventWithTriggerRef

    out <- runHostFrame $ myGuest $ Input eOpen eRead
    (eQuit, FireCommand fire) <- hostPerformEventT $ myGuestIO out

    hQuit  <- subscribeEvent eQuit

    let
      readPhase = do
        mQuit  <- readEvent hQuit >>= sequence
        return mQuit

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
    eMessage =       ffilter ((/= "/") . take 1) eRead
    eQuit    = () <$ ffilter (== "/quit") eRead
    eWrite   = leftmost [
        "Hi"  <$ eOpen
      ,          eMessage
      , "Bye" <$ eQuit
      ]
  return $ Output eWrite eQuit

guest' :: SampleApp6 t m
guest' (Input eOpen eRead) = mdo
  bHasNotQuit <- hold True (False <$ eQuit)
  let
    eRead' = gate bHasNotQuit eRead
    eMessage =       ffilter ((/= "/") . take 1) eRead'
    eQuit    = () <$ ffilter (== "/quit") eRead'
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
