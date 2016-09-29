{-
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE RankNTypes #-}
module Host2 (
    go2
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

type SampleApp2 t m = (Reflex t, MonadHold t m, MonadFix m)
                  => Event t String
                  -> m (Event t ())

host :: (forall t m. SampleApp2 t m)
     -> IO ()
host myGuest = do

  runSpiderHost $ do
    (e, eTriggerRef) <- newEventWithTriggerRef
    eQuit <- runHostFrame $ myGuest e
    hQuit <- subscribeEvent eQuit

    let
      loop = do
        input <- liftIO getLine
        liftIO $ putStrLn $ "Input Event: " ++ show input

        {-
        mETrigger <- liftIO $ readIORef eTriggerRef
        mQuit <- case mETrigger of
          Nothing -> do
            return Nothing
          Just eTrigger -> do
            fireEventsAndRead [eTrigger :=> Identity input] $ do
              mValue <- readEvent hQuit
              sequence mValue
        -}

        mQuit <- fireEventRefAndRead eTriggerRef input hQuit

        liftIO $ putStrLn $ "Output Event: " ++ show mQuit

        unless (isJust mQuit)
          loop

    loop

guest :: SampleApp2 t m
guest e = do
  let
    eQuit = void . ffilter (== "/quit") $ e
  return eQuit

go2 :: IO ()
go2 = do
  hSetBuffering stdin LineBuffering
  host guest
