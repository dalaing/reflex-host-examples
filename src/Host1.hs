{-
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE RankNTypes #-}
module Host1 (
    go1
  ) where

import Control.Monad (forever)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Identity (Identity(..))
import Control.Monad.IO.Class (liftIO)
import Data.IORef (readIORef)
import System.IO

import Data.Dependent.Sum

import Reflex
import Reflex.Host.Class

type SampleApp1 t m = (Reflex t, MonadHold t m, MonadFix m)
                  => Event t String
                  -> m (Behavior t Int)

host :: (forall t m. SampleApp1 t m)
     -> IO ()
host myGuest = do

  runSpiderHost $ do
    (e, eTriggerRef) <- newEventWithTriggerRef
    b <- runHostFrame $ myGuest e

    forever $ do
      input <- liftIO getLine
      liftIO $ putStrLn $ "Input Event: " ++ show input

      {-
      mETrigger <- liftIO $ readIORef eTriggerRef
      case mETrigger of
        Nothing -> do
          return ()
        Just eTrigger -> do
          fireEvents [eTrigger :=> Identity input]
      -}
      fireEventRef eTriggerRef input

      output <- runHostFrame $ sample b
      liftIO $ putStrLn $ "Output Behavior: " ++ show output

guest :: SampleApp1 t m
guest e = do
  -- increment every time we read a line
  d <- foldDyn (+) 0 (1 <$ e)
  -- return the number of lines as a behavior
  return $ current d

go1 :: IO ()
go1 = do
  hSetBuffering stdin LineBuffering
  host guest
