{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE RankNTypes #-}
module Interpret (
    Interpretable
  , interpret
  ) where

import Control.Monad.Ref
import Control.Monad.Fix
import Data.Functor.Identity

import Data.Dependent.Sum

import Reflex
import Reflex.Host.Class

type Interpretable t m a b = (Reflex t, MonadHold t m, MonadFix m)
                           => Event t a
                           -> m (Event t b)

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
