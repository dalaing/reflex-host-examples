{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Interpret (
    Interpretable
  , interpret
  , FanIn(..)
  , fanE
  , MergeOut(..)
  , mergeE
  , mkInterpretable
  ) where

import Control.Monad.Ref
import Control.Monad.Fix
import Data.Functor.Identity

import Control.Lens

import Data.Dependent.Sum

import Reflex
import Reflex.Host.Class

type Interpretable t m a b = (Reflex t, MonadHold t m, MonadFix m)
                           => Event t a
                           -> m (Event t b)

fanE :: Reflex t => Prism' a e -> Event t a -> Event t e
fanE p = fmapMaybe (preview p)

class FanIn a where
  type TestEventInput a
  fanIn :: Reflex t => Event t (TestEventInput a) -> a t

mergeE :: Reflex t => Prism' a e -> Event t e -> Event t [a]
mergeE p = fmap (pure . review p)

class MergeOut a where
  type TestEventOutput a
  mergeOut :: Reflex t => a t -> Event t [TestEventOutput a]

mkInterpretable :: (Reflex t, FanIn i, MergeOut o) => (i t -> m (o t)) -> Interpretable t m (TestEventInput i) [TestEventOutput o]
mkInterpretable f = \i -> do
  o <- f . fanIn $ i
  return $ mergeOut o

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
