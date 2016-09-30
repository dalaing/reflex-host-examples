{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Host6.Test (
    InputCmd(..)
  , OutputCmd(..)
  , test
  ) where

import Data.Functor.Identity
import Control.Monad.Fix

import Data.GADT.Compare
import Data.Dependent.Sum
import Data.Dependent.Map

import Reflex

import Control.Lens
import Control.Lens.TH

import Host6
import Interpret

data InputCmd =
    Open
  | Read String
  deriving (Eq, Ord, Show)

makePrisms ''InputCmd

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

data OutputCmd =
    Write String
  | Quit
  deriving (Eq, Ord, Show)

makePrisms ''OutputCmd

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

mkInterpretable' :: Reflex t => SampleApp6 t m -> Interpretable t m InputCmd [OutputCmd]
mkInterpretable' guest = \i -> do
  o <- guest . fanInput $ i
  return $ mergeOutput o

test' :: (forall t m. SampleApp6 t m) -> [Maybe InputCmd] -> IO [Maybe [OutputCmd]]
test' app =
  interpret (mkInterpretable' app)

instance FanIn Input where
  type TestEventInput Input = InputCmd
  fanIn =
    Input <$>
      fanE _Open <*>
      fanE _Read

instance MergeOut Output where
  type TestEventOutput Output = OutputCmd
  mergeOut (Output eWrite eQuit)=
    mergeWith (++) [
        mergeE _Write eWrite
      , mergeE _Quit eQuit
      ]

-- It would be nice to wrap this up so that we have
-- - the FanIn instance
-- - the MergeOut instance
-- - the constraints that we need to hold for our app
test :: (FanIn i, MergeOut o) => (forall t m. (Reflex t, MonadHold t m, MonadFix m) => (i t -> m (o t))) -> [Maybe (TestEventInput i)] -> IO [Maybe [TestEventOutput o]]
test app =
  interpret (mkInterpretable app)

-- |
-- >>> test' guest [Just Open, Just (Read "Hello"), Just (Read "/quit"), Just (Read "Oops")]
-- [Just [Write "Hi"],Just [Write "Hello"],Just [Write "Bye",Quit],Just [Write "Oops"]]
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

-- |
-- >>> test guestTrimQuit [Just Open, Just (Read "Hello"), Just (Read "/quit"), Just (Read "Oops")]
-- [Just [Write "Hi"],Just [Write "Hello"],Just [Write "Bye",Quit],Nothing]
guestTrimQuit :: SampleApp6 t m
guestTrimQuit (Input eOpen eRead) = mdo
  bHasNotQuit <- hold True (False <$ eQuit)
  let
    eRead' = gate bHasNotQuit eRead
    eMessage =       ffilter (/= "/quit") eRead'
    eQuit    = () <$ ffilter (== "/quit") eRead'
    eWrite   = leftmost [
        "Hi"  <$ eOpen
      ,          eMessage
      , "Bye" <$ eQuit
      ]
  return $ Output eWrite eQuit
