{-|
Copyright  :  (C) 2019, Myrtle Software Ltd
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Verification
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes   #-}

module Clash.Verification where

import Clash.Annotations.Primitive (hasBlackBox)
import Clash.Signal
import Clash.Signal.Internal
import Clash.XException

past
  :: HiddenClock dom
  => Signal dom Bool
  -> Signal dom Bool
past s = errorX "no past" :- s
{-# NOINLINE past #-}
{-# ANN past hasBlackBox #-}

assertAt
  :: KnownDomain dom
  => Clock dom
  -> (HiddenClock dom => Signal dom Bool)
  -> a
  -> a
assertAt !_ !_ = id
{-# NOINLINE assertAt #-}
{-# ANN assertAt hasBlackBox #-}

assertAlways
  :: Signal dom Bool
  -> a
  -> a
assertAlways !_ = id
{-# NOINLINE assertAlways #-}
{-# ANN assertAlways hasBlackBox #-}
