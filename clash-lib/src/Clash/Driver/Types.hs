{-|
  Copyright  :  (C) 2013-2016, University of Twente,
                    2016-2017, Myrtle Software Ltd,
                    2017     , QBayLogic, Google Inc.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Type definitions used by the Driver module
-}

{-# LANGUAGE CPP #-}

module Clash.Driver.Types where

-- For Int/Word size
#include "MachDeps.h"

import           BasicTypes                     (InlineSpec)
import qualified Data.Set                       as Set
import           Data.Text                      (Text)
import           SrcLoc                         (SrcSpan)
import           Util                           (OverridingBool(..))

import           Clash.Core.Term                (Term)
import           Clash.Core.Var                 (Id)
import           Clash.Core.VarEnv              (VarEnv)

import           Clash.Netlist.BlackBox.Types   (HdlSyn (..))

-- | Global function binders
--
-- Global functions cannot be mutually recursive, only self-recursive
type BindingMap = VarEnv (Id,SrcSpan,InlineSpec,Term)

-- | Debug Message Verbosity
data DebugLevel
  = DebugNone
  -- ^ Don't show debug messages
  | DebugSilent
  -- ^ Run invariant checks and err if violated (enabled by any debug flag)
  | DebugFinal
  -- ^ Show completely normalized expressions
  | DebugName
  -- ^ Show names of applied transformations
  | DebugTry
  -- ^ Show names of tried AND applied transformations
  | DebugApplied
  -- ^ Show sub-expressions after a successful rewrite
  | DebugAll
  -- ^ Show all sub-expressions on which a rewrite is attempted
  deriving (Eq,Ord,Read)

data ClashOpts = ClashOpts { opt_inlineLimit :: Int
                           , opt_specLimit   :: Int
                           , opt_inlineFunctionLimit :: Word
                           , opt_inlineConstantLimit :: Word
                           , opt_dbgLevel    :: DebugLevel
                           , opt_dbgTransformations :: Set.Set String
                           , opt_cachehdl    :: Bool
                           , opt_cleanhdl    :: Bool
                           , opt_primWarn    :: Bool
                           , opt_color       :: OverridingBool
                           , opt_intWidth    :: Int
                           , opt_hdlDir      :: Maybe String
                           -- ^ Directory to store temporary files in. Will be
                           -- cleaned after Clash has finished executing.
                           , opt_hdlSyn      :: HdlSyn
                           , opt_errorExtra  :: Bool
                           , opt_floatSupport :: Bool
                           , opt_importPaths :: [FilePath]
                           , opt_componentPrefix :: Maybe String
                           , opt_newInlineStrat :: Bool
                           , opt_escapedIds :: Bool
                           , opt_ultra :: Bool
                           -- ^ Perform a high-effort compile, trading improved
                           -- performance for potentially much longer compile
                           -- times.
                           --
                           -- Name inspired by Design Compiler's /compile_ultra/
                           -- flag.
                           , opt_forceUndefined :: Maybe (Maybe Int)
                           -- ^
                           -- * /Nothing/: generate undefined's in the HDL
                           --
                           -- * /Just Nothing/: replace undefined's by a
                           -- constant in the HDL; the compiler decides what's
                           -- best
                           --
                           -- * /Just (Just x)/: replace undefined's by /x/ in
                           -- the HDL
                           , opt_checkIDir   :: Bool
                           }


defClashOpts :: ClashOpts
defClashOpts
  = ClashOpts
  { opt_dbgLevel            = DebugNone
  , opt_dbgTransformations  = Set.empty
  , opt_inlineLimit         = 20
  , opt_specLimit           = 20
  , opt_inlineFunctionLimit = 15
  , opt_inlineConstantLimit = 0
  , opt_cachehdl            = True
  , opt_cleanhdl            = True
  , opt_primWarn            = True
  , opt_color               = Auto
  , opt_intWidth            = WORD_SIZE_IN_BITS
  , opt_hdlDir              = Nothing
  , opt_hdlSyn              = Other
  , opt_errorExtra          = False
  , opt_floatSupport        = False
  , opt_importPaths         = []
  , opt_componentPrefix     = Nothing
  , opt_newInlineStrat      = True
  , opt_escapedIds          = True
  , opt_ultra               = False
  , opt_forceUndefined      = Nothing
  , opt_checkIDir           = True
  }

-- | Information about the generated HDL between (sub)runs of the compiler
data Manifest
  = Manifest
  { manifestHash :: (Int,Maybe Int)
    -- ^ Hash of the TopEntity and all its dependencies
    --   + (maybe) Hash of the TestBench and all its dependencies
  , portInNames  :: [Text]
  , portInTypes  :: [Text]
    -- ^ The rendered versions of the types of the input ports of the TopEntity
    --
    -- Used when dealing with multiple @TopEntity@s who have different names
    -- for types which are structurally equal
  , portOutNames :: [Text]
  , portOutTypes :: [Text]
    -- ^ The rendered versions of the types of the output ports of the TopEntity
    --
    -- Used when dealing with multiple @TopEntity@s who have different names
    -- for types which are structurally equal
  , componentNames :: [Text]
    -- ^ Names of all the generated components for the @TopEntity@ (does not
    -- include the names of the components of the @TestBench@ accompanying
    -- the @TopEntity@).
  }
  deriving (Show,Read)
