{-# LANGUAGE DataKinds,TypeOperators,TypeFamilies,NoImplicitPrelude,FlexibleContexts,TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}  -- needed for "KnownNat (BitSize a)" constraint on instance AutoReg(Maybe a)
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
module AutoReg where
import Data.Maybe (fromMaybe,isJust)
import Data.Word
import Data.Int
import Clash.Explicit.Prelude
import Clash.Prelude ((.&&.))
import qualified Clash.Prelude as I
import GHC.Generics
import Data.Functor.Compose
import Data.Proxy

class (NFDataX a) => AutoReg a where
  autoReg :: KnownDomain dom
          => Clock dom -> Reset dom -> Enable dom
          -> a -- ^ Reset value
          -> Signal dom a -> Signal dom a
  autoReg = register

autoRegGeneric
  :: (Generic a, GAutoReg (Rep a), KnownDomain dom)
  => Clock dom -> Reset dom -> Enable dom
  -> a -- ^ Reset value
  -> Signal dom a -> Signal dom a
autoRegGeneric clk rst en initVal input =
  to <$> gAutoReg clk rst en (from initVal) (from <$> input)
  -- undefined

instance AutoReg Bool
instance AutoReg Char
instance AutoReg Integer
instance AutoReg Int
instance AutoReg Int8
instance AutoReg Int16
instance AutoReg Int32
instance AutoReg Int64
instance AutoReg Word
instance AutoReg Word8
instance AutoReg Word16
instance AutoReg Word32
instance AutoReg Word64

instance AutoReg Bit
instance AutoReg (BitVector n)
instance AutoReg (Signed n)
instance AutoReg (Unsigned n)
instance AutoReg (Index n)

instance (AutoReg a, BitPack a, KnownNat (BitSize a)) => AutoReg (Maybe a) where
  autoReg clk rst en initVal m =
    let
        tag = isJust <$> m
        tagInit = isJust initVal
        tagR = register clk rst en tagInit tag
        tagSig = (pack <$> tagR)

        val = fromMaybe undefined <$> m
        valInit = fromMaybe undefined initVal

        valR = autoReg clk rst (toEnable (fromEnable en .&&. tag)) valInit val
        valSig = (pack <$> valR)
    in unpack <$> ((++#) <$> tagSig <*> valSig)

-- instance (AutoReg a, AutoReg b) => AutoReg (a,b) where
--   autoReg clk rst en initVal xys =
--     ((,) <$> xSig) <*> ySig
--     where
--       (xs,ys) = unbundle xys
--       (xInit,yInit) = initVal
--       xSig = autoReg clk rst en xInit xs
--       ySig = autoReg clk rst en yInit ys

instance (KnownNat n, AutoReg a) => AutoReg (Vec n a) where
  autoReg clk rst en initVal xs =
    bundle $ (autoReg clk rst en) <$> initVal <*> (unbundle xs)


autoRegImplicit :: (I.HiddenClockResetEnable dom, AutoReg a)
                => a -> Signal dom a -> Signal dom a
autoRegImplicit = I.hideClockResetEnable autoReg

-- instance Bundle ((f :*: g) k) where
--   type Unbundled dom ((f :*: g) k) = ((forall k. Signal dom (f k)) :*: (forall k. Signal dom (g k)))
-- p
-- pbundle (fs :*: gs) = (:*:) <$> fs <*> gs

--(:*:) :: f p -> g p -> (:*:) f g p
--punbundle :: Signal dom (f p :*: g p) -> (Signal dom (f p) :*: Signal dom (g p))

-- punbundle :: Functor g => g ((:*:) f f p) -> (:*:) g g (f p)
-- punbundle :: forall sig f g p . sig ((:*:) f g p) -> (:*:) (Compose sig f) (Compose sig g) p
--
-- punbundle fgs = (_ <$> fgs) :*: (_ <$> fgs)
--     where
--       -- fs :: _
--       -- fs = fmap getF fgs
--       -- getF :: _ -- (:*:) f g1 p1 -> f p1
--       getF (f :*: _) = f
--       getG (_ :*: g) = g



-- class GAutoReg g where
--   gAutoReg :: KnownDomain dom
--           => Clock dom -> Reset dom -> Enable dom
--           -> g a -- ^ Reset value
--           -> Signal dom (g a) -> Signal dom (g a)
--
-- instance (GAutoReg f, GAutoReg g) => GAutoReg (f :*: g) where
--   gAutoReg clk rst en initVal input =
--     (:*:) <$> fSig <*> gSig
--     where
--       (fs :*: gs) = punbundle input :: _ -- ((Signal dom f) :*: (Signal dom g))
--       (fInit :*: gInit) = initVal
--       fSig = autoReg clk rst en fInit fs
--       gSig = autoReg clk rst en gInit gs



-- class GAutoReg f where
--   gAutoReg :: f a -> Compose (Signal dom) f a -> Compose (Signal dom) f a
--
-- instance (GAutoReg f, GAutoReg g) => GAutoReg (f :*: g) where
--   gAutoReg (a :*: b) (Compose q) = Compose ((:*:) <$> getCompose sa <*> getCompose sb)
--     where
--       qa = fmap (\(l :*: _) -> l) q
--       sa = gAutoReg a (Compose qa)
--
--       qb = fmap (\(_ :*: r) -> r) q
--       sb = gAutoReg b (Compose qb)

punbundle
  :: Signal dom ((:*:) f g a) -> (:*:) (Compose (Signal dom) f) (Compose (Signal dom) g) a
punbundle fgs = (Compose (getF <$> fgs)) :*: (Compose (getG <$> fgs))
  where
    getF (f :*: _) = f
    getG (_ :*: g) = g


class GAutoReg f where
  gAutoReg :: KnownDomain dom => Clock dom -> Reset dom -> Enable dom -> f a -> Signal dom (f a) -> Signal dom (f a)

instance (GAutoReg f, GAutoReg g) => GAutoReg (f :*: g) where
  gAutoReg clk rst en (a :*: b) q =  ((:*:) <$> sa <*> sb)
    where
      getL (l :*: _) = l
      getR (_ :*: r) = r
      fs :*: gs = unbundle q
      sa = gAutoReg clk rst en a (getCompose fs) -- (getL <$> q)
      sb = gAutoReg clk rst en b (getCompose gs) -- (getR <$> q)

-- Ignore datatype and constructor metadata
instance GAutoReg a => GAutoReg (M1 D d a) where
  gAutoReg clk rst en initVal input = M1 <$> gAutoReg clk rst en (unM1 initVal) (unM1 <$> input)
instance GAutoReg a => GAutoReg (M1 C d a) where
  gAutoReg clk rst en initVal input = M1 <$> gAutoReg clk rst en (unM1 initVal) (unM1 <$> input)
-- but use record selector metadata
-- instance (GAutoReg a,Selector selMeta) => GAutoReg (M1 S selMeta a) where
--   gAutoReg clk rst en initVal input = M1 <$> gAutoReg clk rst en (unM1 initVal) (unM1 <$> input)
--     where
--       nm = selName (initVal)

-- field with a selector name
instance (GAutoReg a) => GAutoReg (M1 S (MetaSel (Just selNm) su ss ds) a) where
  gAutoReg clk rst en initVal input = M1 <$> suffixName @selNm (gAutoReg clk rst en (unM1 initVal) (unM1 <$> input))
-- field without a selector name
instance (GAutoReg a) => GAutoReg (M1 S (MetaSel Nothing su ss ds) a) where
  gAutoReg clk rst en initVal input = M1 <$> gAutoReg clk rst en (unM1 initVal) (unM1 <$> input)




instance AutoReg c => GAutoReg (K1 i c) where
  gAutoReg clk rst en initVal input = K1 <$> autoReg clk rst en (unK1 initVal) (unK1 <$> input)





instance Bundle ((f :*: g) a) where
  type Unbundled t ((f :*: g) a) = (Compose (Signal t) f :*: Compose (Signal t) g) a
  bundle (Compose l :*: Compose r) = (:*:) <$> l <*> r
  unbundle s = Compose (getL <$> s) :*: Compose (getR <$> s)
   where
    getL (l :*: _) = l
    getR (_ :*: r) = r
