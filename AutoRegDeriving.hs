{-# LANGUAGE ViewPatterns,TemplateHaskell #-}

module AutoRegDeriving where
import Prelude
import Data.List (nub)
import Clash.Explicit.Prelude (register)
import Clash.Signal.Internal
import Clash.XException

import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Ppr

import AutoReg
import Control.Monad

import Control.Lens.Internal.TH (appsE1,bndrName,conAppsT)
import Data.List

deriveAutoReg :: Name -> DecsQ
deriveAutoReg tyNm = do
  info <- reify tyNm
  case info of
    TyConI tyDec -> deriveTyConI tyDec
    _ -> fail (unlines [ "deriveAutoReg not possible for: " ++ pprint tyNm ++ ", reify returned:"
                       , show info])


deriveTyConI :: Dec -> DecsQ
deriveTyConI (DataD _cxt nm tyVars _mkind cons _derivs) =
  case cons of
    [con] -> deriveAutoRegProduct nm tyVars con
    [] -> fail "Can't derive AutoReg for empty type"
    _  -> fail "TODO derive AutoReg for sum types"

deriveTyConI (NewtypeD _cxt nm tyVars _mkind con _derivs) = deriveAutoRegProduct nm tyVars con
deriveTyConI other = fail ("deriveTyConI called with: " ++ show other)


deriveAutoRegProduct :: Name -> [TyVarBndr] -> Con -> DecsQ
deriveAutoRegProduct tyNm tyVarBndrs con = case con of
  NormalC nm (map (\(_,ty) -> (Nothing,ty)) -> fieldTys) -> go nm fieldTys
  RecC nm (map (\(sNm,_,ty)-> (Just sNm,ty)) -> fieldTys) -> go nm fieldTys
  InfixC f1 nm f2 -> go nm (map (\(_,ty) -> (Nothing,ty)) [f1,f2])
  ForallC _ _ con' -> fail "Can't derive AutoReg for existentially quantified data constructors" -- deriveAutoRegProduct tyNm tyVarBndrs con'
  _ -> fail "Can't derive AutoReg for GADTs"
  where
    go :: Name -> [(Maybe Name,Type)] -> Q [Dec]
    go tyConNm fields = do
      let fieldTys = map snd fields
          fieldNames = map fst fields
      args <- mapM newName ["clk","rst","en","initVal","input"]
      let [clkE,rstE,enE,initValE,inputE] = map varE args
      tyVarRoles <- reifyRoles tyNm
      let
        tyVars = map (VarT . bndrName) tyVarBndrs
        ctx = map (AppT (ConT ''AutoReg)) [tv | (tv,RepresentationalR) <- zip tyVars tyVarRoles] -- TODO what about the Nominal role?
        ty = conAppsT tyNm tyVars
        argsP = map varP args
      parts <- generateNames "field" fields
      let
        field :: Name -> Int -> DecQ
        field nm nr = valD (varP nm) (normalB [|$fieldSel <$> $inputE|]) []
          where
            fieldSel = do
              xNm <- newName "x"
              lamE [conP tyConNm [if nr == n then varP xNm else wildP | (n,_) <- zip [0..] fields]] (varE xNm)
      fieldDecls <- sequence $ zipWith field parts [0..]
      sigs <- generateNames "sig" fields
      initVals <- generateNames "initVal" fields
      let initPat = conP tyConNm (map varP initVals)
      initDecl <- valD initPat (normalB initValE) []
      partDecls <- concat <$> (sequence $ zipWith4 (\s v i nameM -> case nameM of
                                                                     Nothing -> [d| $s = autoReg $clkE $rstE $enE $i $v|]
                                                                     Just nm -> let nmSym = litT $ strTyLit (show nm) in [d| $s = suffixName @($nmSym) (autoReg $clkE $rstE $enE $i $v) |]
                                                   )
                                                   (varP <$> sigs)
                                                   (varE <$> parts)
                                                   (varE <$> initVals)
                                                   (fieldNames)
                                                   )
      let
          decls :: [DecQ]
          decls = map pure (initDecl : fieldDecls ++ partDecls)
          tyConE = conE tyConNm
      let body = case map varE sigs of
                  (sig0:rest) -> foldl (\acc ty -> [| $acc <*> $ty |] ) [| $tyConE <$> $sig0 |] rest
                  [] -> fail "Derive AutoReg not supported for constructors without fields"

      autoRegDec <- funD 'autoReg [clause argsP (normalB body) decls]
      return [InstanceD Nothing ctx (AppT (ConT ''AutoReg) ty) [autoRegDec]]

-- | Generate a list of fresh Name's:
-- prefix0_.., prefix1_.., prefix2_.., ..
generateNames :: String -> [a] -> Q [Name]
generateNames prefix xs = sequence (zipWith (\n _ -> newName $ prefix ++ show n) [0..] xs)
