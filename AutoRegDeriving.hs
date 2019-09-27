{-# LANGUAGE ViewPatterns,TemplateHaskell #-}

module AutoRegDeriving where
import Prelude
import Data.List (nub)
import Clash.Explicit.Prelude (register)
import Clash.Signal.Internal
import Clash.XException

import Language.Haskell.TH.Datatype
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Ppr

import AutoReg
import Control.Monad

import Control.Lens.Internal.TH (appsE1,bndrName,conAppsT)
import Data.List

deriveAutoReg :: Name -> DecsQ
deriveAutoReg tyNm = do
  tyInfo <- reifyDatatype tyNm
  case datatypeCons tyInfo of
    [] -> fail $ "Can't derive AutoReg for empty types"
    [conInfo] -> deriveAutoRegProduct tyInfo conInfo
    _ -> fail "Can't derive AutoReg for sum types"



deriveAutoRegProduct :: DatatypeInfo -> ConstructorInfo -> DecsQ
deriveAutoRegProduct tyInfo conInfo = go (constructorName conInfo) (zip fieldNames (constructorFields conInfo))
  where
    tyNm = datatypeName tyInfo
    tyVarBndrs = datatypeVars tyInfo

    fieldNames = case constructorVariant conInfo of
      RecordConstructor nms -> map Just nms
      _ -> repeat Nothing
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
