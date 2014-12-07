{-# LANGUAGE OverloadedStrings #-}

module Data.SafeData.Aeson where

import qualified Data.Aeson           as A
import qualified Data.HashMap.Strict  as H
import qualified Data.SafeCopy        as S
import qualified Data.Scientific      as SF
import qualified Data.Text            as T
import qualified Data.Vector          as V

instance A.ToJSON S.Value where
  toJSON (S.BValue v)   = A.Bool v
  toJSON (S.CValue v)   = A.String $ T.singleton v
  toJSON (S.SValue v)   = A.String $ T.pack v
  toJSON (S.DValue v)   = A.Number $ SF.fromFloatDigits v
  toJSON (S.FValue v)   = A.Number $ SF.fromFloatDigits v
  toJSON (S.IValue v)   = A.Number $ fromInteger $ toInteger v
  toJSON (S.I8Value v)  = A.Number $ fromInteger $ toInteger v
  toJSON (S.I16Value v) = A.Number $ fromInteger $ toInteger v
  toJSON (S.I32Value v) = A.Number $ fromInteger $ toInteger v
  toJSON (S.I64Value v) = A.Number $ fromInteger $ toInteger v
  toJSON (S.BIValue v)  = undefined
  toJSON (S.OrdValue v) = undefined
  toJSON (S.WValue v)   = undefined
  toJSON (S.W8Value v)  = undefined
  toJSON (S.W16Value v) = undefined
  toJSON (S.W32Value v) = undefined
  toJSON (S.W64Value v) = undefined
  toJSON (S.UValue v)   = undefined
  toJSON (S.BSValue v)  = undefined
  toJSON (S.BSLValue v) = undefined

  toJSON (S.Object ty cstr version vs)
                        = A.Object $ H.fromList $
                          [ ("_type",    A.toJSON ty)
                          , ("_cstr",    A.toJSON cstr)
                          , ("_version", A.toJSON version)
                          ]
                          ++ map (\(k, v) -> (T.pack k, A.toJSON v)) vs 
  toJSON (S.Array v)    = A.Array $ V.fromList $ map A.toJSON v
