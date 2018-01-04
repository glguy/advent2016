{-# Language TemplateHaskell #-}
module Optimizer where

import Language.Haskell.TH

explode :: Name -> ExpQ -> ExpQ
explode name f =
  do TyConI (DataD _ _ _ _ constructors _) <- reify name
     xName <- newName "x"
     lam1E (varP xName)
           (caseE (varE xName)
                [ match (conP constructorName [])
                        (normalB (appE f (conE constructorName)))
                        []
                | NormalC constructorName [] <- constructors
                ])
