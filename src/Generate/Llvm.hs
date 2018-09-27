{-# LANGUAGE OverloadedStrings #-}
module Generate.Llvm (generate) where

import qualified Data.Text.Lazy as LazyText

import LLVM.Pretty (ppllvm)
import qualified LLVM.AST as AST
import qualified LLVM.AST.Linkage as Linkage
import qualified LLVM.AST.Visibility as Visibility
import qualified LLVM.AST.CallingConvention as Convention

import qualified AST.Module as Module


generate :: Module.Optimized -> LazyText.ByteString
generate (Module.Module moduleName _ _info) =
  ppllvm astModule


astModule :: AST.Module
astModule =
  AST.Module
    { AST.moduleName         = "pine"
    , AST.moduleDataLayout   = Nothing
    , AST.moduleTargetTriple = Nothing
    , AST.moduleDefinitions  =
        [ AST.GlobalDefinition
            (AST.Function
                Linkage.External
                Visibility.Default
                Nothing
                Convention.C
                []
                (AST.IntegerType 8)
                (AST.Name "main")
                ([], False)
                []
                Nothing
                Nothing
                0
                Nothing
                Nothing
                [ AST.BasicBlock
                    (AST.Name "entry")
                    []
                    (AST.Do
                        (AST.Ret
                            (Just (AST.ConstantOperand (AST.Int 8 42)))
                            []
                        )
                    )
                ]
            )
        ]
    }
