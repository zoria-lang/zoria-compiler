{-# LANGUAGE OverloadedStrings #-}
module Parser.Module where


import           Parser.Common
import qualified Syntax                        as Ast
import           Parser.RawAst

mockModule :: RawModule
mockModule = RawModule { modId      = Ast.ModuleId [] (Ast.ModName "Foo")
                       , modExports = Everything
                       , modImports = []
                       , modDefs    = []
                       }

module' :: Parser RawModule
module' = do 
    keyword "module"
    return mockModule
