{-# LANGUAGE OverloadedStrings #-}
module Parser.Module where


import           Parser.Common
import           Parser.RawAst
import           Parser.Identifier
import qualified Syntax                        as Ast
import           Text.Megaparsec                ( (<?>) )

mockModule :: RawModule
mockModule = RawModule { modId      = Ast.ModuleId [] (Ast.ModName "Foo")
                       , modExports = Everything
                       , modImports = []
                       , modDefs    = []
                       }

module' :: Parser RawModule
module' = do
    keyword "module"
    name <- moduleName
    return $ mockModule { modId = name }

moduleName :: Parser Ast.ModuleId
moduleName = (Ast.ModuleId <$> nameQualifier <*> name) <?> "module name"
  where
    nameQualifier = list' "" name "" "/"
    name          = Ast.ModName <$> uppercaseIdentifier
