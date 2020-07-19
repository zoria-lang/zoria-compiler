{-# LANGUAGE OverloadedStrings #-}
module Parser.Module where


import           Parser.Common
import           Parser.RawAst
import           Parser.Identifier
import qualified Syntax                        as Ast
import           Text.Megaparsec                ( (<?>) )
import qualified Text.Megaparsec               as P

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
moduleName = do
    names <- list1' "" name "" "/"
    return $ Ast.ModuleId (init names) (last names)
    where name = (Ast.ModName <$> uppercaseIdentifier) <?> "module name"
