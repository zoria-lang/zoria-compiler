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
    (name, _) <- moduleHeader
    return $ mockModule { modId = name }

moduleHeader :: Parser (Ast.ModuleId, IdentifierList)
moduleHeader = do
    keyword "module"
    name <- moduleName <?> "module name"
    return (name, Everything)

moduleName :: Parser Ast.ModuleId
moduleName = do
    names <- list1' "" name "" "\\"
    return $ Ast.ModuleId (init names) (last names)
    where name = Ast.ModName <$> (uppercaseIdentifier <?> "module name") 
