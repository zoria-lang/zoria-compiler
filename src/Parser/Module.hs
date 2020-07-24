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
    imports   <- P.some importStatement
    return $ mockModule { modId = name, modImports = imports }

moduleHeader :: Parser (Ast.ModuleId, IdentifierList)
moduleHeader = do
    keyword "module"
    name    <- qualifiedModuleId
    exports <- importList
    return (name, exports)

moduleName :: Parser Ast.ModName
moduleName = Ast.ModName <$> uppercaseIdentifier <?> "module name"

qualifiedModuleId :: Parser Ast.ModuleId
qualifiedModuleId = do
    names <- list1' "" moduleName "" "\\"
    return $ Ast.ModuleId (init names) (last names)

importStatement :: Parser (Ast.Located Import)
importStatement = do
    keyword "import"
    withPos $ \pos -> do
        from    <- qualifiedModuleId
        imports <- importList
        alias   <- P.optional $ keyword "as" *> moduleName
        return $ Ast.Located pos (Import from alias imports)

importList :: Parser IdentifierList
importList = P.option Everything
                      (Specified <$> listOfItems <?> "list of identifiers")
    where listOfItems = list' "(" importListItem ")" ","

-- TODO: add type import items
importListItem :: Parser (Ast.Located Ast.ImportedValue)
importListItem = withPos $ \pos -> Ast.Located pos <$> simpleImportListItem
  where
    simpleImportListItem =
        Ast.ImportedIdentifier . Ast.Identifier <$> variableName
