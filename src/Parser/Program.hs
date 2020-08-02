{-# LANGUAGE OverloadedStrings #-}
module Parser.Program (program) where

import Parser.ParserIO
import Parser.Common
import Parser.Identifier
import Parser.Type
import Parser.Definition
import Parser.ModuleResolver
import Syntax
import GetOpt (optInputs)
import Data.Maybe (catMaybes)
import Control.Applicative ((<|>))
import Control.Monad (forM)
import Text.Megaparsec ((<?>))
import qualified Text.Megaparsec as P

-- Parser of programs. It is supposed to be used once.
program :: ParserIO (Program ())
program = do
    -- TODO: import prelude
    rootFile <- head . optInputs <$> getopt
    Program <$> parseFile rootFile module'

-- Parser for modules. The job of file parser is delegated here.
module' :: FilePath -> ParserIO (Module())
module' path = do
    skipWhitespace
    (name, exports) <- moduleHeader <?> "module declaration"
    validateModuleName path name
    withStack name path $ do
        rawImports <- P.many $ located (import' <?> "module import")        
        imports  <- forM rawImports $ importFile module'
        importOperatorsFromImports imports
        definitions <- catMaybes <$> P.many topLevelDef
        skipWhitespace >> P.eof
        finalizeModule exports
        return $ Module (ModuleId [] name) path imports exports definitions

-- Parser for module declarations.
moduleHeader :: ParserIO (ModName, Maybe [Located ImportedValue])
moduleHeader = do
    keyword "module"
    name    <- uppercaseName <?> "module name"
    exports <- moduleIdentifierList
    return (ModName name, exports)

    -- Parser for a single import statement. It does not read any new files.
import' :: ParserIO RawImport
import' = do
    keyword "import"
    name  <- qualifiedModuleName <?> "module name"
    list  <- moduleIdentifierList
    alias <- P.optional $ keyword "as" *> (uppercaseName <?> "module synonym")
    return (name, ModName <$> alias, list)

-- Parser for import lists. Despite the name it is also used to parse
-- the export lists. The lists are optional so it returns Maybe.
moduleIdentifierList :: ParserIO (Maybe [Located ImportedValue])
moduleIdentifierList = P.optional list
  where
    list = separatedList "{" "}" (located exportElem) ","
    -- Parser for single imported/exported thing (identifier or a type name
    -- (possibly with constructors)).
    exportElem :: ParserIO ImportedValue
    exportElem = (ImportedIdentifier . Identifier <$> prefixIdentifier)
             <|> (uncurry ImportedType <$> typeImport)
    -- Parser for type imports. Type imports consist of type name optionally
    -- followed by the explicit type constructor list. [] is a valid type name.
    typeImport :: ParserIO (TypeName, Maybe [ConstructorName])
    typeImport = do
        typeName     <- TypeName <$> typeName
        constructors <- P.optional constructorList
        return (typeName, constructors)
    -- Parser for lists of exported/imported type constructors.
    constructorList :: ParserIO [ConstructorName]
    constructorList = separatedList "{" "}" constructorName "," 
        <?> "constructor list"
