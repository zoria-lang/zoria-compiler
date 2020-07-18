-- File with eval examples and tests. Remove it or do something else later

import           ValueSyntax
import           Syntax
import           Environment
import           Utility
import           Evaluator

import qualified Data.Text                     as T

testPosition :: Utility.Position
testPosition = Position 0 ""

e :: Expr Integer
e = Var (Identifier (T.pack "x")) testPosition 0

e2 = Block [] testPosition 0

env :: Environment
env = extendEnvironment (T.pack "x") (PrimitiveVal (IntVal 3)) emptyEnvironment

e3 = Lambda
    (WildcardPattern testPosition ())
    (And (Primitive (BoolLit True) testPosition ())
         (Primitive (BoolLit True) testPosition ())
         testPosition
         ()
    )
    Nothing
    testPosition
    ()

pattern :: Pattern ()
pattern = ConstPattern (IntLit 4) testPosition ()

-- val :: Value
-- val = PrimitiveVal (IntVal 5)

t :: Expr ()
t = Tuple
    [ Tuple
        [ Primitive (IntLit 1) testPosition ()
        , Primitive (IntLit 2) testPosition ()
        ]
        testPosition
        ()
    , Primitive (IntLit 3) testPosition ()
    ]
    testPosition
    ()--,
        -- Primitive (BoolLit False) testPosition ()
        -- Primitive (FloatLit 4.5) testPosition ()] testPosition ()


makeIdentifier :: String -> Identifier
makeIdentifier str = (Identifier (T.pack str))

identifiers = map makeIdentifier ["x", "y"] --,"z"]

varPatterns = map (\id -> VarPattern id testPosition ()) identifiers

tp = TuplePattern varPatterns testPosition ()

tp2 = TuplePattern [tp, VarPattern (makeIdentifier "z") testPosition ()]
                   testPosition
                   ()

namedPatternExample = NamedPattern (makeIdentifier "tuple") tp2 testPosition ()

vals = eval t emptyEnvironment

result = do
    v <- vals
    return $ matchPattern tp2 v


result2 = do
    v <- vals
    return $ matchPattern namedPatternExample v


cp = ConstPattern (IntLit 3) testPosition ()

cpTest = matchPattern cp (PrimitiveVal (IntVal 3))
