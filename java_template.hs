import Data.List

data Import = Import Package
data Package = String
data Visibility = Public | Private
data TType = TInt (Maybe Int) | TBool (Maybe Bool) | TString (Maybe String) deriving (Eq)
type Variable = String
data VariableDeclaration = Dcl (Variable, TType) deriving (Show, Eq)
data Function = Function { functionVisibility :: Visibility, functionName :: String, functionSignature :: Signature, functionBody :: [Expression] } deriving Show
data Signature = Signature { javaSignatureInputs :: [VariableDeclaration], javaSignatureOutput :: VariableDeclaration } deriving (Show, Eq)
type Expression = String

-- show instances
instance Show Visibility where
  show Public = "public"
  show Private = "private"

instance Show TType where
  show (TInt _) = "int"
  show (TBool _) = "boolean"
  show (TString _) = "String"

-- T template code 
x :: VariableDeclaration
x = Dcl ("x", TInt $ Just 3)

y :: VariableDeclaration
y = Dcl ("y", TInt $ Just 2)

z :: VariableDeclaration
z = Dcl ("z", TBool Nothing)

bigger :: Function
bigger = Function Public "bigger" (Signature [x, y] z) 
     [jt z ++ " " ++ jn z ++ " = " ++ jn x ++ " < " ++ jn y]

-- to java functions
jFunctionDefinition :: Function -> String
jFunctionDefinition (Function v n s b) = show v ++ " " ++ jOutputType s ++ " " ++ n ++ jSignature s ++ "{" ++ foldr ((\a b -> a ++ ";" ++ b) . jExpression) "" b ++ " return " ++ jOutputVariable s ++  ";};"

jFunctionCall :: Function -> [VariableDeclaration] -> Maybe String
jFunctionCall f vs 
    | vs == javaSignatureInputs (functionSignature f) = Just $ 
                                                          functionName f ++ "(" ++ intercalate ", " (map (\x -> je x) vs) ++ ")"
    | otherwise = Nothing

jSignature :: Signature -> String 
jSignature (Signature is _) = "(" ++ intercalate ", " (map jVariableDeclaration is) ++ ")"

jVariableDeclaration :: VariableDeclaration -> String
jVariableDeclaration (Dcl (v, t)) = show t ++ " " ++ v

--evaluate variable
je :: VariableDeclaration -> String 
je (Dcl (_, t)) = eval t
                                     where eval (TInt (Just i)) = show i
                                           eval (TBool (Just b)) = show b
                                           eval (TString (Just s)) = s
                                           eval _ = "ERROR: Could not evaluate variable."

--variable name
jn :: VariableDeclaration -> String
jn (Dcl (v, _)) = v

--variable type
jt :: VariableDeclaration -> String
jt (Dcl (_, t)) = show t

jExpression :: Expression -> String
jExpression e = e 

jOutputType :: Signature -> String
jOutputType (Signature _ (Dcl (vn, vt))) = show vt

jOutputVariable :: Signature -> String
jOutputVariable (Signature _ (Dcl (vn, _))) = vn

-- main
main = do
    putStrLn $ jFunctionDefinition bigger
    maybePrint $ jFunctionCall bigger [x, y]
    maybePrint $ jFunctionCall bigger [x]
        where maybePrint (Just s) = putStrLn s
              maybePrint Nothing = putStrLn "ERROR: could not assign variables to function. Did you provide variables which match the signature?"
                  
