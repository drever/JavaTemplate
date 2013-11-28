import Data.List

data Import = Import Package
data Package = String
data Visibility = Public | Private
data JavaType = JavaInt | JavaBool | JavaString deriving (Eq)
type Variable = String
data VariableDeclaration = VariableDeclaration (Variable, JavaType) deriving (Show, Eq)
data Function = Function { functionVisibility :: Visibility, functionName :: String, functionSignature :: JavaSignature, functionBody :: [JavaExpression] } deriving Show
data JavaSignature = JavaSignature { javaSignatureInputs :: [VariableDeclaration], javaSignatureOutput :: VariableDeclaration } deriving (Show, Eq)
type JavaExpression = String

-- show instances
instance Show Visibility where
  show Public = "public"
  show Private = "private"

instance Show JavaType where
  show JavaInt = "int"
  show JavaBool = "boolean"
  show JavaString = "String"

-- Java template code 
varX :: VariableDeclaration
varX = VariableDeclaration ("x", JavaInt)

varY :: VariableDeclaration
varY = VariableDeclaration ("y", JavaInt)

varZ :: VariableDeclaration
varZ = VariableDeclaration ("z", JavaBool)

bigger :: Function
bigger = Function Public "bigger" (JavaSignature [varX, varY] varZ) 
     [rjt varZ ++ " " ++ rjv varZ ++ " = " ++ rjv varX ++ " < " ++ rjv varY]

-- to java functions
renderFunctionDefinition :: Function -> String
renderFunctionDefinition (Function v n s b) = show v ++ " " ++ javaSignatureOutputType s ++ " " ++ n ++ renderSignature s ++ "{" ++ foldr ((\a b -> a ++ ";" ++ b) . renderExpression) "" b ++ " return " ++ javaSignatureOutputVariable s ++  ";};"

renderFunctionCall :: Function -> [VariableDeclaration] -> Maybe String
renderFunctionCall f vs 
    | vs == javaSignatureInputs (functionSignature f) = Just "TBD, implement variable declaration to variable assignment."
    | otherwise = Nothing

renderSignature :: JavaSignature -> String 
renderSignature (JavaSignature is _) = "(" ++ intercalate ", " (map renderVariableDeclaration is) ++ ")"

renderVariableDeclaration :: VariableDeclaration -> String
renderVariableDeclaration (VariableDeclaration (v, t)) = show t ++ " " ++ v

rjv :: VariableDeclaration -> String
rjv (VariableDeclaration (v, _)) = v

rjt :: VariableDeclaration -> String
rjt (VariableDeclaration (_, t)) = show t
renderExpression :: JavaExpression -> String
renderExpression e = e 

javaSignatureOutputType :: JavaSignature -> String
javaSignatureOutputType (JavaSignature _ (VariableDeclaration (vn, vt))) = show vt

javaSignatureOutputVariable :: JavaSignature -> String
javaSignatureOutputVariable (JavaSignature _ (VariableDeclaration (vn, _))) = vn

-- main
main = do
    putStrLn $ renderFunctionDefinition bigger
    maybePrint $ renderFunctionCall bigger [varX, varY]
    maybePrint $ renderFunctionCall bigger [varX]
        where maybePrint (Just s) = putStrLn s
              maybePrint Nothing = putStrLn "ERROR: could not assign variables to function. Did you provide variables which match the signature?"
                  
