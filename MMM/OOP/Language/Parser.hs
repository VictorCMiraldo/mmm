-----------------------------------------------------------------------------
-- |
-- Module      :  Parser
-- Copyright   :  HASLAb team, University of Minho
-- License     :  MIT
--
-- Maintainer  :  Victor Miraldo <victor.cacciari@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
--Simple parser for 'MMM.OOP.Language.Syntax'
----------------------------------------------------------------------------
module MMM.OOP.Language.Parser where

import Text.Parsec
import Text.Parsec.Pos
import Text.Parsec.Error
import Text.Parsec.Language
import Text.Parsec.Expr
import qualified Text.Parsec.Token as Tkn

import MMM.OOP.Language.Syntax
import qualified MMM.Util.Pretty as PPR

import Control.Applicative hiding ((<|>), many)

import System.IO(hGetContents, withFile, IOMode(ReadMode))




oopLangDef
  = javaStyle {
      Tkn.reservedNames =
        ["class", "method", "var", "return", "if", "then", "else",
         "int", "float", "void", "this", "list", "of", "head", "tail",
         "length", "in", "import"       
        ],
      Tkn.identStart  = letter,
      Tkn.identLetter = alphaNum <|> char '_',
      Tkn.reservedOpNames = ["+", "*", "^", "==", "=", ".", "!", "<", "<=", ">", ">="
                            , "&&", "||", "@", "/", "-", "%", "++"]
  }
  
lexer 		  = Tkn.makeTokenParser oopLangDef
identifier 	= Tkn.identifier lexer
iden        = identifier
reservedOp  = Tkn.reservedOp lexer
reOp        = reservedOp
operator    = Tkn.operator lexer
op          = operator
reserved    = Tkn.reserved lexer
re          = reserved
brackets	  = Tkn.brackets lexer
braces      = Tkn.braces lexer
angles 		  = Tkn.angles lexer
parens	    = Tkn.parens lexer
comma       = Tkn.comma  lexer
symbol      = Tkn.symbol lexer
semi        = Tkn.semi lexer
colon       = Tkn.colon lexer
dot         = Tkn.dot lexer
str         = Tkn.stringLiteral lexer

---------------------------------------
-- * Parser

type P a = Parsec String () a

pName :: P Var
pName = (try pAcc
     <|> try (ThisVar <$> (re "this" >> dot >> pName))
     <|> (SimpleVar <$> getPosition <*> iden)
  ) <?> "Name"
  where
    pAcc = do 
      (n:ns) <- iden `sepBy` dot
      p      <- getPosition
      let r = foldr Access (SimpleVar p n) (reverse ns)
      return r 
    
    swap (a, b) = (b, a)
    
pImportStmt :: P String
pImportStmt = (re "import" *> str <* semi) <?> "Import Statement"

pModule :: P (Module Var)
pModule = Module <$> many pImportStmt <*> many pClass

pClass :: P (Class Var)
pClass = (Class 
       <$> (re "class" >> iden)
       <*> pInheritance
       <*> braces pClassDef
       ) <?> "Class"
       
pInheritance :: P (Maybe (Class Var))
pInheritance = return Nothing

pClassDef :: P (ClassDef Var)
pClassDef = ClassDef <$> many pVarDef <*> many pMethodDef

pVarDef :: P (VarDef Var)
pVarDef = ((VarDef <$> (re "var" >> pName) <*> (colon >> pType)) <* semi) <?> "Variable Definition"

pMethodDef :: P (MethodDef Var)
pMethodDef = Method <$> iden 
                    <*> parens (pParms `sepBy` comma)
                    <*> option (Simple TyVoid) (colon >> pType)
                    <*> pStmtSeq
  where
    pParms = (,) <$> pName <*> (colon >> pType) 
                       
pStmtSeq :: P (StmtSeq Var)
pStmtSeq = braces $ many pStmt

pStmt :: P (Stmt Var)
pStmt = (try (StmtCall <$> pName <*> parens (pBoolExp `sepBy` comma) <* semi)
     <|> try (StmtIf <$> (re "if" >> pBoolExp) 
                     <*> (re "then" >> pStmtSeq)
                     <*> (option [] (re "else" >> pStmtSeq)))
     <|> try (StmtAssign <$> pLhs <*> (reOp "=" >> pBoolExp <* semi))
     <|> (StmtRet <$> (re "return" >> pBoolExp <* semi))) <?> "Statement"
    
pLhs :: P (Lhs Var)
pLhs = (LhsVar <$> pName)

binary  name fun assoc = Infix (do{ reservedOp name; return fun }) assoc
prefix  name fun       = Prefix (do{ reservedOp name; return fun })
postfix name fun       = Postfix (do{ reservedOp name; return fun })

prefixRE  name fun = Prefix (do{ re name; return fun })
binaryRE  name fun = Infix (do{ reservedOp name; return fun }) AssocNone

pBoolExp :: P (Exp Var)
pBoolExp = buildExpressionParser boolTable pCompExp
  where
    boolTable = 
      [ [ prefix "!" (ExpUnop Not) ]
      , [ binary "&&" (ExpBinop And) AssocLeft
        , binary "||" (ExpBinop Or)  AssocLeft
        ]
      ]

pCompExp :: P (Exp Var)
pCompExp = buildExpressionParser compTable (try boolLit <|> pArithExp)
  where
    boolLit = try (re "true" >> return (ExpLit $ LitBool True))
          <|> (re "false" >> return (ExpLit $ LitBool False))
  
    compTable = 
      [
        [ binary "==" (ExpBinop Eq)  AssocNone
        , binary "!=" (ExpBinop Neq) AssocNone
        , binary "<=" (ExpBinop Leq) AssocNone
        , binary "<"  (ExpBinop Le)  AssocNone
        , binary ">"  (ExpBinop Ge)  AssocNone
        , binary ">=" (ExpBinop Geq) AssocNone
        ]
      , [ binaryRE "in" (\x y -> ExpBtin ExpBIn [x, y]) ]
      ]
      
pArithExp :: P (Exp Var)
pArithExp = buildExpressionParser opTable (try atom <|> parens pBoolExp)
  where
    atom = try (ExpCall <$> pName <*> parens (pBoolExp `sepBy` comma))
       <|> try (ExpVar <$> pName)
       <|> (ExpLit <$> pLit) 
    
    opTable =
      [ [ prefix "-" (ExpUnop Inv) ]
      , [ binary "^" (ExpBinop Exp) AssocLeft ]
      , [ binary "*" (ExpBinop Mul) AssocLeft 
        , binary "/" (ExpBinop Div) AssocLeft
        , binary "%" (ExpBinop Mod) AssocNone
        ]
      , [ binary "+" (ExpBinop Sum) AssocLeft 
        , binary "-" (ExpBinop Sub) AssocLeft
        ]
      , [ binary "@" (\x y -> ExpBtin ExpBCons [x, y]) AssocRight
        , prefixRE "head" (ExpBtin ExpBHead . (:[])) 
        , prefixRE "tail" (ExpBtin ExpBTail . (:[]))
        , prefixRE "length" (ExpBtin ExpBLength . (:[]))
        , binary "++" (\x y -> ExpBtin ExpBCat [x, y]) AssocRight
        ]
      ]
                    
                    
pLit :: P Literal
pLit = (LitInt . fromInteger <$> (Tkn.integer lexer))
   <|> (LitList <$> brackets (pLit `sepBy` comma)) 


pType :: P (Type Var)
pType = try (Simple <$> pTySimple)
    <|> try (ListOf <$> (re "list" >> re "of" >> pType))
    <|> (Object <$> pName)
  where
    pTySimple = (re "int" >> return TyInt) 
            <|> (re "float" >> return TyFloat)
            <|> (re "void"  >> return TyVoid)
            
--------------------------------------------------------------------------------
-- ** Interface

-- |Parses a OOP file
oopParse :: FilePath -> IO (Either ParseError (Module Var))
oopParse f = runParser pModule () f <$> readFile f
        
--------------------------------------------------------------------------------
-- ** Debugging Utilities   
            
-- |Testing only, parses something with no annotations.    
pTest :: P b -> String -> Either ParseError b
pTest p = runParser p () "<interactive>"

pprTest :: (PPR.Pretty b) => P b -> String -> IO ()
pprTest p = putStrLn . either show (show . PPR.pretty) . pTest p

pTestF :: String -> IO ()
pTestF file = readFile file >>= putStrLn . ppr . pTest pClass
  where
    ppr = either show (show . PPR.pretty)
