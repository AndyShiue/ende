module Ast ( Operator(..)
           , Term(..)
           , FunctionCall(..)
           , Statement(..)
           , Block(..)
           ) where
import Foreign.Ptr
import Foreign.StablePtr

data Operator = Add | Sub | Mul | Div
data Term = Literal Integer
          | Var String
          | Infix Term Operator Term
          | Call FunctionCall [Term]
          | Scope Block
          | While Term Block
data FunctionCall = FunctionCall { name :: String
                                 , arity :: Int
                                 }
data Statement = TermSemicolon Term | Let String Term | LetMut String Term | Mutate String Term
data Block = Block { stmts :: [Statement]
                   , end :: Term
                   }
block :: Block
block = Block { stmts = [TermSemicolon (Literal 123)]
              , end = Var "123"
              }

getTree :: IO (Ptr ())
getTree = do
  ptr <- newStablePtr block
  return $ castStablePtrToPtr ptr
foreign export ccall getTree :: IO (Ptr ())
