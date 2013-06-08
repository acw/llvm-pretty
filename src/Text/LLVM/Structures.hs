module Text.LLVM.Structures(
         Structure
       , declareStructure
       , structureType
       , access
       , set
       , setMany
       , alloc
       , allocInit
       , llvmMalloc, llvmAlloca
       )
 where

import Control.Monad
import Text.LLVM

data Structure = Structure String [(String, (Int, Type))]

declareStructure :: Ident -> [(String, Type)] -> LLVM Structure
declareStructure name@(Ident n) fields = do
  emitTypeDecl $ TypeDecl {
    typeName  = name
  , typeValue = structT types
  }
  return $ Structure n (zip names (zip [0..] types))
 where
  names = map fst fields
  types = map snd fields

structureType :: Structure -> Type
structureType (Structure n _) = refT (Ident n)

access :: IsValue a => Structure -> String -> Typed a -> BB (Typed Value)
access (Structure n fields) name item =
  case lookup name fields of
    Nothing    -> fail $ "Cannot access field " ++ show name ++ " of " ++ n
    Just (i,t) -> do
      ptr <- getelementptr (ptrT t) item [i32 0, i32 i]
      load ptr

set :: (IsValue a, IsValue b) =>
       Structure -> String -> Typed a -> Typed b ->
       BB ()
set str name item x = setMany str item [(name, toValue `fmap` x)]

alloc :: Structure -> (Typed Value -> BB (Typed Value)) -> BB (Typed Value)
alloc (Structure n fields) malloc = do
  ptr   <- getelementptr (ptrT strt) (nullPtr strt) [iT 64 -: (1::Integer)]
  size  <- ptrtoint ptr (iT 64)
  ptri8 <- malloc size
  bitcast ptri8 (ptrT strt)
 where strt = refT (Ident n)

setMany :: IsValue a =>
           Structure -> Typed a -> [(String, Typed Value)] ->
           BB ()
setMany str@(Structure n fields) strval fvals = do
  fData  <- mapM fieldTriple fvals
  fData' <- forM fData getFieldPtrs
  forM_ fData' $ \ (ptr, val) -> store val ptr
 where
  fieldTriple :: (String, Typed Value) -> BB (Int, Type, Typed Value)
  fieldTriple (name, val) = do
    (i, t) <- fieldInfo name
    return (i, t, val)
  --
  fieldInfo :: String -> BB (Int, Type)
  fieldInfo   name =
    case lookup name fields of
      Nothing    -> fail $ "Cannot access field " ++ show name ++ " of " ++ n
      Just res   -> return res
  --
  getFieldPtrs :: (Int, Type, Typed Value) -> BB (Typed Value, Typed Value)
  getFieldPtrs (i, t, val) = do
    ptr <- getelementptr (ptrT t) strval [i32 0, i32 i]
    return (ptr, val)

allocInit :: Structure ->
             (Typed Value -> BB (Typed Value)) ->
             [(String, Typed Value)] ->
             BB (Typed Value)
allocInit str malloc fvals = do
  strval <- alloc str malloc
  setMany str strval fvals
  return strval

llvmMalloc :: Typed Value -> BB (Typed Value)
llvmMalloc size = call (ptrT (iT 8)) (Symbol "malloc") [size]

llvmAlloca :: Typed Value -> BB (Typed Value)
llvmAlloca size = alloca (iT 8) (Just size) Nothing

i32 :: Int -> Typed Value
i32 x = iT 32 -: int x
