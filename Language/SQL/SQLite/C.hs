{-# LANGUAGE ForeignFunctionInterface, TemplateHaskell, QuasiQuotes #-}
module Language.SQL.SQLite.C where

import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS hiding (length)
import Data.Word
import Foreign.C
import Foreign.Ptr
import Foreign.StablePtr
import Foreign.Storable
import Language.Haskell.Semiquotation

import Language.SQL.SQLite
import Language.SQL.SQLite.CTH

foreign export ccall "hsql_free" free :: Ptr () -> IO ()
free :: Ptr () -> IO ()
free pointer = do
  let stablePointer = castPtrToStablePtr pointer
  freeStablePtr stablePointer

foreign export ccall "hsql_show" showC :: Ptr () -> Ptr CSize -> Ptr CString -> IO ()
showC :: Ptr () -> Ptr CSize -> Ptr CString -> IO ()
showC objectPointer sizePointer bufferPointer = do
  object <- deRefStablePtr $ castPtrToStablePtr objectPointer :: IO [Token]
  let bytestring = BS.fromString $ show object
  if sizePointer /= nullPtr
    then poke sizePointer $ fromIntegral $ BS.length bytestring + 1
    else return ()   
  if bufferPointer /= nullPtr
    then do
      mapM_ (\(word, index) -> pokeElemOff (castPtr bufferPointer)
                                           index
                                           word)
            $ zip (BS.unpack bytestring) [0..]
      pokeElemOff (castPtr bufferPointer) (BS.length bytestring) (0 :: Word8)
    else return ()

exportType
  ''Type
  [("type", 'Type,
    [("identifier", [t| UnqualifiedIdentifier |]),
     ("sizes", [t| MaybeTypeSize |])])]
exportShowTokensForType ''Type
{-

-- It needs to expand several to definitions:

-- A function for each type constructor (in this case we are fortunate enough to only
-- have one) that constructs the type.
foreign export ccall "hsql_new_type"
  newType :: Ptr () -> Ptr () -> IO (Ptr ())
newType :: Ptr () -> Ptr () -> IO (Ptr ())
newType a stuff = do
  a' <- getGeneric (unused :: UnqualifiedIdentifier) a
  stuff' <- getGeneric (unused :: Maybe ... the long type above) stuff
  newStablePointer (Type a' stuff') >>= return . castStablePtrToPtr

-- Now one accessor function for each parameter of that type constructor.
foreign export ccall "hsql_get_type_identifier"
  getTypeIdentifier :: Ptr () -> IO (Ptr ())
getTypeIdentifier :: Ptr () -> IO (Ptr ())
getTypeIdentifier type' = do
  (Type identifier _) <- getGeneric (unused :: Type) type'
  resultStablePointer <- newStablePointer $ identifier
  return $ castStablePtrToPtr resultStablePointer


-- Another accessor.
foreign export ccall "hsql_get_type_stuff"
  getTypeStuff :: Ptr () -> IO (Ptr ())
getTypeStuff :: Ptr () -> IO (Ptr ())
getTypeStuff type' = do
  (Type _ stuff) <- getGeneric (unused :: Type) type'
  resultStablePointer <- newStablePointer $ stuff
  return $ castStablePtrToPtr resultStablePointer
-}
