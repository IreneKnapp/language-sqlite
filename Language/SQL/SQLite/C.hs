{-# LANGUAGE ForeignFunctionInterface, TemplateHaskell #-}
module Language.SQL.SQLite.C where

-- For sanity's sake, keep these imports in synch with the ones in CTH.hs.
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS hiding (length)
import Data.Word
import Foreign.C
import Foreign.Ptr
import Foreign.StablePtr
import Foreign.Storable
import Language.SQL.SQLite
import Language.SQL.SQLite.CTH

foreign export ccall "hsql_free" free :: Ptr () -> IO ()
free :: Ptr () -> IO ()
free pointer = do
  let stablePointer = castPtrToStablePtr pointer
  freeStablePtr stablePointer

foreign export ccall "hsql_show" showC :: Ptr a -> Ptr CSize -> Ptr CString -> IO ()
showC :: Ptr a -> Ptr CSize -> Ptr CString -> IO ()
showC objectPointer sizePointer bufferPointer = do
  let bytestring = BS.fromString $ show $ castPtr objectPointer
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

exportShowTokensForType 'Type

-- foreign export ccall "hsql_Type" typeC :: Ptr () -> Ptr () -> 