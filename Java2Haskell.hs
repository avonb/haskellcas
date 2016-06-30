module Java2Haskell where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.MessagePack as MSG

import Foreign.C

-- import FFI.Anything.TH (deriveCallable)
import FFI.Anything.TypeUncurry.Msgpack

main:: IO()
main = Prelude.putStrLn "HelloWorld"


add1:: Int -> Int
add1 i = i+1

foreign export ccall add1_export :: CString -> IO CString
add1_export = export add1