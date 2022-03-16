module Hashable32( Hash
                 , Hashable(..)
                 , VisiHash(..)
                 , showHash, showsHash
                 ) where
-- import Data.Bits(shiftL,shiftR,testBit,xor)
import Data.List(foldl')
import Data.Word(Word32)
import Text.Printf

type Hash = Word32

showHash :: Hash -> String
showHash h = printf "%#010x" h
showsHash = showString . showHash

newtype VisiHash = VH Hash
instance Show VisiHash where
  show (VH h) = showHash h

-- | Combine two hash values. 0 is the left identity
combine :: Hash -> Hash -> Hash
-- combine a b = xor (a*16777619) b
combine a b = a*16777619+ b

defaultSalt :: Hash
defaultSalt = 0xdeadbeef -- 0xbada55bb

class Hashable a where
  hash :: a -> Hash
  hash = hashWithSalt defaultSalt

  hashWithSalt :: Hash -> a -> Hash
  hashWithSalt s = combine s . hash

instance Hashable Word32 where
  hash = id

instance Hashable Int where
  hash = fromIntegral

instance Hashable Char where
  hash = fromIntegral . fromEnum

instance (Hashable a, Hashable b) => Hashable (a,b) where
  hash (a,b) = hash a `combine` hash b

instance Hashable a => Hashable [a] where
  hash [] = 0
  hash [x] = hash x
  hash (x:xs) = hash x `combine` hash xs

{-
data SP = SP !Hash !Hash
instance Hashable a => Hashable [a] where
  hashWithSalt salt arr = finalise (foldl' step (SP salt 0) arr)
      where
        finalise :: SP -> Hash
        finalise (SP s l) = hashWithSalt s l
        -- step :: SP -> a -> SP
        step (SP s l) x   = SP (hashWithSalt s x) (l + 1)
-}
