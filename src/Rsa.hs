module Rsa
    ( generatePair,
      encryptascii,
      decryptascii,
      pubkey,
      privkey,
      KeyPair,
      PubKey,
      PrivKey,
    ) where

import Math.NumberTheory.GCD
import Data.Bits
import Data.Maybe
import Data.List
import System.Random
import Data.Function
import Math.NumberTheory.Primes

modExp :: Integer -> Integer -> Integer -> Integer
modExp b 0 m = 1
modExp b e m = t * modExp ((b * b) `mod` m) (shiftR e 1) m `mod` m
               where t = if testBit e 0 then b `mod` m else 1

rndPrime :: Int -> IO Integer
rndPrime bits =
    fix $ \again -> do
        x <- fmap (.|. 1) $ randomRIO (2^(bits - 1), 2^bits - 1)
        if isPrime x then return x else again

type Message = Integer
type Modulus = Integer
type Public = Integer
type Private = Integer
type Cipher = Integer

encrypt :: Message -> Public -> Modulus -> Maybe Cipher
encrypt c k m
    | c < 0 || c >= m = Nothing
    | otherwise       = Just $ modExp c k m 

decrypt :: Cipher -> Private -> Modulus -> Maybe Message
decrypt = encrypt

stringify :: Message -> String
stringify 0 = []
stringify n = (toEnum $ fromIntegral $ mod n 256) : stringify (div n 256)

numberify :: String -> Message
numberify [] = 0
numberify (x:xs) = (toInteger . fromEnum) x + 256 * numberify xs

decryptascii :: Cipher -> PrivKey -> Maybe String
decryptascii c (d, m) = do
    cipher <- encrypt c d m
    return $ stringify cipher

encryptascii :: String -> PubKey -> Maybe Cipher
encryptascii s (e, m) = encrypt (numberify s) e m

type KeyPair = (Public, Private, Modulus)
type PubKey = (Public, Modulus)
type PrivKey = (Private, Modulus)

pubkey :: KeyPair -> PubKey
pubkey (e, d, m) = (e, m)

privkey :: KeyPair -> PrivKey
privkey (e, d, m) = (d, m)

type Prime = Integer
calculatePair :: Prime -> Prime -> KeyPair
calculatePair p q = (pub,priv,p*q)
    where phi = (p-1)*(q-1)
          pub = fromJust (find ((== 1) . (`gcd` phi)) [n,(n-1)..2])
          (1,g,_) = extendedGCD pub phi
          priv = phi + g
          n = if phi > 65537 then 65537 else 7 

generatePair :: Int -> IO KeyPair
generatePair b = do p <- rndPrime b
                    fix $ \again -> do 
                          q <- rndPrime b 
                          if p /= q then return (calculatePair p q) else again
