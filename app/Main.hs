module Main where

import Rsa
import Text.Printf
import System.IO
import Data.Maybe
import Data.Function
import Control.Monad.State.Lazy

data MenuChoice = Generate
                | LoadKeyPair
                | LoadPubKey
                | Encrypt
                | Decrypt
                | Quit
                | Help
                deriving (Enum, Read, Show)


data KeyState = SKeyPair KeyPair | SPubKey PubKey


pubkeyS :: KeyState -> PubKey
pubkeyS (SKeyPair key) = pubkey $ key
pubkeyS (SPubKey key) = key


maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads


execWithKey :: KeyState -> MenuChoice -> IO ()
execWithKey key Encrypt = do
    msg <- getLine
    let cipher = encryptascii msg $ pubkeyS key
    case cipher of
        Just cipher -> print cipher
        Nothing -> putStrLn "Invalid input"
execWithKey (SKeyPair key) Decrypt = do
    msgstr <- getLine
    let msg = maybeRead msgstr
    case msg of
        Just msg -> do
            let plain = decryptascii msg $ privkey key
            case plain of
                Just plain -> putStrLn $ "< " ++ plain
                Nothing -> putStrLn "Invalid input"
        Nothing -> putStrLn "Please enter a numerical ciphertext"


delegateAction :: MenuChoice -> StateT (Maybe KeyState) IO ()
delegateAction choice = do
    key <- get
    case key of
        Just key ->
            liftIO $ execWithKey key choice
        Nothing ->
            liftIO $ putStrLn "No key loaded or generated"


keyFormat = "(Public Exponent, Private Exponent, Public Modulus)"


exec :: MenuChoice -> StateT (Maybe KeyState) IO () 
exec Generate = do
    key <- liftIO $ Rsa.generatePair 2048
    put $ Just (SKeyPair key)
    liftIO $ putStrLn keyFormat
    liftIO $ print key
exec LoadKeyPair = do
    keystr <- liftIO $ getLine
    let key = maybeRead keystr
    case key of
        Just key -> put $ Just $ SKeyPair key
        Nothing -> liftIO $ putStrLn $ "Please input a key of the format " ++ keyFormat
exec LoadPubKey = do
    keystr <- liftIO $ getLine
    let key = maybeRead keystr
    case key of
        Just key -> put $ Just $ SPubKey key
        Nothing -> liftIO $ putStrLn "Please input a key of the format (Public Exponent, Modulus)"
exec a@Encrypt = delegateAction a
exec a@Decrypt = delegateAction a
exec Help = liftIO $ mapM_ print [ Generate .. ]
exec _ = liftIO $ putStrLn "Not implemented"

mainloop :: (Maybe KeyState) -> IO ()
mainloop state = do
        putStr "> " 
        choicestr <- getLine
        case maybeRead choicestr of 
            Just choice ->
                case choice of 
                    Quit -> putStrLn "Bye!"
                    x -> do
                        nstate <- execStateT (exec x) state
                        mainloop nstate
            Nothing -> do 
                putStrLn "Invalid command. Try \"Help\""
                mainloop state

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    mainloop Nothing
