-- | Generates keys

{-# LANGUAGE OverloadedStrings #-}

module Main where

import CEC.Keys

import Data.Aeson
import Data.Text.Lazy.Encoding
import qualified Data.Text.Lazy.IO as T
import System.Environment
import System.Exit

usage :: IO ()
usage = do
  putStrLn "Usage: tgcec-keygen [--help | --gen]"

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    ["--gen"] -> do
      (pk,sk) <- generateKeyPair
      ek <- generateKey
      T.putStrLn $ decodeUtf8 $ encode $ object
        [ "public-key" .= pk
        , "secret-key" .= sk
        , "encrypt-key" .= ek
        ]
    _ -> usage
