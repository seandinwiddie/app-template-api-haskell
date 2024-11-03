{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Web.Scotty (scotty, middleware, get, param, status, ScottyM)
import qualified Web.Scotty as S
import Network.Wai.Middleware.Cors (simpleCors)
import Network.HTTP.Types.Status (status404)
import Data.Aeson (Value(..), decode, object, (.=), Object)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as Key
import Data.ByteString.Lazy as B
import GHC.Generics
import Control.Monad.IO.Class
import qualified Data.Text.Lazy as T
import System.IO

-- State management
data AppState = AppState {
    initialState :: Value
} deriving (Show)

-- Main application
main :: IO ()
main = do
    -- Read initial state from JSON file
    jsonData <- B.readFile "src/data/initialState.json"
    case decode jsonData of
        Nothing -> putStrLn "Failed to parse JSON"
        Just state -> do
            let appState = AppState state
            scotty 3000 $ do
                -- Enable CORS
                middleware simpleCors

                -- Home route
                get "/" $ do
                    S.json $ object ["message" .= ("Welcome to the API" :: String)]

                -- Status route
                get "/status" $ do
                    S.json $ object ["status" .= ("OK" :: String)]

                -- Get all data
                get "/data" $ do
                    S.json $ initialState appState

                -- Dynamic route for specific keys
                get "/:key" $ do
                    keyText <- param "key"
                    let key = Key.fromText keyText
                    case initialState appState of
                        Object obj -> case KM.lookup key obj of
                            Just value -> S.json $ object [key .= value]
                            Nothing -> do
                                status status404
                                S.json $ object ["error" .= ("Key not found" :: String)]
                        _ -> do
                            status status404
                            S.json $ object ["error" .= ("Invalid state format" :: String)]
