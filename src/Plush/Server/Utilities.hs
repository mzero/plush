{-
Copyright 2012 Google Inc. All Rights Reserved.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}

{-# LANGUAGE OverloadedStrings #-}

module Plush.Server.Utilities (
    -- * Responses
    notFound, badRequest,
    respApp,

    -- * JSON Applications
    JsonApplication,
    jsonApp,
    returnJson, returnResponse,

    -- * Keyed Applications
    KeyedRequest,
    keyedApp,
    )
    where

import qualified Blaze.ByteString.Builder.ByteString as B
import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Data.Aeson
import Network.HTTP.Types
import Network.Wai


-- | 404 \"File not found\"
notFound :: Response
notFound = responseLBS status404
                        [ ("Content-Type", "text/plain") ] "File not found"

-- | 400 \"Bad Request\"
badRequest :: Response
badRequest = responseLBS status400
                        [ ("Content-Type", "text/plain") ] "Bad request"

-- | An application that returns a fixed response no matter the request.
respApp :: Response -> Application
respApp resp _ = ($ resp)


-- | Like 'Application', but takes a request type @/a/@ and returns a response
-- type @/b/@.
--
-- Both types are indended to be trasmitted as JSON, so @/a/@ should be an
-- instance of 'FromJSON', and @/b/@ should be an instance of 'ToJSON'.
--
-- The actual type returns @'Either' 'Response' /b/@ so that if an error
-- occurs, the application can return an error response of some sort, rather
-- than JSON.
type JsonApplication a b = a -> IO (Either Response b)

-- | Transforms a 'JsonApplication' into an 'Application'. If a value of type
-- @/a/@ cannot be parsed from the JSON in the request, then 'badRequest'
-- results.
jsonApp :: (FromJSON a, ToJSON b) => JsonApplication a b -> Application
jsonApp jApp req respond = do
    body <- strictRequestBody req
    case decode body of
        Nothing -> respond badRequest
        Just a -> jApp a >>= respond . either id jsonResponse
  where
    jsonResponse =
        responseBuilder status200 [ ("Content-Type", "application/json") ]
        . B.fromLazyByteString . encode

-- | Utility for returning a JSON response in a JsonApplication
returnJson :: (ToJSON b) => b -> IO (Either Response b)
returnJson = return . Right

-- | Utility for returning 'Response' response in a JsonApplication
returnResponse :: (ToJSON b) => Response -> IO (Either Response b)
returnResponse = return . Left



-- | A request with the form @{ key: /k/, req: /r/ }@, where @/r/@ is an
-- inner request.
data KeyedRequest a = KeyedRequest String a
instance (FromJSON a) => FromJSON (KeyedRequest a) where
    parseJSON (Object v) = KeyedRequest <$> v .: "key" <*> v .: "req"
    parseJSON _ = mzero

-- | Transforms a 'JsonApplication' into one that takes a 'KeyedRequest'.
-- The first argument is the /key/, and if it matches the key in the request
-- then the inner value is passed on to the inner application. Otherwise,
-- a 'badRequest' results.
keyedApp :: (FromJSON a, ToJSON b) => String -> JsonApplication a b
    -> JsonApplication (KeyedRequest a) b
keyedApp key jApp (KeyedRequest key' a) | key' == key = jApp a
keyedApp _ _ _ = returnResponse badRequest
