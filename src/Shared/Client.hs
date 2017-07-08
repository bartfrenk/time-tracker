{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}

module Shared.Client
  ( module Shared.Client
  , Query
  , MonadReader
  ) where

import           BasicPrelude
import           Control.Monad.Catch
import           Control.Monad.Reader
import qualified Data.ByteString.Char8  as C8
import qualified Data.ByteString.Lazy   as Lazy
import           Network.HTTP.Client
import           Network.HTTP.Types.URI


mkDefaultClientEnv :: Config -> IO ClientEnv
mkDefaultClientEnv config =
  ClientEnv config <$> newManager defaultManagerSettings

data Config = Config
  { baseURL  :: String,
    user     :: ByteString,
    password :: ByteString
  }

data ClientEnv = ClientEnv
  { config  :: Config
  , manager :: Manager
  }

data Method
  = GET
  | POST

methodToByteString :: Method -> ByteString
methodToByteString GET  = "GET"
methodToByteString POST = "POST"

runClientT :: ReaderT ClientEnv m a -> ClientEnv -> m a
runClientT = runReaderT

runClient :: ReaderT ClientEnv IO a -> ClientEnv -> IO a
runClient = runReaderT

type ClientMonad m =
  (MonadThrow m, MonadReader ClientEnv m, MonadIO m)

setMethod :: Method -> Request -> Request
setMethod method request = request { method = methodToByteString method }

mkRequest :: (MonadThrow m, MonadReader ClientEnv m)
          => Method -> String -> m Request
mkRequest = mkRequestWithParse parseRequest

mkRequestThrow :: (MonadThrow m, MonadReader ClientEnv m)
               => Method -> String -> m Request
mkRequestThrow = mkRequestWithParse parseUrlThrow

mkRequestWithParse :: (MonadThrow m, MonadReader ClientEnv m)
                   => (String -> m Request) -> Method -> String -> m Request
mkRequestWithParse parse method path = do
  baseURL <- reader (baseURL . config)
  request <- parse (baseURL ++ path)
  user <- reader (user . config)
  password <- reader (password . config)
  return $ setMethod method
         $ applyBasicAuth user password request

http :: ClientMonad m => Request -> m (Response Lazy.ByteString)
http request = reader manager >>= liftIO . httpLbs request

replace :: Char -> Char -> ByteString -> ByteString
replace old new = C8.map (\c -> if c == old then new else c)

encodeQuery :: Query -> ByteString
encodeQuery query =
  replace ' ' '+' $ urlDecode False (renderQuery True query)
