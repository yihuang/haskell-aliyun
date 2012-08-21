{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, StandaloneDeriving, MultiParamTypeClasses #-}
module Network.Aliyun where

import qualified Prelude
import BasicPrelude
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as S

import Control.Monad.Trans.Resource (ResourceT)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Control
--import Control.Monad.Base.Control
import Control.Monad.Base

import Data.Default (def)
import Data.Time (getCurrentTime, formatTime)
import qualified Data.Conduit as C

import System.Locale (defaultTimeLocale)
import Network.HTTP.Conduit

import Network.Aliyun.Utils

data YunConf = YunConf
  { yunHost :: ByteString
  , yunId   :: ByteString
  , yunKey  :: ByteString
  }

type YunEnv = YunConf

newtype Yun a = Yun { unYun :: ReaderT YunEnv IO a }
    deriving ( Functor, Applicative, Monad, MonadIO
             )

instance MonadBase IO Yun where
    liftBase = liftIO

askConf = Yun ask
asksConf f = f <$> askConf

runYun :: YunConf -> Yun a -> IO a
runYun conf yun = runReaderT (unYun yun) conf

formatNow :: IO ByteString
formatNow = S.pack . formatTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S GMT" <$> getCurrentTime

getRequest :: ByteString -> Yun (Request m)
getRequest path = do
    conf <- askConf
    time <- liftIO formatNow
    let req = def { host = yunHost conf
                  , method = "GET"
                  , path = path
                  , requestHeaders = [("Date", time)]
                  }
    return $ authorizeRequest req (yunId conf) (yunKey conf)

listService :: Yun LByteString
listService = do
    req  <- getRequest "/"
    liftIO $ withManager $ \man ->
        responseBody <$> httpLbs req man
