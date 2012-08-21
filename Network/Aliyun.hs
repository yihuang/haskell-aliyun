{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, StandaloneDeriving, MultiParamTypeClasses #-}
module Network.Aliyun where

import qualified Prelude
import BasicPrelude
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Base64 as B64

import Control.Monad.Trans.Resource (ResourceT)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Control
--import Control.Monad.Base.Control
import Control.Monad.Base

import Data.Default (def)
import Data.Time (getCurrentTime, formatTime)
import Data.HMAC (hmac_sha1)
import qualified Data.Conduit as C

import System.Locale (defaultTimeLocale)
import qualified Network.HTTP.Types as W
import Network.HTTP.Conduit

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

authorize :: Request m -> Yun (Request m)
authorize req = do
    conf <- askConf
    let date = fromMaybe "" $ lookup "Date" (requestHeaders req)
        s = S.concat [ method req
                     , "\n", "", "\n", "", "\n"
                     , date, "\n", ""
                     , path req
                     ]
        auth = S.concat [ "OSS ", yunId conf, ":"
                        , B64.encode $ B.pack (hmac_sha1 (B.unpack (yunKey conf)) (B.unpack s))
                        ]
    return $ req { requestHeaders = ("Authorization", auth) : requestHeaders req }

getRequest :: ByteString -> Yun (Request m)
getRequest path = do
    conf <- askConf
    time <- liftIO formatNow
    let req = def { host = yunHost conf
                  , method = "GET"
                  , path = path
                  , requestHeaders = [ ("Date", time)
                                     , ("Host", yunHost conf)
                                     ]
                  }
    authorize req

listService :: Yun LByteString
listService = do
    req  <- getRequest "/"
    liftIO $ withManager $ \man ->
        responseBody <$> httpLbs req man
