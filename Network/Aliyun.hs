{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, StandaloneDeriving, MultiParamTypeClasses, RecordWildCards, TypeFamilies #-}
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
import qualified Control.Exception.Lifted as Lifted
import qualified Blaze.ByteString.Builder as B

import Data.Default (Default(def))
import Data.Time (getCurrentTime, formatTime)
import qualified Data.Conduit as C
import qualified Data.Conduit.Binary as C
import qualified Data.Conduit.List as C

import qualified System.IO as IO
import System.Locale (defaultTimeLocale)
import Network.HTTP.Conduit
import qualified Network.HTTP.Types as W

import Network.Aliyun.Utils

data YunConf = YunConf
  { yunHost :: ByteString
  , yunId   :: ByteString
  , yunKey  :: ByteString
  }

type YunEnv = (YunConf, Manager)

newtype Yun a = Yun { unYun :: ReaderT YunEnv (ResourceT IO) a }
    deriving (Functor, Applicative, Monad, MonadIO, C.MonadResource, C.MonadThrow, C.MonadUnsafeIO)

instance MonadBase IO Yun where
    liftBase = Yun . liftBase

instance MonadBaseControl IO Yun where
    newtype StM Yun a = YunStM { unYunStM :: StM (ReaderT YunEnv (ResourceT IO)) a }
    liftBaseWith f = Yun . liftBaseWith $ \runInBase -> f $ liftM YunStM . runInBase . unYun
    restoreM = Yun . restoreM . unYunStM

askConf     = fst <$> Yun ask
askManager  = snd <$> Yun ask
asksConf f  = f <$> askConf

runYun :: YunConf -> Yun a -> IO a
runYun conf yun = withManager $ \man -> runReaderT (unYun yun) (conf, man)

formatNow :: IO ByteString
formatNow = S.pack . formatTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S GMT" <$> getCurrentTime

data RequestHints = RequestHints
  { hMethod  :: ByteString
  , hPath    :: ByteString
  , hHeaders :: W.RequestHeaders
  , hBody    :: RequestBody Yun
  }

instance Default RequestHints where
    def = RequestHints "GET" "/" [] (RequestBodyLBS empty)

lbsRequest :: RequestHints -> Yun (Response LByteString)
lbsRequest RequestHints{..} = do
    conf <- askConf
    time <- liftIO formatNow
    let req = def { host            = yunHost conf
                  , method          = hMethod
                  , path            = hPath
                  , requestHeaders  = ("Date", time) : hHeaders
                  , requestBody     = hBody
                  }
        req' = authorizeRequest req (yunId conf) (yunKey conf)
    askManager >>= httpLbs req'

listService :: Yun LByteString
listService =
    responseBody <$> lbsRequest def

putBucket :: ByteString -> Maybe ByteString -> Yun LByteString
putBucket name macl = do
    let hds = maybe [] (\acl -> [("x-oss-acl", acl)]) macl
    responseBody <$>
        lbsRequest def{ hMethod  = "PUT"
                      , hPath    = "/"++name
                      , hHeaders = hds
                      }

getBucket :: ByteString -> Yun LByteString
getBucket name =
    responseBody <$>
        lbsRequest def{ hPath = "/"++name }

getBucketACL :: ByteString -> Yun LByteString
getBucketACL name =
    responseBody <$>
        lbsRequest def{ hPath = S.concat ["/", name, "?acl"] }

deleteBucket :: ByteString -> Yun LByteString
deleteBucket name =
    responseBody <$>
        lbsRequest def{ hMethod = "DELETE"
                      , hPath   = "/"++name
                      }

putObject :: ByteString -> ByteString -> RequestBody Yun -> Yun LByteString
putObject bucket name body = do
    responseBody <$>
        lbsRequest def{ hMethod = "PUT"
                      , hPath   = S.concat ["/", bucket, "/", name]
                      , hBody   = body
                      }

putObjectStr :: ByteString -> ByteString -> LByteString -> Yun LByteString
putObjectStr bucket name body = putObject bucket name (RequestBodyLBS body)

putObjectFile :: ByteString -> ByteString -> IO.FilePath -> Yun LByteString
putObjectFile bucket name path =
    Lifted.bracket
        (liftIO $ IO.openBinaryFile path IO.ReadMode)
        (liftIO . IO.hClose)
        (\h -> do
           size <- fromIntegral <$> liftIO (IO.hTell h)
           let src  = C.sourceHandle h C.$= C.map B.fromByteString
               body = RequestBodySource size src
           putObject bucket name body
        )
