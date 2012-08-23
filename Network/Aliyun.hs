{-# LANGUAGE OverloadedStrings
           , GeneralizedNewtypeDeriving
           , MultiParamTypeClasses
           , RecordWildCards
           , TypeFamilies
           , TupleSections
           #-}
module Network.Aliyun where

import qualified Prelude
import BasicPrelude
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy as L

import Control.Monad.Trans.Resource (ResourceT)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Control
import Control.Monad.Base
import qualified Control.Exception.Lifted as Lifted
import qualified Blaze.ByteString.Builder as B

import Data.Maybe (maybeToList)
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
  , hNeedMd5 :: Bool
  }

instance Default RequestHints where
    def = RequestHints "GET" "/" [] (RequestBodyLBS empty) False

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
        req' = authorizeRequest req (yunId conf) (yunKey conf) hNeedMd5
    askManager >>= httpLbs req'

listService :: Yun LByteString
listService =
    responseBody <$> lbsRequest def

putBucket :: ByteString -> Maybe ByteString -> Yun LByteString
putBucket name macl = do
    let hds = maybeToList $ ("x-oss-acl",) <$> macl
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
putObject bucket name body =
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
           size <- fromIntegral <$> liftIO (IO.hFileSize h)
           let src  = C.sourceHandle h C.$= C.map B.fromByteString
               body = RequestBodySource size src
           putObject bucket name body
        )

-- this is invalid: aliyun don't support chunked tranfer-encoding.
putObjectStream :: ByteString -> ByteString -> C.Source Yun B.Builder -> Yun LByteString
putObjectStream bucket name source =
    putObject bucket name (RequestBodySourceChunked source)

-- TODO put object multipart

getObject :: ByteString -> ByteString -> Yun LByteString
getObject bucket name = getObjectRange bucket name Nothing

getObjectRange :: ByteString -> ByteString -> Maybe ByteString -> Yun LByteString
getObjectRange bucket name mrange = do
    let hds = maybeToList $ ("Range",) . ("bytes="++) <$> mrange
    responseBody <$>
        lbsRequest def{ hPath    = S.concat ["/", bucket, "/", name]
                      , hHeaders = hds
                      }

copyObject :: ByteString -> ByteString -> ByteString -> Yun LByteString
copyObject bucket name source =
    responseBody <$>
        lbsRequest def{ hMethod  = "PUT"
                      , hPath    = S.concat ["/", bucket, "/", name]
                      , hHeaders = [("x-oss-copy-source", source)]
                      }

headObject :: ByteString -> ByteString -> Yun LByteString
headObject bucket name =
    responseBody <$>
        lbsRequest def{ hMethod = "HEAD"
                      , hPath   = S.concat ["/", bucket, "/", name]
                      }

deleteObject :: ByteString -> ByteString -> Yun LByteString
deleteObject bucket name =
    responseBody <$>
        lbsRequest def{ hMethod = "DELETE"
                      , hPath   = S.concat ["/", bucket, "/", name]
                      }

deleteObjects :: ByteString -> [ByteString] -> Bool -> Yun LByteString
deleteObjects bucket names verbose = do
    let body = L.fromChunks $
          [ "<Delete><Quiet>"
          , if verbose then "false" else "true"
          , "</Quiet>"
          ] ++
          concat [["<Object><Key>", name, "</Key></Object>"] | name <- names] ++
          [ "</Delete>" ]
    responseBody <$>
        lbsRequest def{ hMethod  = "POST"
                      , hPath    = S.concat ["/", bucket, "?delete"]
                      , hBody    = RequestBodyLBS body
                      , hNeedMd5 = True
                      }
