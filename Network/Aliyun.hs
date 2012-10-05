{-# LANGUAGE OverloadedStrings
           , GeneralizedNewtypeDeriving
           , MultiParamTypeClasses
           , RecordWildCards
           , TypeFamilies
           , TupleSections
           #-}
module Network.Aliyun
  ( Yun(..)
  , YunConf(..)
  , YunEnv
  , BucketQuery(..)
  , runYun
  , listService
  , putBucket
  , getBucket
  , getBucketContents
  , getBucketACL
  , deleteBucket
  , putObject
  , putObjectStr
  , putObjectStream
  , putObjectFile
  , getObject
  , getObjectRange
  , copyObject
  , headObject
  , deleteObject
  , deleteObjects
  , module Network.Aliyun.Types
  ) where

import qualified Prelude as P
import BasicPrelude
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Control.Monad.Trans.Resource (ResourceT)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Control
import Control.Monad.Base
import qualified Control.Exception.Lifted as Lifted
import qualified Blaze.ByteString.Builder as B

import Data.Maybe (maybeToList)
import Data.Default (Default)
import Data.Time (getCurrentTime, formatTime)
import qualified Data.Conduit as C
import qualified Data.Conduit.Binary as C
import qualified Data.Conduit.List as C
import Data.Aeson (FromJSON)
import Text.XML.ToJSON (parseXML)

import qualified System.IO as IO
import System.Locale (defaultTimeLocale)
import Network.HTTP.Conduit
import qualified Network.HTTP.Types as W

import Network.Aliyun.Types
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

askConf :: Yun YunConf
askConf     = fst <$> Yun ask
askManager :: Yun Manager
askManager  = snd <$> Yun ask
asksConf :: (YunConf -> a) -> Yun a
asksConf f  = f <$> askConf

runYun :: YunConf -> Yun a -> IO a
runYun conf yun = withManager $ \man -> runReaderT (unYun yun) (conf, man)

runYunWithManager :: Manager -> YunConf -> Yun a -> ResourceT IO a
runYunWithManager man conf yun = runReaderT (unYun yun) (conf, man)

formatNow :: IO ByteString
formatNow = S.pack . formatTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S GMT" <$> getCurrentTime

data RequestHints = RequestHints
  { hMethod  :: ByteString
  , hPath    :: ByteString
  , hQuery   :: ByteString
  , hHeaders :: W.RequestHeaders
  , hBody    :: RequestBody Yun
  , hNeedMd5 :: Bool
  }

instance Default RequestHints where
    def = RequestHints "GET" "/" "" [] (RequestBodyLBS empty) False

lbsRequest :: RequestHints -> Yun (Response LByteString)
lbsRequest RequestHints{..} = do
    conf <- askConf
    time <- liftIO formatNow
    let req = def { host            = yunHost conf
                  , method          = hMethod
                  , path            = hPath
                  , queryString     = hQuery
                  , requestHeaders  = ("Date", time) : hHeaders
                  , requestBody     = hBody
                  }
        req' = authorizeRequest req (yunId conf) (yunKey conf) hNeedMd5
    askManager >>= httpLbs req'

xmlResponse :: (C.MonadThrow m, FromJSON a) => Response LByteString -> m a
xmlResponse = parseXML . responseBody

listService :: Yun BucketList
listService =
    xmlResponse =<< lbsRequest def

putBucket :: ByteString -> Maybe ByteString -> Yun LByteString
putBucket name macl = do
    let hds = maybeToList $ ("x-oss-acl",) <$> macl
    responseBody <$>
        lbsRequest def{ hMethod  = "PUT"
                      , hPath    = "/"++name
                      , hHeaders = hds
                      }

data BucketQuery = BucketQuery
  { qryPrefix       :: Text
  , qryMaxKeys      :: Int
  , qryMarker       :: Maybe Text
  , qryDelimiter    :: Maybe Char
  }
instance Default BucketQuery where
    def = BucketQuery "" 1000 Nothing Nothing

getBucket :: ByteString -> BucketQuery -> Yun Bucket
getBucket name qry =
    xmlResponse =<< lbsRequest def{ hPath = "/"++name, hQuery=qs }
  where
    qs = T.encodeUtf8 $ T.concat
           [ "prefix=", qryPrefix qry
           , "&max-keys=", show (qryMaxKeys qry)
           , maybe "" ("&marker="++) (qryMarker qry)
           , maybe "" (("&delimiter="++) . T.singleton) (qryDelimiter qry)
           ]

getBucketContents :: ByteString -> BucketQuery -> C.Source Yun BucketContent
getBucketContents name query = loop query
  where
    loop qry = do
        bucket <- lift (getBucket name qry)
        mapM_ (C.yield . ContentDirectory) (bucketDirectories bucket)
        mapM_ (C.yield . ContentFile) (bucketContents bucket)
        when (bucketIsTruncated bucket) $
            loop qry{qryMarker=bucketNextMarker bucket}

getBucketACL :: ByteString -> Yun BucketACL
getBucketACL name =
    xmlResponse =<< lbsRequest def{ hPath = S.concat ["/", name, "?acl"] }

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

copyObject :: ByteString -> ByteString -> ByteString -> Yun CopyResult
copyObject bucket name source =
    xmlResponse =<<
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

deleteObjects :: ByteString -> [ByteString] -> Bool -> Yun DeleteResult
deleteObjects bucket names verbose = do
    let body = L.fromChunks $
          [ "<Delete><Quiet>"
          , if verbose then "false" else "true"
          , "</Quiet>"
          ] ++
          concat [["<Object><Key>", name, "</Key></Object>"] | name <- names] ++
          [ "</Delete>" ]
    xmlResponse =<<
        lbsRequest def{ hMethod  = "POST"
                      , hPath    = S.concat ["/", bucket, "?delete"]
                      , hBody    = RequestBodyLBS body
                      , hNeedMd5 = True
                      }
