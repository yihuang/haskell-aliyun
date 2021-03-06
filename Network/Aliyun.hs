{-# LANGUAGE OverloadedStrings
           , GeneralizedNewtypeDeriving
           , MultiParamTypeClasses
           , RecordWildCards
           , TypeFamilies
           , TupleSections
           , FlexibleContexts
           , Rank2Types
           #-}
module Network.Aliyun
  ( Yun(..)
  , YunConf(..)
  , YunEnv
  , BucketQuery(..)
  , runYun
  , runYunWithManager
  , listService
  , putBucket
  , getBucket
  , getBucketContents
  , getBucketContentsLifted
  , getBucketACL
  , deleteBucket
  , putObject
  , putObjectStr
  , putObjectStream
  , putObjectFile
  , getObject
  , getObjectRange
  , getObjectStream
  , getObjectRangeStream
  , copyObject
  , headObject
  , deleteObject
  , deleteObjects
  , module Network.Aliyun.Types
  ) where

import qualified Prelude as P
import BasicPrelude
import qualified Filesystem.Path as Path
import qualified Filesystem.Path.CurrentOS as Path
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy as L
import qualified Data.Text.Encoding as T
import Data.Time.Clock (UTCTime(UTCTime))

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
  { yunHost :: !ByteString
  , yunId   :: !ByteString
  , yunKey  :: !ByteString
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

-- | run yun monad with a new http manager.
runYun :: YunConf -> Yun a -> IO a
runYun conf yun = withManager $ \man -> runReaderT (unYun yun) (conf, man)

-- | run yun monad with provided http manager.
runYunWithManager :: Manager -> YunConf -> Yun a -> ResourceT IO a
runYunWithManager man conf yun = runReaderT (unYun yun) (conf, man)

formatNow :: IO ByteString
formatNow = S.pack . formatTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S GMT" <$> getCurrentTime

data RequestHints = RequestHints
  { hMethod  :: !ByteString
  , hPath    :: !ByteString
  , hQuery   :: !ByteString
  , hHeaders :: !W.RequestHeaders
  , hBody    :: !(RequestBody Yun)
  , hNeedMd5 :: !Bool
  }

instance Default RequestHints where
    def = RequestHints "GET" "/" "" [] (RequestBodyLBS empty) False

mkRequest :: RequestHints -> Yun (Request Yun)
mkRequest RequestHints{..} = do
    conf <- askConf
    time <- liftIO formatNow
    let req = def { host            = yunHost conf
                  , method          = hMethod
                  , path            = hPath
                  , queryString     = hQuery
                  , requestHeaders  = ("Date", time) : hHeaders
                  , requestBody     = hBody
                  }
    return $ authorizeRequest req (yunId conf) (yunKey conf) hNeedMd5

streamRequest :: RequestHints -> Yun (Response (C.ResumableSource Yun S.ByteString))
streamRequest hints = do
    req <- mkRequest hints
    askManager >>= http req

lbsRequest :: RequestHints -> Yun (Response LByteString)
lbsRequest hints =
    streamRequest hints >>= lbsResponse

xmlResponse :: (C.MonadThrow m, FromJSON a) => Response LByteString -> m a
xmlResponse = parseXML . responseBody

-- | list all the buckets.
listService :: Yun BucketList
listService =
    xmlResponse =<< lbsRequest def

-- | create or update a bucket.
putBucket :: ByteString -> Maybe ByteString -> Yun LByteString
putBucket name macl = do
    let hds = maybeToList $ ("x-oss-acl",) <$> macl
    responseBody <$>
        lbsRequest def{ hMethod  = "PUT"
                      , hPath    = "/"++name
                      , hHeaders = hds
                      }

-- | bucket item query conditions.
data BucketQuery = BucketQuery
  { qryPrefix       :: !ByteString
  , qryMaxKeys      :: !Int
  , qryMarker       :: !(Maybe Text)
  , qryDelimiter    :: !(Maybe Char)
  }
instance Default BucketQuery where
    def = BucketQuery "" 1000 Nothing Nothing

-- | query bucket items.
getBucket :: ByteString -> BucketQuery -> Yun Bucket
getBucket name qry =
    xmlResponse =<< lbsRequest def{ hPath = "/"++name, hQuery=qs }
  where
    qs = S.concat
           [ "prefix=", qryPrefix qry
           , "&max-keys=", S.pack $ P.show (qryMaxKeys qry)
           , maybe "" (("&marker="++) . T.encodeUtf8) (qryMarker qry)
           , maybe "" (("&delimiter="++) . S.singleton) (qryDelimiter qry)
           ]

-- | Generic version of `getBucketContents'.
getBucketContentsLifted :: Monad m => (forall a. Yun a -> m a) -> ByteString -> ByteString -> C.Source m BucketContent
getBucketContentsLifted liftYun name prefix = loop def{qryDelimiter=Just '/', qryPrefix=prefix}
  where
    withFilePath f = either id id . Path.toText . f . Path.fromText

    loop qry = do
        bucket <- lift $ liftYun $ getBucket name qry
        -- yield directories
        mapM_ ( C.yield
              . flip ContentDirectory (UTCTime (toEnum 60000) 0)
              . withFilePath Path.dirname
              )
              (bucketDirectories bucket)
        -- yield files
        mapM_ ( C.yield
              . ContentFile
              . (\f -> f{fileKey = withFilePath Path.filename (fileKey f)})
              )
              (bucketContents bucket)
        when (bucketIsTruncated bucket) $
            loop qry{qryMarker=bucketNextMarker bucket}

-- | get bucket items streamlined, support more then 1000 items.
getBucketContents :: ByteString -> ByteString -> C.Source Yun BucketContent
getBucketContents = getBucketContentsLifted id

-- | query bucket acl info.
getBucketACL :: ByteString -> Yun BucketACL
getBucketACL name =
    xmlResponse =<< lbsRequest def{ hPath = S.concat ["/", name, "?acl"] }

-- | delete bucket by name
deleteBucket :: ByteString -> Yun LByteString
deleteBucket name =
    responseBody <$>
        lbsRequest def{ hMethod = "DELETE"
                      , hPath   = "/"++name
                      }

-- | upload a file.
putObject :: ByteString -> ByteString -> RequestBody Yun -> Yun LByteString
putObject bucket name body =
    responseBody <$>
        lbsRequest def{ hMethod = "PUT"
                      , hPath   = S.concat ["/", bucket, "/", name]
                      , hBody   = body
                      }

-- | Upload a file with `LByteString' content.
putObjectStr :: ByteString -> ByteString -> LByteString -> Yun LByteString
putObjectStr bucket name body = putObject bucket name (RequestBodyLBS body)

-- | Upload a file from disk streamlined.
putObjectFile :: ByteString -> ByteString -> IO.FilePath -> Yun LByteString
putObjectFile bucket name path =
    Lifted.bracket
        (liftIO $ IO.openBinaryFile path IO.ReadMode)
        (liftIO . IO.hClose)
        (\h -> do
           size <- fromIntegral <$> liftIO (IO.hFileSize h)
           let src  = C.sourceHandle h C.$= C.map B.fromByteString
           putObjectStream bucket name size src
        )

-- | Upload a file from a source streamlined.
-- aliyun don't support chunked tranfer-encoding, so size must be passed.
putObjectStream :: ByteString -> ByteString -> Int64 -> C.Source Yun B.Builder -> Yun LByteString
putObjectStream bucket name size source =
    putObject bucket name (RequestBodySource size source)

-- TODO put object multipart

-- | download a file.
getObject :: ByteString -> ByteString -> Yun LByteString
getObject bucket name = getObjectRange bucket name Nothing

-- | download a file streamlined.
getObjectStream :: ByteString -> ByteString -> Yun (C.ResumableSource Yun S.ByteString)
getObjectStream bucket name = getObjectRangeStream bucket name Nothing

-- | download a range of file.
getObjectRange :: ByteString -> ByteString -> Maybe ByteString -> Yun LByteString
getObjectRange bucket name mrange = do
    let hds = maybeToList $ ("Range",) . ("bytes="++) <$> mrange
    responseBody <$>
        lbsRequest def{ hPath    = S.concat ["/", bucket, "/", name]
                      , hHeaders = hds
                      }

-- | download a range of file streamlined.
getObjectRangeStream :: ByteString -> ByteString -> Maybe ByteString -> Yun (C.ResumableSource Yun S.ByteString)
getObjectRangeStream bucket name mrange = do
    let hds = maybeToList $ ("Range",) . ("bytes="++) <$> mrange
    responseBody <$>
        streamRequest def{ hPath    = S.concat ["/", bucket, "/", name]
                      , hHeaders = hds
                      }

-- | copy an object.
copyObject :: ByteString -> ByteString -> ByteString -> Yun CopyResult
copyObject bucket name source =
    xmlResponse =<<
        lbsRequest def{ hMethod  = "PUT"
                      , hPath    = S.concat ["/", bucket, "/", name]
                      , hHeaders = [("x-oss-copy-source", source)]
                      }

-- | HEAD request get object.
headObject :: ByteString -> ByteString -> Yun LByteString
headObject bucket name =
    responseBody <$>
        lbsRequest def{ hMethod = "HEAD"
                      , hPath   = S.concat ["/", bucket, "/", name]
                      }

-- | delete object.
deleteObject :: ByteString -> ByteString -> Yun LByteString
deleteObject bucket name =
    responseBody <$>
        lbsRequest def{ hMethod = "DELETE"
                      , hPath   = S.concat ["/", bucket, "/", name]
                      }

-- | batch delete multiple objects.
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
