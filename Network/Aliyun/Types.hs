{-# LANGUAGE OverloadedStrings #-}
module Network.Aliyun.Types
  ( Owner(..)
  , BucketList(..)
  , BucketContent(..)
  , Bucket(..)
  , BucketACL(..)
  , DeleteResult(..)
  , CopyResult(..)
  ) where

import qualified Prelude as P
import BasicPrelude
import Safe (readMay)
import Control.Applicative ((<|>))

import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Aeson.Types (typeMismatch, Parser)
import Data.Aeson (FromJSON(parseJSON), Value(..), (.:), (.:?))
import Data.Time.Clock (UTCTime(UTCTime))
import Data.Time.Format (parseTime)
import System.Locale (defaultTimeLocale)

-- parser utils

strRead :: (FromJSON a) => (Text -> Maybe a) -> Value -> Parser a
strRead conv o = case o of
    String s ->
        maybe (fail $ "convert failed:"++T.unpack s)
              pure
              (conv s)
    _        -> parseJSON o

strBool :: Value -> Parser Bool
strBool = strRead conv
  where
    conv "true"  = Just True
    conv "false" = Just False
    conv _       = Nothing

pList :: (Value -> Parser a) -> Value -> Parser [a]
pList p (Array a) = mapM p (V.toList a)
pList p a         = (:[]) <$> p a

pTime :: String -> UTCTime
pTime s = fromMaybe (UTCTime (toEnum 0) 0) $ parseTime defaultTimeLocale "%FT%T.000Z" s

data Owner = Owner
  { ownerId     :: !Text
  , ownerName   :: !Text
  } deriving (Show)

instance FromJSON Owner where
    parseJSON (Object o) =
        Owner <$> o .: "ID" <*> o .: "DisplayName"
    parseJSON o =
        typeMismatch "Object" o

data BucketList = BucketList
  { bucketOwner :: !Owner
  , bucketList  :: ![(Text, UTCTime)]
  } deriving (Show)

instance FromJSON BucketList where
    parseJSON (Object o) = do
        root <- o .: "ListAllMyBucketsResult"
        let getBuckets = root .: "Buckets" >>= (.: "Bucket")
        BucketList <$> (root .: "Owner" >>= parseJSON)
                   <*> ( (getBuckets >>= pList pBucket)
                         <|> pure []
                       )
      where
        pBucket (Object a) =
            (,) <$> a .: "Name"
                <*> (pTime <$> a .: "CreationDate")
        pBucket a = typeMismatch "Object [BucketList.bucketList[i]]" a
    parseJSON a = typeMismatch "BucketList" a

data BucketContent = BucketContent
  { contentKey          :: !Text
  , contentLastModified :: !UTCTime
  , contentETag         :: !Text
  , contentType         :: !Text
  , contentSize         :: !Integer -- to support very large file
  , contentStorage      :: !Text
  , contentOwner        :: !Owner
  } deriving (Show)

instance FromJSON BucketContent where
    parseJSON (Object o) =
        BucketContent <$> o .: "Key"
                      <*> (pTime <$> o .: "LastModified")
                      <*> o .: "ETag"
                      <*> o .: "Type"
                      <*> (o .: "Size" >>= strRead (readMay . T.unpack))
                      <*> o .: "StorageClass"
                      <*> o .: "Owner"
    parseJSON a = typeMismatch "Object" a

data Bucket = Bucket
  { bucketName          :: !Text
  , bucketPrefix        :: !Text
  , bucketMarker        :: !Text
  , bucketMaxKeys       :: !Int
  , bucketDelimiter     :: !Text
  , bucketIsTruncated   :: !Bool
  , bucketNextMarker    :: Maybe Text
  , bucketContents      :: ![BucketContent]
  } deriving (Show)

instance FromJSON Bucket where
    parseJSON (Object o) = do
        r <- o .: "ListBucketResult"
        Bucket <$> r .: "Name"
               <*> r .: "Prefix"
               <*> r .: "Marker"
               <*> (r .: "MaxKeys" >>= strRead (readMay . T.unpack))
               <*> r .: "Delimiter"
               <*> (r .: "IsTruncated" >>= strBool)
               <*> r .:? "NextMarker"
               <*> ( (r .: "Contents" >>= pList parseJSON)
                     <|> pure []
                   )
    parseJSON a = typeMismatch "Object" a

data BucketACL = BucketACL
  { bucketACLOwner  :: !Owner
  , bucketACLs      :: ![Text]
  } deriving (Show)

instance FromJSON BucketACL where
    parseJSON (Object o) = do
        r <- o .: "AccessControlPolicy"
        let getAcls = r .: "AccessControlList" >>= (.: "Grant")
        BucketACL <$> (r .: "Owner" >>= parseJSON)
                  <*> ( (getAcls >>= pList parseJSON)
                        <|> pure []
                      )
    parseJSON a = typeMismatch "Object" a

newtype DeleteResult = DeleteResult [Text]
    deriving (Show)

instance FromJSON DeleteResult where
    parseJSON (Object a) = do
        r <- a .: "DeleteResult"
        DeleteResult <$> ( (r .: "Deleted" >>= pList pKey)
                            <|> pure []
                         )
      where
        pKey (Object o) = o .: "Key"
        pKey o          = typeMismatch "Object" o
    parseJSON o = typeMismatch "Object" o

data CopyResult = CopyResult
  { copyLastModified :: !UTCTime
  , copyETag         :: !Text
  } deriving (Show)

instance FromJSON CopyResult where
    parseJSON (Object o) = do
        r <- o .: "CopyObjectResult"
        CopyResult <$> (pTime <$> r .: "LastModified")
                   <*> r .: "ETag"
    parseJSON o = typeMismatch "Object" o
