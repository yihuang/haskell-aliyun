{-# LANGUAGE OverloadedStrings #-}
module Network.Aliyun.Utils where

import qualified Prelude
import BasicPrelude

import Data.Ord (comparing)
import qualified Data.ByteString as B
import qualified Data.CaseInsensitive as CI
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as L
import Data.HMAC (hmac_sha1)
import qualified Data.Digest.MD5 as MD5

import qualified Network.HTTP.Types as W
import Network.HTTP.Conduit

canonicalizeHeaders :: W.RequestHeaders -> ByteString
canonicalizeHeaders hds =
    let hds' = [hd | hd <- hds, "x-oss-" `S.isPrefixOf` CI.foldedCase (fst hd)]

        -- merge grouped headers.
        merge :: [W.Header] -> W.Header
        merge [] = error "impossible [canonicalizeHeaders]"
        merge xs@((name, _):_) = (name, S.concat . intersperse "," $ map snd xs)

        hds'' = map merge . group . sortBy (comparing fst) $ hds'
    in  S.concat [S.concat [CI.foldedCase k, ":", v, "\n"] | (k,v) <- hds'']

bodyContent :: RequestBody m -> LByteString
bodyContent (RequestBodyLBS s) = s
bodyContent _ = error "unimplemented [bodyContent]"

authorizeRequest :: Request m -> ByteString -> ByteString -> Bool -> Request m
authorizeRequest req identity key needMD5 =
    let date    = fromMaybe "" $ lookup "Date" hds
        ctype   = fromMaybe "" $ lookup "Content-Type" hds
        bodyMD5 = B64.encode . B.pack . MD5.hash . L.unpack . bodyContent . requestBody $ req
        hds'    = requestHeaders req
        hds     = if needMD5 then ("Content-MD5", bodyMD5) : hds' else hds'
        s = S.concat [ method req, "\n"
                     , if needMD5 then bodyMD5 else "", "\n"
                     , ctype, "\n"
                     , date, "\n", canonicalizeHeaders hds
                     , path req
                     ]
        auth = S.concat [ "OSS ", identity, ":"
                        , B64.encode $ B.pack (hmac_sha1 (B.unpack key) (B.unpack s))
                        ]
    in  req { requestHeaders = ("Authorization", auth) : hds }
