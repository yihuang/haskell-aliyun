{-# LANGUAGE OverloadedStrings #-}
module Network.Aliyun.Utils where

import qualified Prelude
import BasicPrelude

import Data.Ord (comparing)
import qualified Data.ByteString as B
import qualified Data.CaseInsensitive as CI
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Base64 as B64
import Data.HMAC (hmac_sha1)

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

authorizeRequest :: Request m -> ByteString -> ByteString -> Request m
authorizeRequest req identity key =
    let hds = requestHeaders req
        date = fromMaybe "" $ lookup "Date" hds
        s = S.concat [ method req
                     , "\n", "", "\n", "", "\n"
                     , date, "\n", canonicalizeHeaders hds
                     , path req
                     ]
        auth = S.concat [ "OSS ", identity, ":"
                        , B64.encode $ B.pack (hmac_sha1 (B.unpack key) (B.unpack s))
                        ]
    in  req { requestHeaders = ("Authorization", auth) : requestHeaders req }
