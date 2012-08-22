{-# LANGUAGE OverloadedStrings #-}
import qualified Prelude
import BasicPrelude
import Network.Aliyun
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy.Char8 as L

{-
 - Before run test.hs, create a file named "config" in current directory,
 - which contains two lines, the first line is identity, second line is secret key.
 -}

loadConf :: FilePath -> IO YunConf
loadConf path = do
    (ident : key : _) <- lines <$> readFile path
    return $ YunConf "storage.aliyun.com" (T.encodeUtf8 ident) (T.encodeUtf8 key)

p :: MonadIO m => m LByteString -> m ()
p m = m >>= liftIO . L.putStrLn

main :: IO ()
main = do
    conf <- loadConf "./config"
    runYun conf $ do
        liftIO $ putStrLn "list service"
        p listService
        liftIO $ putStrLn "put bucket"
        p $ putBucket "yihuang_bucket" Nothing
        liftIO $ putStrLn "list service"
        p listService
        liftIO $ putStrLn "get bucket"
        p $ getBucket "yihuang_bucket"
        liftIO $ putStrLn "get bucket acl"
        p $ getBucketACL "yihuang_bucket"
        liftIO $ putStrLn "put bucket acl"
        p $ putBucket "yihuang_bucket" (Just "public-read-write")
        liftIO $ putStrLn "get bucket acl"
        p $ getBucketACL "yihuang_bucket"
        liftIO $ putStrLn "put string object test1"
        p $ putObjectStr "yihuang_bucket" "test1" "hello world"
        liftIO $ putStrLn "put string object test2"
        liftIO $ writeFile "./data" "hello world"
        p $ putObjectFile "yihuang_bucket" "test2" "./data"
        liftIO $ putStrLn "get bucket"
        p $ getBucket "yihuang_bucket"