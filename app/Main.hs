module Main where

import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.ByteString as B
import Data.IP.Internal
import Network.DNS
import Control.Monad

main :: IO ()
main = print 5

matchDNS :: [String] -> IO [Either DNSError [IPv4]]
matchDNS [] = return []
matchDNS (x:xs) = do rs <- makeResolvSeed defaultResolvConf
                     let urlx = withResolver rs $ \resolver -> lookupA resolver $ encodeUtf8 . T.pack $ x
                     let prepend = liftM2 (:)
                     prepend urlx $ matchDNS xs

