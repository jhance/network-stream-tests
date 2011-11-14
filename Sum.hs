import Control.Monad.IO.Class
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import qualified Data.ByteString as B
import Data.Enumerator
import Data.Enumerator.List
import Data.Serialize
import Network
import Network.ByteString.Stream
import System.IO
import System.Posix.Unistd

nums :: [Int]
nums = [1..100]

main :: IO ()
main = withSocketsDo $ main'

main' :: IO ()
main' = do socket <- listenOn $ PortNumber (fromIntegral 4444)
           m <- newEmptyMVar
           forkIO $ client m
           (h, _, _) <- accept socket
           hSetBuffering h NoBuffering
           withStream h $ \s -> do forM_ nums $ \x -> do write s (encode x)
                                   return (Success ())
           takeMVar m
           return ()

client :: MVar () -> IO ()
client m = client' `finally` putMVar m ()

client' :: IO ()
client' = do print "Waiting 3 seconds"
             sleep 3
             print "Done"
             h <- connectTo "localhost" (PortNumber (fromIntegral 4444))
             receiveE h sum' >>= print
             
sum' :: Iteratee B.ByteString IO Int
sum' = do maybeBytes <- Data.Enumerator.List.head
          case maybeBytes of
            Nothing -> return 0
            Just bytes -> do let (Right i) = decode bytes
                             rest <- sum'
                             return $ i + rest
