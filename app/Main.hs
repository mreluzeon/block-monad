module Main where

import Lib

import Control.Monad
import Data.Char
import System.IO
import Network
import Data.Time.LocalTime
import Control.Parallel

data RequestType = GET | POST deriving (Show)
data Request = Request { rtype :: RequestType, path :: String, options :: [(String,String)] }
data Response = Response { version :: String, statuscode :: Int }

instance Show Request where
  show r = "Request { " ++ show((rtype r)) ++ " " ++ (path r)  ++ (foldl (\acc (k,v) -> acc ++ "\n  " ++ k ++ ": " ++ v) "" (options r)) ++ "\n}"

instance Show Response where
  show r = version(r) ++ " " ++ show(statuscode(r)) ++ " " ++ (case statuscode(r) of
    100 -> "Continue"
    200 -> "OK"
    404 -> "Not Found") ++ "\r\n\r\n"

fromString :: String -> RequestType
fromString t = case t of
  "GET" -> GET
  "POST" -> POST

respond :: Request -> Handle -> IO ()
respond request handle = do
  putStrLn $ show request
  let response = Response {version = "HTTP/1.1", statuscode = 200}
  hPutStr handle $ show(response)
  time <- getZonedTime
  case path(request) of
    "/furry" -> hPutStr handle $ "Blocked by RosComNadzor"
    name -> hPutStr handle $ "Hello, " ++ tail(name)
 
parseRequestHelper :: ([String], [(String,String)]) -> [(String,String)]
parseRequestHelper ([], accum) = accum
parseRequestHelper ((l:rest), accum) 
  | (length (words l)) < 2 = accum
  | otherwise = parseRequestHelper(rest, accum ++ [(reverse . tail . reverse . head . words $ l, unwords . tail . words $ l)] )

parseRequest :: [String] -> Request
parseRequest lns = case (words (head lns)) of
  [t,p,_] -> Request {rtype=(fromString t), path=p, options=parseRequestHelper((tail lns),[])}

handleAccept :: Handle -> String -> IO ()
handleAccept handle hostname = do 
  putStrLn $ "Handling request from " ++ hostname
  request <- fmap (parseRequest . lines) (hGetContents handle)
  respond request handle
  return ()

-- main :: IO ()
-- main = someFunc

main = withSocketsDo $ do
  sock <- listenOn (PortNumber 9000)
  putStrLn "Listening on port 9000"
  forever $ do
    (handle, hostname, port) <- accept sock
    handleAccept handle hostname
    hClose handle