module Main where

import System.IO
import qualified Network.Socket as N
import Data.List
import System.Exit
import Control.Monad.Trans.Reader
import Control.Exception
import Control.Monad.IO.Class
import Control.Concurrent
import System.Environment

data Client = Client { clientSocket :: Handle,
                 clientServer :: String,
                 clientPort :: N.PortNumber,
                 clientChan :: String,
                 clientNick :: String }

type Net = ReaderT Client IO

validArgs :: IO ()
validArgs = do
  args <- getArgs
  let server = args !! 0; port = read (args !! 1) :: N.PortNumber; chan = args !! 2; nick = args !! 3
  bracket (connect server port chan nick) disconnect loop
  where
    disconnect = hClose . clientSocket
    loop st = runReaderT run st

help :: IO ()
help = do
  args <- getArgs
  putStrLn "This is a terminal-based IRC Client written in Haskell"
  putStrLn "Usage: Takes 4 arguments"
  putStrLn "IP, Port, Channel, Nickname"
  putStrLn "Channel does not use a #"
  putStrLn $ "You entered " ++ (show (length args)) ++ " " ++ if (length args) == 1 then "argument" else "arguments"

checkPort :: IO ()
checkPort = do
  args <- getArgs
  case reads (args !! 1) :: [(N.PortNumber,String)] of
    [(_,"")] -> validArgs
    _ -> putStrLn "Invalid Port" >> help

main :: IO ()
main = do
  args <- getArgs
  if ((length args) /= 4) then help else checkPort

connect :: String -> N.PortNumber -> String -> String -> IO Client
connect server port chan nick = notify $ do
  h <- connectTo server port
  return $ Client h server port ("#"++chan) nick
  where
    notify a = bracket_
      (putStrLn ("Connecting to " ++ server ++ " ...") >> hFlush stdout)
      (putStrLn "done.")
      a

connectTo :: N.HostName -> N.PortNumber -> IO Handle
connectTo host port = do
  addr : _ <- N.getAddrInfo Nothing (Just host) (Just (show port))
  sock <- N.socket (N.addrFamily addr) (N.addrSocketType addr) (N.addrProtocol addr)
  N.connect sock (N.addrAddress addr)
  N.socketToHandle sock ReadWriteMode

run :: Net ()
run = do
  nick <- asks clientNick
  chan <- asks clientChan
  write "NICK" nick
  write "USER" (nick ++ " 0 * :tutorial client")
  write "JOIN" chan
  liftIO $ hSetBuffering stdout NoBuffering
  listen

write :: String -> String -> Net ()
write cmd args = do
  h <- asks clientSocket
  let msg = cmd ++ " " ++ args ++ "\r\n"
  liftIO $ hPutStr h msg
  liftIO $ putStr ("> " ++ msg)

readIRC :: Net ()
readIRC = do
  h <- asks clientSocket
  line <- liftIO $ hGetLine h
  liftIO $ putStrLn line
  let msg = init line
  if isPing msg then pong msg else eval $ clean msg
  where
    clean :: String -> String
    clean = drop 1 . dropWhile (/= ':') . drop 1

    isPing :: String -> Bool
    isPing x = "PING :" `isPrefixOf` x

    pong :: String -> Net()
    pong x = write "PONG" (':' : drop 6 x)

writeIRC :: Net ()
writeIRC = do
  h <- asks clientSocket
  chan <- asks clientChan
  input <- liftIO $ getLine
  liftIO $ hPutStr h ("PRIVMSG " ++ chan ++ " :" ++ input ++ "\r\n")

listen :: Net ()
listen = do
  h <- asks clientSocket
  server <- asks clientServer
  port <- asks clientPort
  chan <- asks clientChan
  nick <- asks clientNick
  writeID <- liftIO $ forkThread $ forever $ runReaderT writeIRC $ Client h server port chan nick
  readID <- liftIO $ forkThread $ forever $ runReaderT readIRC $ Client h server port chan nick
  liftIO $ takeMVar writeID
  liftIO $ takeMVar readID
  return ()

forkThread :: IO () -> IO (MVar ())
forkThread proc = do
    handle' <- newEmptyMVar
    _ <- forkFinally proc (\_ -> putMVar handle' ())
    return handle'

forever :: IO() -> IO()
forever a = do a; forever a

eval :: String -> Net ()
eval "!quit" = write "QUIT" ":Exiting" >> liftIO exitSuccess
eval x | "!id " `isPrefixOf` x = privmsg $ drop 4 x
eval _ = return ()

privmsg :: String -> Net ()
privmsg msg = do
  chan <- asks clientChan
  write "PRIVMSG" (chan ++ " :" ++ msg)
