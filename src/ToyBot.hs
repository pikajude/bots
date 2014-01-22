module ToyBot where

import Control.Monad.IO.Class
import Network.Bots
import System.IO

conn = (ircConnection "irc.mountai.net" (PortNumber 6667)) {
        authenticate = \h _ -> liftIO $ hPutStrLn h "NICK aughters\r"
      }

main :: IO ()
main = runBotWithState () [ircConnection "irc.mountai.net" (PortNumber 6667)]
