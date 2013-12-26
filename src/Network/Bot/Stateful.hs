module Network.Bot.Stateful (

) where

import Control.Monad.State
import Network.Bot.Connection.Class
import Unsafe.Coerce

class Stateful a where
    applyState :: st -> a -> a

instance Stateful Connection where
    applyState s conn = conn { execute = (`evalStateT` s) }
