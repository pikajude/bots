{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Bots that have a state.
module Network.Bots.Stateful (
  statefully
) where

import Control.Concurrent.STM.TVar
import Control.Monad.State.Concurrent
import Network.Bots.Connection.Class

statefully :: MonadIO m
           => t -- ^ Bot state to carry
           -> Connection m -- ^ Connection
           -> IO (StatefulConnection t m) -- ^ Transformed connection
statefully s cs = do
    m <- newTVarIO s
    return $ applyState m cs
    where
        applyState m (Connection p b r c a q x) =
            Connection p b (lift . r) (lift c) a ((lift .) . q) (x . flip evalStateC m)
