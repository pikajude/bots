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

-- | Transform a list of connections into a list of stateful connections,
-- concurrently operating on the same shared state.
statefully :: MonadIO m
           => TVar t -- ^ Bot state to carry
           -> [Connection m] -- ^ Connections
           -> [StatefulConnection t m] -- ^ Transformed connections
statefully m = go where
    go [] = []
    go (Connection p b r c a q x:css) =
        Connection p b (lift . r) (lift c) a ((lift .) . q) (x . flip evalStateC m)
      : go css
