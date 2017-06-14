module ChatWork.MonadHttpIO where

import Control.Exception (throwIO)
import Network.HTTP.Req

instance MonadHttp IO where
  handleHttpException = throwIO
