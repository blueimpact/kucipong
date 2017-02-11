{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Kucipong.Monad.Aws.Class where

import Kucipong.Prelude

import Control.Monad.Trans (MonadTrans)
import Network.AWS (Error)
import Web.Spock (ActionCtxT, UploadedFile)

import Kucipong.Db (Image)
import Kucipong.Monad.Cookie.Trans (KucipongCookieT)
import Kucipong.Monad.Db.Trans (KucipongDbT)
import Kucipong.Monad.SendEmail.Trans (KucipongSendEmailT)

data FileUploadError
  = AwsError Error
  | FileReadError IOException
  deriving (Generic, Show, Typeable)


-- |
-- Default implementations are used to easily derive instances for monads
-- transformers that implement 'MonadTrans'.
class Monad m => MonadKucipongAws m where
  awsS3PutUploadedFile
    :: UploadedFile
    -> m (Either FileUploadError Image)
  default awsS3PutUploadedFile
    :: ( MonadKucipongAws n
       , MonadTrans t
       , m ~ t n
       )
    => UploadedFile -> t n (Either FileUploadError Image)
  awsS3PutUploadedFile = lift . awsS3PutUploadedFile

instance MonadKucipongAws m => MonadKucipongAws (ActionCtxT ctx m)
instance MonadKucipongAws m => MonadKucipongAws (ExceptT e m)
instance MonadKucipongAws m => MonadKucipongAws (IdentityT m)
instance MonadKucipongAws m => MonadKucipongAws (KucipongCookieT m)
instance MonadKucipongAws m => MonadKucipongAws (KucipongDbT m)
instance MonadKucipongAws m => MonadKucipongAws (KucipongSendEmailT m)
instance MonadKucipongAws m => MonadKucipongAws (ReaderT r m)
