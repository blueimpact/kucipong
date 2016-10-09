
module Main where

import Kucipong.Prelude

import Control.FromSum ( fromEitherM )
import Database.Persist ( Entity(..) )
import Options.Applicative
    ( InfoMod, Parser, ParserInfo, ReadM, argument, eitherReader, execParser
    , fullDesc, header, helper, info, metavar, progDesc, str
    )
import Text.Email.Validate ( validateFromString )

import Kucipong.Config ( createConfigFromEnv )
import Kucipong.Db ( adminLoginTokenLoginToken )
import Kucipong.Monad
    ( MonadKucipongDb(..), MonadKucipongSendEmail(..), runKucipongM )

data AddAdminCommand = AddAdminCommand
    { addAdminEmail :: EmailAddress
    , addAdminName :: Text
    }
    deriving (Data, Eq, Generic, Show, Typeable)

-- | Add a new 'Admin'.  If an 'Admin' with the 'EmailAddress' already exists,
-- just create a new 'AdminLoginToken' for them and then send them an email.
addAdmin
    :: ( MonadBaseControl IO m
       , MonadCatch m
       , MonadIO m
       , MonadKucipongDb m
       , MonadKucipongSendEmail m
       )
    => EmailAddress
    -> Text
    -- ^ Name of the 'Admin'
    -> m ()
addAdmin email name = do
    eitherAdminEntity <- try $ dbUpsertAdmin email name
    (Entity adminKey _) <- fromEitherM handleUpsertAdminError eitherAdminEntity
    eitherAdminLoginToken <- try $ dbCreateAdminMagicLoginToken adminKey
    (Entity _ adminLoginToken) <-
        fromEitherM handleCreateAdminLoginTokenError eitherAdminLoginToken
    let loginToken = adminLoginTokenLoginToken adminLoginToken
    sendAdminLoginEmail email loginToken
    putStrLn $ "Created (or updated) admin with email " <> tshow email <>
        " and name \"" <> name <> "\".\nSent email with login url."
  where
    handleUpsertAdminError :: SomeException -> m a
    handleUpsertAdminError err =
        error $ "Error when trying to upsert admin: " <> show err

    handleCreateAdminLoginTokenError :: SomeException -> m a
    handleCreateAdminLoginTokenError err =
        error $ "Error when trying to create admin login token: " <> show err

run :: AddAdminCommand -> IO ()
run (AddAdminCommand email name) = do
    config <- createConfigFromEnv
    res <- runKucipongM config $ addAdmin email name
    case res of
        Right () -> pure ()
        Left appErr ->
            putStrLn $ "Got app err: " <> tshow appErr

-- | 'ReadM' parser for 'EmailAddress'.
emailReadM :: ReadM EmailAddress
emailReadM = eitherReader validateFromString

-- | 'ReadM' parser for 'Text'.  Similar to 'str'.
textReadM:: ReadM Text
textReadM = pack <$> str

addAdminCommandParser :: Parser AddAdminCommand
addAdminCommandParser = AddAdminCommand <$> emailOption <*> nameOption
  where
    emailOption :: Parser EmailAddress
    emailOption = argument emailReadM $ metavar "ADMIN_EMAIL"

    nameOption :: Parser Text
    nameOption = argument textReadM $ metavar "ADMIN_NAME"

main :: IO ()
main = execParser topOpts >>= run
  where
    topOpts :: ParserInfo AddAdminCommand
    topOpts = info (helper <*> addAdminCommandParser) topDesc

    topDesc :: InfoMod AddAdminCommand
    topDesc =
        let headerMsg = "kucipong-add-admin - add a new admin user"
            progDescMsg =
                "Add a new admin user to the database and send them an " <>
                "email they can use to login."
        in fullDesc `mappend`
            progDesc progDescMsg `mappend`
            header headerMsg
