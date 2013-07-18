{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as HTTP
import qualified Network.Wai.Handler.Warp as HTTP
import qualified System.Environment as System
import qualified System.Exit as System
import qualified System.IO as System

import Control.Applicative
import Control.Monad
import Data.Monoid


data Configuration =
  Configuration {
      configurationAWS :: AWSConfiguration,
      configurationDatabase :: DatabaseConfiguration,
      configurationWeb :: WebConfiguration
    }
instance JSON.FromJSON Configuration where
  parseJSON (JSON.Object v) =
    Configuration <$> v JSON..: "aws"
                  <*> v JSON..: "database"
                  <*> v JSON..: "web"
  parseJSON _ = mzero


data AWSConfiguration =
  AWSConfiguration {
      awsConfigurationLogin :: AWSLogin,
      awsConfigurationS3 :: S3AWSConfiguration,
      awsConfigurationIAM :: IAMAWSConfiguration
    }
instance JSON.FromJSON AWSConfiguration where
  parseJSON (JSON.Object v) =
    AWSConfiguration <$> v JSON..: "login"
                     <*> v JSON..: "s3"
                     <*> v JSON..: "iam"
  parseJSON _ = mzero


data AWSLogin =
  AWSLogin {
      awsLoginAccessKey :: Text.Text,
      awsLoginSecretKey :: Text.Text
    }
instance JSON.FromJSON AWSLogin where
  parseJSON (JSON.Object v) =
    AWSLogin <$> v JSON..: "access_key"
             <*> v JSON..: "secret_key"
  parseJSON _ = mzero


data S3AWSConfiguration =
  S3AWSConfiguration {
      s3AWSConfigurationBucket :: Text.Text
    }
instance JSON.FromJSON S3AWSConfiguration where
  parseJSON (JSON.Object v) =
    S3AWSConfiguration <$> v JSON..: "bucket"
  parseJSON _ = mzero


data IAMAWSConfiguration =
  IAMAWSConfiguration {
      iamAWSConfigurationGroupname :: Text.Text,
      iamAWSConfigurationUsername :: Text.Text
    }
instance JSON.FromJSON IAMAWSConfiguration where
  parseJSON (JSON.Object v) =
    IAMAWSConfiguration <$> v JSON..: "groupname"
                        <*> v JSON..: "username"
  parseJSON _ = mzero


data DatabaseConfiguration =
  DatabaseConfiguration {
      databaseConfigurationFilename :: Text.Text
    }
instance JSON.FromJSON DatabaseConfiguration where
  parseJSON (JSON.Object v) =
    DatabaseConfiguration <$> v JSON..: "filename"
  parseJSON _ = mzero


data WebConfiguration =
  WebConfiguration {
      webConfigurationPort :: Int
    }
instance JSON.FromJSON WebConfiguration where
  parseJSON (JSON.Object v) =
    WebConfiguration <$> v JSON..: "port"
  parseJSON _ = mzero


main :: IO ()
main = do
  arguments <- System.getArgs
  case arguments of
    [configurationFilename] -> do
      configurationText <- Text.readFile configurationFilename
      case JSON.eitherDecode'
           $ LBS.fromChunks [Text.encodeUtf8 configurationText] of
        Left message -> do
          putStrLn $ "Invalid configuration: " ++ message
          System.exitFailure
        Right configuration -> do
          let port = webConfigurationPort $ configurationWeb configuration
          HTTP.run port application
    _ -> do
      putStrLn "Usage: codex config.json"
      System.exitFailure


application :: HTTP.Application
application request = do
  return $ HTTP.ResponseSource HTTP.status200 [] $ do
    putStrLn "Hmm..."

