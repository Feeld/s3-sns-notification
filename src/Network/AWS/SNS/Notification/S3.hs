{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module Network.AWS.SNS.Notification.S3 where

import           Data.Aeson       (FromJSON (..), withObject, (.:))
import           Data.Text        (Text)
import           Data.Time        (UTCTime)
import           GHC.Generics     (Generic)

-- | https://docs.aws.amazon.com/es_es/AmazonS3/latest/dev/notification-content-structure.html
newtype Notification = Notification
  { records :: [ NotificationRecord ]
  } deriving (Eq, Show, Generic)

instance FromJSON Notification where
  parseJSON = withObject "Notification" $ \o -> 
    o .: "Records" >>= fmap Notification . parseJSON 

data NotificationRecord = NotificationRecord
  { eventVersion      :: Text
  , eventSource       :: Text
  , awsRegion         :: Text
  , eventTime         :: UTCTime
  , eventName         :: Text
  , userIdentity      :: UserIdentity
  , requestParameters :: RequestParameters
  , s3                :: S3Event
  } deriving (Eq, Show, Generic, FromJSON)

newtype UserIdentity = UserIdentity
  { principalId :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

newtype RequestParameters = RequestParameters
  { sourceIPAddress :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

data S3Event = S3Event
  { s3SchemaVersion :: Text
  , configurationId :: Text
  , bucket          :: S3Bucket
  , object          :: S3Object
  } deriving (Eq, Show, Generic, FromJSON)

data S3Bucket = S3Bucket
  { name          :: Text
  , ownerIdentity :: UserIdentity
  , arn           :: Text
  } deriving (Eq, Show, Generic, FromJSON)

data S3Object = S3Object
  { key       :: Text
  , sequencer :: Text
  , size      :: Maybe Int
  , eTag      :: Maybe Text
  , versionId :: Maybe Text
  } deriving (Eq, Show, Generic, FromJSON)

