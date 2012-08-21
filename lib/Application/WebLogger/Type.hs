{-# LANGUAGE DeriveDataTypeable, 
             TemplateHaskell, 
             TypeFamilies, 
             TypeSynonymInstances, 
             OverloadedStrings, 
             FlexibleInstances, 
             RecordWildCards #-}

module Application.WebLogger.Type where

import           Control.Applicative 
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Acid 
import           Data.Aeson
import           Data.Foldable
import           Data.Data
import           Data.SafeCopy
import           Data.Sequence

-- | 
data WebLoggerInfo = WebLoggerInfo { 
  weblog_content :: String
} deriving (Show,Typeable,Data)

deriveSafeCopy 0 'base ''WebLoggerInfo


-- |
instance FromJSON WebLoggerInfo where
  parseJSON (Object v) = WebLoggerInfo <$>  v .: "content" 
  parseJSON _ = mzero 


-- |
instance ToJSON WebLoggerInfo where
  toJSON WebLoggerInfo {..} = object [ "content" .= weblog_content ] 

-- | 
type WebLoggerRepo = Seq WebLoggerInfo 


instance FromJSON (Seq WebLoggerInfo) where
  parseJSON v = fromList <$> parseJSON v

instance ToJSON WebLoggerRepo where
  toJSON = toJSON . toList


-- |
addLog :: WebLoggerInfo 
             -> Update WebLoggerRepo WebLoggerInfo 
addLog minfo = do 
  modify (|> minfo) 
  return minfo
 
{-
-- |
queryWebLogger :: UUID -> Query WebLoggerInfoRepository (Maybe WebLoggerInfo) 
queryWebLogger uuid = do 
  m <- ask 
  return (M.lookup uuid m)
-}

-- |
queryAllLog :: Query WebLoggerRepo WebLoggerRepo
queryAllLog = do m <- ask   
                 return m

{-
-- | 
updateWebLogger :: WebLoggerInfo -> Update WebLoggerInfoRepository (Maybe WebLoggerInfo)
updateWebLogger minfo = do 
  m <- get 
  let (r,m') = M.updateLookupWithKey (\_ _ -> Just minfo) (yesodcrud_uuid minfo) m
  put m'
  maybe (return Nothing) (const (return (Just minfo))) r 

-- | 
deleteWebLogger :: UUID -> Update WebLoggerInfoRepository (Maybe WebLoggerInfo)
deleteWebLogger uuid = do 
  m <- get
  let r = M.lookup uuid m  
  case r of 
    Just _ -> do  
      let m' = M.delete uuid m  
      put m' 
      return r
    Nothing -> return Nothing
-}

makeAcidic ''WebLoggerRepo [ 'addLog, 'queryAllLog ]
