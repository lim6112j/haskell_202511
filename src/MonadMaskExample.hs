{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}

module MonadMaskExample where
import Relude
-- 또는 Protolude

import Control.Exception.Safe (MonadMask, bracket, bracketOnError, finally, onException)
import UnliftIO (MonadUnliftIO, openFile, hClose, hGetBuffering, tryAny )
import Data.Text.IO (hGetContents)
import System.IO (openTempFile)
import UnliftIO.Directory (removeFile)

-- 예시: 파일을 안전하게 읽고 처리하고 무조건 닫기
safeReadFile :: (MonadIO m, MonadMask m) => FilePath -> (Text -> m a) -> m a
safeReadFile path action = bracket
    (liftIO $ openFile path ReadMode)      -- acquire
    (liftIO . hClose)                      -- release (항상 실행됨)
    (\h -> liftIO (hGetContents h) >>= action)  -- use

-- 예시 2: 에러가 나도 반드시 정리 (bracketOnError)
safeTempFile :: (MonadIO m, MonadMask m) => Text -> (FilePath -> Handle -> m a) -> m a
safeTempFile template action = bracketOnError
    (liftIO $ openTempFile "/tmp" (toString template))
    (\(fp, h) -> liftIO $ do hClose h; void $ tryAny $ removeFile fp)
    (uncurry action)

-- 예시 3: finally 스타일 (가장 자주 쓰임)
-- Note: This example is commented out because it requires postgresql-simple
-- To use it, add postgresql-simple to package.yaml dependencies
{-
import Database.PostgreSQL.Simple (Connection, connectPostgreSQL, close)

withDBConnection :: (MonadIO m, MonadMask m) => Text -> (Connection -> m a) -> m a
withDBConnection connStr action =
    bracket
      (liftIO $ connectPostgreSQL (encodeUtf8 connStr))
      (liftIO . close)
      action
    `onException` liftIO (putStrLn ("DB 연결 중 예외 발생, 정리 완료" :: Text))
-}
