{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Prelude
  ( module Relude
  -- 안전한 함수들 (head, tail 등은 에러 대신 Maybe)
  , headMaybe, tailMaybe, lastMaybe, initMaybe
  , readMaybe, fromMaybe, maybe, either
  -- 더 자주 쓰는 별명
  , (&), (<&>)
  , when, unless, void
  , for_, forM_
  , Text, LText, ByteString, LByteString
  , tshow
  , ordNub, sortOn, on
  -- 에러 핸들링 최강 조합
  , throwIO, throw, catch, try, catchAny
  , MonadIO(liftIO)
  , displayException
  -- 디버깅 신의 한 수
  , trace, traceM, traceShowId, traceShowM
  -- 변환 유틸
  , bool
  -- 개발 중에만 켜지는 assert
  , assert
  ) where

import Relude
       hiding (head, tail, init, last, readMaybe, undefined, assert, Head, Tail)

import Relude.Extra.Newtype   (un)
import Relude.Extra.Tuple     (dup)
import Relude.Enum      (universe)
import Control.Monad.Catch

-- 안전한 partial 함수들 (실무에서 99.9% 이걸 씀)
headMaybe []      = Nothing
headMaybe (x:_)   = Just x
tailMaybe []      = Nothing
tailMaybe (_:xs)  = Just xs
initMaybe []      = Nothing
initMaybe xs      = Just (take (length xs - 1) xs)
lastMaybe []      = Nothing
lastMaybe xs      = Just (Last xs)

-- Text로 show (� nombre도 안 붙음)
tshow :: Show a => a -> Text
tshow = show >>> toText

-- 흔히 쓰는 catchAll
catchAny :: MonadCatch m => m a -> (SomeException -> m a) -> m a
catchAny = catch

-- 개발 중에만 동작하는 assert (프로덕션에서는 사라짐)
#ifndef PRODUCTION
assert :: Bool -> a -> a -> a
assert False _ _ = error "assertion failed!"
assert True  _  x = x
#else
assert :: Bool -> a -> a
assert _ x = x
#endif
