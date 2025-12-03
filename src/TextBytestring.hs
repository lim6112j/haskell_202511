{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module TextBytestring (somefunc2) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

somefunc2 :: IO ()
somefunc2 = do
  --  creating byetestring
  let strictBS = B.pack [99, 97, 116]
  print strictBS
  let lazyBS = BL.pack [99, 97, 116]
  print lazyBS
  let strictBS' = BC.pack "cat"
  print strictBS'
  let lazyBS' = BLC.pack "cat"
  print lazyBS'
  --  creating text
  let strictT = T.pack "cat"
  print strictT
  let lazyT = TL.pack "cat"
  print lazyT
  --  converting bytestring to text
  let strictT' = E.decodeUtf8 strictBS'
  print strictT'
  let lazyT' = TL.decodeUtf8 lazyBS'
  print lazyT'
  --  converting text to bytestring
  let strictBS'' = E.encodeUtf8 strictT'
  print strictBS''
  let lazyBS'' = TL.encodeUtf8 lazyT'
  print lazyBS''
