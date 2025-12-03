{-# LANGUAGE TemplateHaskellQuotes #-}

module TemplateImporting (makePlusN) where

import Language.Haskell.TH

makePlusN :: Int -> Q Exp
makePlusN n = [|(+ n)|]
