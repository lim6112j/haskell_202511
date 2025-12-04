{-# LANGUAGE TemplateHaskellQuotes #-}

module TemplateImporting (makePlusN, simpleQuote, exprQuote) where

import Language.Haskell.TH

makePlusN :: Int -> Q Exp
makePlusN n = [|(+ n)|]

-- simple expression and quasi-quote
simpleQuote :: Q Exp
simpleQuote = [|2 + 3|]

-- expression with variable capture
exprQuote :: String -> Q Exp
exprQuote msg = [|putStrLn msg|]
