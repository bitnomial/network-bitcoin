{-# LANGUAGE OverloadedStrings #-}

module Main where


import           Data.Vector             (empty)
import           Network.Bitcoin
import           Test.QuickCheck
import           Test.QuickCheck.Monadic


main :: IO ()
main = mapM_ qcOnce [ canGetInfo
                    , canListUnspent
                    , canGetOutputInfo
                    ]


qcOnce :: Property -> IO ()
qcOnce = quickCheckWith stdArgs { maxSuccess = 1
                                , maxSize = 1
                                , maxDiscardRatio = 1
                                }


client :: IO Client
client = getClient "http://127.0.0.1:18332" "bitcoinrpc" "bitcoinrpcpassword"


canGetInfo :: Property
canGetInfo = monadicIO $ do
    info <- run $ getBitcoindInfo =<< client
    let checks = [ bitcoinVersion info > 80000
                 , onTestNetwork info
                 , bitcoindErrors info == ""
                 ]
    assert $ and checks


canListUnspent :: Property
canListUnspent = monadicIO $ do
    _ <- run $ (\c -> listUnspent c Nothing Nothing Data.Vector.empty) =<< client
    assert True


canGetOutputInfo :: Property
canGetOutputInfo = monadicIO $ do
    info <- run $ (\c-> getOutputInfo c "ab8e26fd95fa371ac15b43684d0c6797fb573757095e7d763ba86ad315f7db04" 1) =<< client
    _ <- run $ print info
    assert True
