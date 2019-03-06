{-# LANGUAGE OverloadedStrings #-}

module Main where


import           Data.Fixed              (Fixed (MkFixed))
import           Data.Vector             (empty)
import qualified Data.Vector             as V
import           Network.Bitcoin
import           Test.QuickCheck
import           Test.QuickCheck.Monadic


main :: IO ()
main = mapM_ qcOnce [ canListUnspent
                    , canGetBlock
                    , canGetOutputInfo
                    , canGetRawTransaction
                    , canGetAddress
                    , canSendPayment
                    , canEstimateFees
                    ]


qcOnce :: Property -> IO ()
qcOnce = quickCheckWith stdArgs { maxSuccess = 1
                                , maxSize = 1
                                , maxDiscardRatio = 1
                                }


client :: IO Client
client = getClient "http://127.0.0.1:18332" "bitcoinrpc" "bitcoinrpcpassword"


canListUnspent :: Property
canListUnspent = monadicIO $ do
    _ <- run $ (\c -> listUnspent c Nothing Nothing Data.Vector.empty) =<< client
    assert True


getTopBlock :: Client -> IO Block
getTopBlock c = getBlockCount c >>= getBlockHash c >>= getBlock c


canGetBlock :: Property
canGetBlock = monadicIO $ do
    run $ client >>=
        getTopBlock >>=
        print
    assert True


canGetRawTransaction :: Property
canGetRawTransaction = monadicIO $ do
    run $ do
        c <- client
        b <- getTopBlock c
        getRawTransactionInfo c (subTransactions b V.! 0) >>= print
    assert True


canGetOutputInfo :: Property
canGetOutputInfo = monadicIO $ do
    run $ do
        c <- client
        b <- getTopBlock c
        getOutputInfo c (subTransactions b V.! 0) 0 >>= print
    assert True


canEstimateFees :: Property
canEstimateFees = monadicIO $ do
    run $ client >>= \c ->
        estimateSmartFee c 10 Nothing >>= print
    assert True


canGetAddress :: Property
canGetAddress = monadicIO $ do
    run $ do
        c <- client
        getNewAddress c Nothing >>= print
    assert True


canSendPayment :: Property
canSendPayment = monadicIO $ do
    run $ do
        c <- client
        addr <- getNewAddress c Nothing
        sendToAddress c addr (MkFixed 10000000) Nothing Nothing >>= print
    assert True
