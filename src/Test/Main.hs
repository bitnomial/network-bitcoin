{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}


module Main where


import           Control.Monad            (void)
import           Data.Either              (isRight)
import           Data.Text                (Text)
import           Data.Vector              (empty)
import qualified Data.Vector              as V
import           Network.Bitcoin
import           Network.Bitcoin.Internal (callApi, tj)
import qualified Network.Bitcoin.Mining   as M
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import           Test.Tasty               (TestName, TestTree, defaultMain,
                                           testGroup)
import           Test.Tasty.QuickCheck


main :: IO ()
main = defaultMain . testGroup "network-bitcoin tests" $
    [ canGenerateBlocks
    , canListUnspent
    , canGetBlock
    , canGetOutputInfo
    , canGetRawTransaction
    , canGetAddress
    , canSendPayment
    ]


client :: IO Client
client = getClient "http://127.0.0.1:18444" "bitcoinrpc" "bitcoinrpcpassword"


nbTest name = testProperty name . once . monadicIO


canGenerateBlocks :: TestTree
canGenerateBlocks = nbTest "generateToAddress" $ do
    void . run $ do
        c             <- client
        rewardAddr <- getNewAddress c Nothing
        M.generateToAddress c 101 rewardAddr Nothing
    assert True


canListUnspent :: TestTree
canListUnspent = nbTest "listUnspent" $ do
    void . run $ do
        c <- client
        listUnspent c Nothing Nothing Data.Vector.empty
    assert True


getTopBlock :: Client -> IO Block
getTopBlock c = getBlockCount c >>= getBlockHash c >>= getBlock c


canGetBlock :: TestTree
canGetBlock = nbTest "getBlockCount / getBlockHash / getBlock" $ do
    run $ client >>= getTopBlock
    assert True


canGetRawTransaction :: TestTree
canGetRawTransaction = nbTest "getRawTransactionInfo" $ do
    run $ do
        c <- client
        b <- getTopBlock c
        getRawTransactionInfo c (subTransactions b V.! 0)
    assert True


canGetOutputInfo :: TestTree
canGetOutputInfo = nbTest "getOutputInfo" $ do
    run $ do
        c <- client
        b <- getTopBlock c
        getOutputInfo c (subTransactions b V.! 0) 0
    assert True


canGetAddress :: TestTree
canGetAddress = nbTest "getNewAddress" $ do
    run $ do
        c <- client
        getNewAddress c Nothing
    assert True


canSendPayment :: TestTree
canSendPayment = nbTest "send payment" $ do
    c   <- run client
    bal <- run $ getBalance c
    amt <- pick . suchThat arbitrary $ \x -> x < bal && x > 0

    (addr, recv) <- run $ do
        addr       <- getNewAddress c Nothing
        sendToAddress c addr amt Nothing Nothing
        M.generate c 6 Nothing
        (addr,) <$> listReceivedByAddress c

    assert . V.elem (addr, amt) . fmap f $ recv
  where
    f = (,) <$> recvAddress <*> recvAmount
