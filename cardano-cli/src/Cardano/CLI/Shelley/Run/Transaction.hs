{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Cardano.CLI.Shelley.Run.Transaction
  ( runTransactionCmd
  ) where

import           Cardano.Prelude

import           Cardano.Api
import           Cardano.CLI.Ops (CliError (..))
import           Cardano.CLI.Shelley.Parsers
import           Cardano.Config.Types (CertificateFile (..))

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, newExceptT)


runTransactionCmd :: TransactionCmd -> ExceptT CliError IO ()
runTransactionCmd cmd =
  case cmd of
    TxBuildRaw txins txouts ttl fee out certs -> runTxBuildRaw txins txouts ttl fee out certs
    _ -> liftIO $ putStrLn $ "runTransactionCmd: " ++ show cmd

runTxBuildRaw
  :: [TxIn]
  -> [TxOut]
  -> SlotNo
  -> Lovelace
  -> TxBodyFile
  -> [CertificateFile]
  -> ExceptT CliError IO ()
runTxBuildRaw txins txouts ttl amount (TxBodyFile fpath) certFps = do
  certs <- mapM readShelleyCert certFps
  firstExceptT CardanoApiError
    . newExceptT
    . writeTxUnsigned fpath
    $ buildShelleyTransaction txins txouts ttl amount certs

 where
   -- TODO: This should exist in its own module along with
   -- a custom error type and an error rendering function.
   readShelleyCert :: CertificateFile -> ExceptT CliError IO Certificate
   readShelleyCert (CertificateFile fp) =
      firstExceptT ShelleyCertReadError . newExceptT $ readCertificate fp