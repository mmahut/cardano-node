{-# LANGUAGE OverloadedStrings #-}

module Cardano.Config.Shelley.Address
  ( AddressError(..)
  , AddressRole(..)
  , ShelleyAddress
  , genAddress
  , genBootstrapAddress
  , readAddress
  , renderAddressError
  , writeAddress
  ) where

import           Cardano.Prelude

import qualified Cardano.Binary as CBOR
import           Control.Monad.Trans.Except.Extra (firstExceptT, newExceptT)
import           Ouroboros.Consensus.Shelley.Protocol.Crypto
                   (TPraosStandardCrypto)
import           Shelley.Spec.Ledger.Address (toAddr)
import           Shelley.Spec.Ledger.Keys (KeyPair(..), hashKey)
import           Shelley.Spec.Ledger.TxData (Addr(..))

import           Cardano.Config.Shelley.ColdKeys (genKeyPair)
import           Cardano.Config.TextView

data AddressRole = BootstrapAddr
                 | NormalAddr

data AddressError = ReadAddressError TextViewFileError
                  | WriteAddressError TextViewFileError

type ShelleyAddress = Addr TPraosStandardCrypto

encodeAddress :: AddressRole -> ShelleyAddress -> TextView
encodeAddress addrRole addr =
  encodeToTextView fileType fileTitle CBOR.toCBOR addr
 where
  fileType = renderAddressRole addrRole
  fileTitle = renderAddressDescr addrRole


decodeAddress :: AddressRole -> TextView -> Either TextViewError ShelleyAddress
decodeAddress addrRole tView = do
  expectTextViewOfType fileType tView
  decodeFromTextView CBOR.fromCBOR tView
 where
  fileType = renderAddressRole addrRole

genAddress :: IO ShelleyAddress
genAddress = do
  (paymentVkey, paymentSkey) <- genKeyPair
  (stakingVkey, stakingSkey) <- genKeyPair
  pure $ toAddr ( KeyPair {sKey = paymentSkey, vKey = paymentVkey}
                , KeyPair {sKey = stakingSkey, vKey = stakingVkey}
                )

genBootstrapAddress :: IO ShelleyAddress
genBootstrapAddress = do
  (vKey', _) <- genKeyPair
  pure . AddrBootstrap $ hashKey vKey'

readAddress :: AddressRole -> FilePath -> ExceptT AddressError IO ShelleyAddress
readAddress role fp = do
  firstExceptT ReadAddressError $ newExceptT $
    readTextViewEncodedFile (decodeAddress role) fp

renderAddressRole :: AddressRole -> TextViewType
renderAddressRole BootstrapAddr = "Genesis"
renderAddressRole NormalAddr = "UTxO address"

renderAddressDescr :: AddressRole -> TextViewTitle
renderAddressDescr BootstrapAddr = "Bootstrap UTxO address"
renderAddressDescr NormalAddr = "UTxO address"

renderAddressError :: AddressError -> Text
renderAddressError err =
  case err of
    ReadAddressError tvErr -> "address read error: " <> renderTextViewFileError tvErr
    WriteAddressError tvErr -> "address write error: " <> renderTextViewFileError tvErr

writeAddress :: AddressRole -> FilePath -> ShelleyAddress -> ExceptT AddressError IO ()
writeAddress role fp addr =
  firstExceptT WriteAddressError $ newExceptT $
    writeTextViewEncodedFile (encodeAddress role) fp addr
