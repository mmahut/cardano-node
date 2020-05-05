{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
module Test.Cardano.Api.Gen
  ( genAddress
  , genCertificate
  , genNetwork
  , genSigningKey
  , genSigningKeyByron
  , genSigningKeyShelley
  , genTxIn
  , genTxOut
  , genTxSigned
  , genTxSignedByron
  , genTxUnsigned
  , genTxUnsignedByron
  , genVerificationKey
  , genVerificationKeyAddressByron
  , genVerificationKeyAddressShelley
  ) where

import           Cardano.Api
import           Cardano.Binary (serialize)
import qualified Cardano.Crypto as Byron
import           Cardano.Crypto.DSIGN
import           Cardano.Crypto.VRF.Class (deriveVerKeyVRF, genKeyVRF)
import           Cardano.Crypto.VRF.Simple (SimpleVRF)
import           Cardano.Prelude
import qualified Cardano.Crypto.Hash.Class as Crypto
import qualified Cardano.Crypto.Hash.Blake2b as Crypto

import           Crypto.Random (drgNewTest, withDRG)

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Coerce (coerce)
import qualified Data.Map.Strict as Map
import           Data.Ratio (approxRational)
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set


import           Shelley.Spec.Ledger.BaseTypes
                   (StrictMaybe (..), UnitInterval, mkDnsName, mkUnitInterval, mkUrl)
import           Shelley.Spec.Ledger.Coin (Coin (..))
import           Shelley.Spec.Ledger.Keys (SignKeyVRF, SKey(..), VerKeyVRF, hash, hashKey)
import           Shelley.Spec.Ledger.Slot (EpochNo(..))
import           Shelley.Spec.Ledger.TxData
                   (PoolMetaData (..), RewardAcnt (..), StakePoolRelay (..),
                    pattern TxBody, Wdrl (..))

import           Test.Cardano.Chain.UTxO.Gen (genTx)
import qualified Test.Cardano.Crypto.Gen as Byron

import           Hedgehog (Gen, Range)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range


genAddress :: Gen Address
genAddress =
  Gen.choice
    [ genVerificationKeyAddressByron
    , genVerificationKeyAddressShelley
    ]

genCertificate :: Gen Certificate
genCertificate = do
  Gen.choice
    [ -- Stake Key related
      genStakingKeyRegistrationCert
    , genStakingKeyDeregistrationCert
    , genStakingKeyDelegationCert
      -- Stake pool related
    , genShelleyStakePoolRegistrationCertificate
    , genShelleyStakePoolRetirementCertificate
    ]

genCredentialShelley :: Gen ShelleyCredential
genCredentialShelley = do
  vKeyHash <- genVerificationKeyHashShelley
  return $ mkShelleyCredential vKeyHash

genEpochNoShelly :: Gen EpochNo
genEpochNoShelly = do
  slot <- Gen.word64 (Range.linear minBound maxBound)
  return $ EpochNo slot

genStakingKeyRegistrationCert :: Gen Certificate
genStakingKeyRegistrationCert = do
  vKeyHash <- genVerificationKeyHashShelley
  pure $ shelleyRegisterStakingAddress vKeyHash

genStakingKeyDeregistrationCert :: Gen Certificate
genStakingKeyDeregistrationCert = do
  vKeyHash <- genVerificationKeyHashShelley
  pure $ shelleyDeregisterStakingAddress vKeyHash

genStakingKeyDelegationCert :: Gen Certificate
genStakingKeyDelegationCert = do
  delegator_vKeyHash <- genVerificationKeyHashShelley

  delegatee_vKeyHash <- genVerificationKeyHashShelley
  pure $ shelleyDelegateStake delegator_vKeyHash delegatee_vKeyHash

genNetwork :: Gen Network
genNetwork =
  Gen.choice
    [ pure Mainnet
    , Testnet <$> Byron.genProtocolMagicId
    ]

genRewardAccountShelley :: Gen ShelleyRewardAccount
genRewardAccountShelley = do
  cred <- genCredentialShelley
  return $ RewardAcnt cred

genSigningKey :: Gen SigningKey
genSigningKey =
  Gen.choice
    [ genSigningKeyByron
    , genSigningKeyShelley
    ]

genSigningKeyByron :: Gen SigningKey
genSigningKeyByron =
  SigningKeyByron <$> Byron.genSigningKey

genSigningKeyShelley :: Gen SigningKey
genSigningKeyShelley = do
  seed <- genSeed
  let sk = fst (withDRG (drgNewTest seed) genKeyDSIGN)
  return $ SigningKeyShelley (SKey sk)

genShelleyStakePoolRegistrationCertificate :: Gen Certificate
genShelleyStakePoolRegistrationCertificate =
  shelleyRegisterStakePool
    <$> genVerificationKeyHashShelley
    <*> genVRFVerificationKeyHashShelley
    <*> (Coin <$> Gen.integral (Range.linear 0 10000000000))
    <*> (Coin <$> Gen.integral (Range.linear 0 10000000000))
    <*> genStakePoolMarginShelley
    <*> genRewardAccountShelley
    <*> genStakePoolOwnersShelley
    <*> Gen.list (Range.linear 1 5) genStakePoolRelayShelley
    <*> (Just <$> genPoolMetaDataShelley)

-- TODO: Improve this generator.
genPoolMetaDataShelley :: Gen ShelleyStakePoolMetaData
genPoolMetaDataShelley = return $ PoolMetaData (mkUrl "Test") "Test"

-- TODO: Cover SingleHostName and MultiHostName data constructors
-- It also seems impossible to construct the `Port` type as the
-- constructors aren't exposed nor a function to create one.
genStakePoolRelayShelley :: Gen ShelleyStakePoolRelay
genStakePoolRelayShelley = do
  --port <- Gen.word16 (Range.linear minBound maxBound)
  return $ SingleHostName SNothing (mkDnsName "Test")

genShelleyStakePoolRetirementCertificate :: Gen Certificate
genShelleyStakePoolRetirementCertificate = do
  vKey <- genVerificationKeyShelley
  epochNo <- genEpochNoShelly
  return $ shelleyRetireStakePool vKey epochNo

genStakePoolOwnersShelley :: Gen ShelleyStakePoolOwners
genStakePoolOwnersShelley = do
  keyHashes <- Gen.list (Range.linear 1 5) genVerificationKeyHashShelley
  return $ Set.fromList keyHashes

-- TODO: Should make sure this covers
-- the 0 to 1 range adequately.
genStakePoolMarginShelley :: Gen UnitInterval
genStakePoolMarginShelley = do
  numerator' <- Gen.double (Range.constantFrom 0 0 1)
  Gen.just . pure $ mkUnitInterval $ approxRational numerator' 1

genTxIn :: Gen TxIn
genTxIn =
  TxIn <$> genFakeTxId <*> Gen.word (Range.linear 0 10000)

genTxOut :: Gen TxOut
genTxOut =
  TxOut <$> genAddress <*> genLovelace

genTxSigned :: Gen TxSigned
genTxSigned =
  Gen.choice
    [ genTxSignedByron
    , genTxSignedShelley
    ]

genTxSignedByron :: Gen TxSigned
genTxSignedByron =
  signTransaction
    <$> genTxUnsignedByron
    <*> genNetwork
    <*> Gen.list (Range.linear 1 5) genSigningKeyByron

genTxSignedShelley :: Gen TxSigned
genTxSignedShelley =
  signTransaction
    <$> genTxUnsignedShelley
    <*> genNetwork
    <*> Gen.list (Range.linear 1 5) genSigningKeyShelley

genTxUnsigned :: Gen TxUnsigned
genTxUnsigned =
  Gen.choice
    [ genTxUnsignedByron
    , genTxUnsignedShelley
    ]

genTxUnsignedByron :: Gen TxUnsigned
genTxUnsignedByron = do
  tx <- genTx
  let cbor = serialize tx
  pure $ TxUnsignedByron tx (LBS.toStrict cbor) (coerce $ Byron.hashRaw cbor)

genTxUnsignedShelley :: Gen TxUnsigned
genTxUnsignedShelley =
    TxUnsignedShelley <$> genTxBodyShelley
  where
    -- TODO: Improve this incredibly naive generator.
    genTxBodyShelley :: Gen ShelleyTxBody
    genTxBodyShelley = do
      coin <- Gen.integral (Range.linear 0 10000000000)
      slot <- Gen.word64 (Range.linear minBound maxBound)
      pure $ TxBody (Set.fromList []) (StrictSeq.fromList []) (StrictSeq.fromList [])
                (Wdrl $ Map.fromList []) (Coin coin) (SlotNo slot) SNothing SNothing

genVerificationKey :: Gen VerificationKey
genVerificationKey =
  Gen.choice
    [ genVerificationKeyByron
    , genVerificationKeyShelley
    ]

genVerificationKeyAddressByron :: Gen Address
genVerificationKeyAddressByron =
  byronVerificationKeyAddress <$> genVerificationKeyByron <*> genNetwork

genVerificationKeyAddressShelley :: Gen Address
genVerificationKeyAddressShelley =
  shelleyVerificationKeyAddress <$> genVerificationKeyShelley <*> genNetwork

genVerificationKeyByron :: Gen VerificationKey
genVerificationKeyByron =
  getVerificationKey <$> genSigningKeyByron

genVerificationKeyShelley :: Gen VerificationKey
genVerificationKeyShelley =
  getVerificationKey <$> genSigningKeyShelley

genVerificationKeyHashShelley :: Gen ShelleyVerificationKeyHash
genVerificationKeyHashShelley = do
  VerificationKeyShelley vk <- genVerificationKeyShelley
  return $ hashKey vk

genVRFKeyPair :: Gen (SignKeyVRF SimpleVRF, VerKeyVRF SimpleVRF)
genVRFKeyPair = do
  seed <- genSeed
  pure $ fst $ withDRG (drgNewTest seed) $ do
     sk <- genKeyVRF
     let vk = deriveVerKeyVRF sk
     pure (sk, vk)

genVRFVerificationKeyHashShelley :: Gen ShelleyVRFVerificationKeyHash
genVRFVerificationKeyHashShelley = do
  (_, vrfVKey) <- genVRFKeyPair
  return $ hash vrfVKey

-- -------------------------------------------------------------------------------------------------

genByteString :: Range Int -> Gen ByteString
genByteString lenRange =
  BS.pack <$> Gen.list lenRange (Gen.element [ '\x00' .. '\xff' ])

-- Generates a fake TxId by applying the right hashing function to a random ByteString.
genFakeTxId :: Gen TxId
genFakeTxId =
  TxId . {- Crypto. -} hashRaw identity <$> genByteString (Range.linear 10 50)

-- This name will clash with one in Cardano.Crypto.Hash.Class.
-- This should be removed (or maybe specialized) then the one in Cardano.Crypto.Hash.Class is
-- available.
hashRaw :: (a -> ByteString) -> a -> Crypto.Hash Crypto.Blake2b_256 ()
hashRaw serialise = Crypto.UnsafeHash . Crypto.digest (Proxy :: Proxy Crypto.Blake2b_256) . serialise

genLovelace :: Gen Lovelace
genLovelace =
  Lovelace <$> Gen.integral (Range.linear 1 999999999999)

genSeed :: Gen (Word64, Word64, Word64, Word64, Word64)
genSeed =
  (,,,,)
    <$> Gen.word64 Range.constantBounded
    <*> Gen.word64 Range.constantBounded
    <*> Gen.word64 Range.constantBounded
    <*> Gen.word64 Range.constantBounded
    <*> Gen.word64 Range.constantBounded
