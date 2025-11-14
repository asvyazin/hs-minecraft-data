{-# LANGUAGE OverloadedStrings #-}

module Data.Minecraft.Protocol where

import Data.Aeson (FromJSON (parseJSON), Key, Value (Array, String), withArray, withObject, (.:), (.:?))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.Text (Text)
import Data.Vector qualified as V

data ProtocolTypeArrayData = ProtocolTypeArrayData
  { protocolTypeArrayDataCountType :: Maybe ProtocolType,
    protocolTypeArrayDataCount :: Maybe Text,
    protocolTypeArrayDataType :: ProtocolType
  }
  deriving (Show)

data ProtocolTypeContainerField = ProtocolTypeObjectField
  { protocolTypeContainerFieldName :: Maybe Text,
    protocolTypeContainerFieldAnon :: Maybe Bool,
    protocolTypeContainerFieldType :: ProtocolType
  }
  deriving (Show)

data ProtocolTypeRegistryEntryHolderOtherwise = ProtocolRegistryEntryHolderOtherwise
  { protocolTypeRegistryEntryHolderOtherwiseName :: Text,
    protocolTypeRegistryEntryHolderOtherwiseType :: ProtocolType
  }
  deriving (Show)

data ProtocolTypeBitfieldField = ProtocolTypeBitfieldField
  { protocolTypeBitfieldFieldName :: Text,
    protocolTypeBitfieldFieldSigned :: Bool,
    protocolTypeBitfieldFieldSize :: Int
  }
  deriving (Show)

data ProtocolType
  = ProtocolTypeNative
  | ProtocolTypeRef Text
  | ProtocolTypeContainer [ProtocolTypeContainerField]
  | ProtocolTypeArray ProtocolTypeArrayData
  | ProtocolTypeRegistryEntryHolder Text ProtocolTypeRegistryEntryHolderOtherwise -- baseName, otherwise=(name, type)
  | ProtocolTypeRegistryEntryHolderSet ProtocolTypeRegistryEntryHolderOtherwise ProtocolTypeRegistryEntryHolderOtherwise -- base, otherwise=(name, type)
  | ProtocolTypeBuffer (Maybe ProtocolType) (Maybe Int) -- countType, count
  | ProtocolTypeOption ProtocolType
  | ProtocolTypeSwitch Text (HashMap.HashMap Text ProtocolType) (Maybe ProtocolType) -- compareTo, fields, default
  | ProtocolTypeMapper ProtocolType (HashMap.HashMap Text Text) -- type (probably always varint), mappings (key, value)
  | ProtocolTypeBitfield [ProtocolTypeBitfieldField]
  | ProtocolTypeEntityMetadataLoop Int ProtocolType -- endVal type
  | ProtocolTypePString ProtocolType -- countType
  | ProtocolTypeBitflags [Text] ProtocolType -- flags, type
  | ProtocolTypeTopBitSetTerminatedArray ProtocolType -- type
  deriving (Show)

instance FromJSON ProtocolTypeContainerField where
  parseJSON = withObject "ProtocolTypeContainerField" $ \v -> do
    let expectedKeys :: HashSet.HashSet Key =
          HashSet.fromList ["name", "anon", "type"]
        realKeys =
          HashSet.fromList $ KeyMap.keys v
        unknownKeys =
          HashSet.difference realKeys expectedKeys
    if not (HashSet.null unknownKeys)
      then
        fail $ "Unknown keys: " ++ show unknownKeys
      else
        ProtocolTypeObjectField
          <$> v .:? "name"
          <*> v .:? "anon"
          <*> v .: "type"

instance FromJSON ProtocolTypeArrayData where
  parseJSON = withObject "ProtocolTypeArrayData" $ \v -> do
    let expectedKeys :: HashSet.HashSet Key =
          HashSet.fromList ["countType", "count", "type"]
        realKeys =
          HashSet.fromList $ KeyMap.keys v
        unknownKeys =
          HashSet.difference realKeys expectedKeys
    if not (HashSet.null unknownKeys)
      then
        fail $ "Unknown keys: " ++ show unknownKeys
      else
        ProtocolTypeArrayData
          <$> v .:? "countType"
          <*> v .:? "count"
          <*> v .: "type"

instance FromJSON ProtocolTypeRegistryEntryHolderOtherwise where
  parseJSON = withObject "ProtocolRegistryEntryHolderOtherwise" $ \v -> do
    let expectedKeys :: HashSet.HashSet Key =
          HashSet.fromList ["name", "type"]
        realKeys =
          HashSet.fromList $ KeyMap.keys v
        unknownKeys =
          HashSet.difference realKeys expectedKeys
    if not (HashSet.null unknownKeys)
      then
        fail $ "Unknown keys: " ++ show unknownKeys
      else
        ProtocolRegistryEntryHolderOtherwise
          <$> v .: "name"
          <*> v .: "type"

instance FromJSON ProtocolTypeBitfieldField where
  parseJSON = withObject "ProtocolTypeBitfieldField" $ \v -> do
    let expectedKeys :: HashSet.HashSet Key =
          HashSet.fromList ["name", "signed", "size"]
        realKeys =
          HashSet.fromList $ KeyMap.keys v
        unknownKeys =
          HashSet.difference realKeys expectedKeys
    if not (HashSet.null unknownKeys)
      then
        fail $ "Unknown keys: " ++ show unknownKeys
      else
        ProtocolTypeBitfieldField
          <$> v .: "name"
          <*> v .: "signed"
          <*> v .: "size"

instance FromJSON ProtocolType where
  parseJSON (String "native") = pure ProtocolTypeNative
  parseJSON (String ref) = pure $ ProtocolTypeRef ref
  parseJSON (Array vec) =
    case V.toList vec of
      [String "container", arr] ->
        withArray
          "container"
          ( \v ->
              ProtocolTypeContainer <$> mapM parseJSON (V.toList v)
          )
          arr
      [String "array", arrayTypeData] ->
        ProtocolTypeArray <$> parseJSON arrayTypeData
      [String "registryEntryHolder", obj] ->
        withObject
          "registryEntryHolder"
          ( \v ->
              ProtocolTypeRegistryEntryHolder <$> v .: "baseName" <*> v .: "otherwise"
          )
          obj
      [String "registryEntryHolderSet", obj] ->
        withObject
          "registryEntryHolderSet"
          ( \v ->
              ProtocolTypeRegistryEntryHolderSet <$> v .: "base" <*> v .: "otherwise"
          )
          obj
      [String "buffer", obj] ->
        withObject
          "buffer"
          ( \v ->
              ProtocolTypeBuffer <$> v .:? "countType" <*> v .:? "count"
          )
          obj
      [String "switch", obj] ->
        withObject
          "switch"
          ( \v ->
              ProtocolTypeSwitch <$> v .: "compareTo" <*> v .: "fields" <*> v .:? "default"
          )
          obj
      [String "mapper", obj] ->
        withObject
          "mapper"
          ( \v ->
              ProtocolTypeMapper <$> v .: "type" <*> v .: "mappings"
          )
          obj
      [String "entityMetadataLoop", obj] ->
        withObject
          "entityMetadataLoop"
          ( \v ->
              ProtocolTypeEntityMetadataLoop <$> v .: "endVal" <*> v .: "type"
          )
          obj
      [String "pstring", obj] ->
        withObject
          "pstring"
          ( \v ->
              ProtocolTypePString <$> v .: "countType"
          )
          obj
      [String "bitflags", obj] ->
        withObject
          "bitflags"
          ( \v ->
              ProtocolTypeBitflags <$> v .: "flags" <*> v .: "type"
          )
          obj
      [String "topBitSetTerminatedArray", obj] ->
        withObject
          "topBitSetTerminatedArray"
          ( \v ->
              ProtocolTypeTopBitSetTerminatedArray <$> v .: "type"
          )
          obj
      [String "bitfield", arr] ->
        ProtocolTypeBitfield <$> parseJSON arr
      [String "option", subType] ->
        ProtocolTypeOption <$> parseJSON subType
      unk@(String _ : _) ->
        fail $ "Unknown ProtocolType array: " ++ show unk
      unk ->
        fail $ "Unknown ProtocolType: " ++ show unk
  parseJSON x = fail $ "Unknown ProtocolType: " ++ show x

newtype ProtocolPacket = ProtocolPacket
  { protocolPacketTypes :: HashMap.HashMap Text ProtocolType
  }
  deriving (Show)

instance FromJSON ProtocolPacket where
  parseJSON = withObject "ProtocolPacket" $ \v -> do
    let expectedKeys :: HashSet.HashSet Key =
          HashSet.fromList ["types"]
        realKeys =
          HashSet.fromList $ KeyMap.keys v
        unknownKeys =
          HashSet.difference realKeys expectedKeys
    if not (HashSet.null unknownKeys)
      then
        fail $ "Unknown keys: " ++ show unknownKeys
      else
        ProtocolPacket <$> v .: "types"

data ProtocolInteraction = ProtocolInteraction
  { protocolInteractionToClient :: ProtocolPacket,
    protocolInteractionToServer :: ProtocolPacket
  }
  deriving (Show)

instance FromJSON ProtocolInteraction where
  parseJSON = withObject "ProtocolInteraction" $ \v -> do
    let expectedKeys :: HashSet.HashSet Key =
          HashSet.fromList ["toClient", "toServer"]
        realKeys =
          HashSet.fromList $ KeyMap.keys v
        unknownKeys =
          HashSet.difference realKeys expectedKeys
    if not (HashSet.null unknownKeys)
      then
        fail $ "Unknown keys: " ++ show unknownKeys
      else
        ProtocolInteraction <$> v .: "toClient" <*> v .: "toServer"

data Protocol = Protocol
  { protocolTypes :: HashMap.HashMap Text ProtocolType,
    protocolHandshaking :: ProtocolInteraction,
    protocolLogin :: ProtocolInteraction,
    protocolConfiguration :: ProtocolInteraction,
    protocolStatus :: ProtocolInteraction,
    protocolPlay :: ProtocolInteraction
  }
  deriving (Show)

instance FromJSON Protocol where
  parseJSON = withObject "Protocol" $ \v -> do
    let expectedKeys :: HashSet.HashSet Key =
          HashSet.fromList ["types", "handshaking", "login", "configuration", "status", "play"]
        realKeys =
          HashSet.fromList $ KeyMap.keys v
        unknownKeys =
          HashSet.difference realKeys expectedKeys
    if not (HashSet.null unknownKeys)
      then
        fail $ "Unknown keys: " ++ show unknownKeys
      else
        Protocol
          <$> v .: "types"
          <*> v .: "handshaking"
          <*> v .: "login"
          <*> v .: "configuration"
          <*> v .: "status"
          <*> v .: "play"