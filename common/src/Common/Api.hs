{-# LANGUAGE DeriveGeneric #-}
{-# language DeriveAnyClass  #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Common.Api where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)
import Database.PostgreSQL.Simple

data PetJson = PetJson Text deriving (Generic, ToJSON, FromJSON)

data ClienteJson = ClienteJson {
    clienteId :: Int,
    nome :: Text,
    contato :: Text    
} deriving (Generic, ToJSON, FromJSON, ToRow, FromRow, Eq, Show)


data AgendaJson = AgendaJson {
    agendaId :: Int,
    clienteAgendaId :: Int,
    dataAgenda :: Text,    
    preco :: Double,
    nomeServico :: Text
} deriving (Generic, ToJSON, FromJSON, ToRow, FromRow, Eq, Show)

data GetAgendaJson = GetAgendaJson {
    agendaIdGet :: Int,
    donoIdGet :: Int,
    donoNomeGet :: Text,
    donoContatoGet :: Text,
    dataAgendaGet :: Text,    
    precoGet :: Double,
    nomeServicoGet :: Text
} deriving (Generic, ToJSON, FromJSON, ToRow, FromRow, Eq, Show)

data PetJsonObject = PetJsonObject {
    petId :: Int,
    tutorId :: Int,
    nomePet :: Text,
    racaPet :: Text
} deriving (Generic, ToJSON, FromJSON, ToRow, FromRow, Eq, Show)

data GetPetJsonObject = GetPetJsonObject {
    petIdGet :: Int,
    nomePetGet :: Text,
    racaPetGet :: Text,
    tutorIdGet :: Int,
    tutorNomeGet :: Text,
    tutorContatoGet :: Text
} deriving (Generic, ToJSON, FromJSON, ToRow, FromRow, Eq, Show)

data ResponseDelete = ResponseDelete Int deriving (Generic, ToJSON, FromJSON, ToRow, FromRow, Eq, Show)