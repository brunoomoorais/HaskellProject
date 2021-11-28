{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase, GADTs #-}
module Backend where

import Common.Route
import Obelisk.Backend
import Database.PostgreSQL.Simple
import Data.Text
import Obelisk.Route
import Snap.Core
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as A
import Common.Api
import Data.Aeson.Text


migration :: Query
migration = "CREATE TABLE IF NOT EXISTS pet\
  \ (id SERIAL PRIMARY KEY, nome TEXT NOT NULL)"

migrateCliente :: Query
migrateCliente = "CREATE TABLE IF NOT EXISTS cliente (id SERIAL PRIMARY KEY, nome TEXT NOT NULL, contato TEXT NOT NULL)"

migratePet :: Query
migratePet = "CREATE TABLE IF NOT EXISTS petz (id SERIAL, tutorId INT, nome TEXT NOT NULL, tipo TEXT NOT NULL, constraint pk_pet primary key (tutorId, id), constraint fk_pet foreign key (tutorId) references cliente)"

migrateAgenda :: Query
migrateAgenda = "CREATE TABLE IF NOT EXISTS agenda (id SERIAL, tutorId INT, dataAgenda DATE NOT NULL, preco DOUBLE PRECISION, nomeServico TEXT NOT NULL, constraint pk_agenda primary key (tutorId, id), constraint fk_agenda foreign key (tutorId) references cliente)"


getConn :: ConnectInfo
getConn = ConnectInfo "ec2-44-198-80-194.compute-1.amazonaws.com"
                      5432 -- porta
                      "rfcjlufwdirbvm"
                      "f0d6b1feeb3c3feaf1dc3477a8078281e466096f35299520bbd8c7e441acb967"
                      "d41is3l3biup4f"

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
        dbcon <- connect getConn
        serve $ do 
          \case 
            BackendRoute_PetRoute :/ () -> do
                Just nome <- A.decode <$> readRequestBody 2000
                liftIO $ do 
                     execute_ dbcon migration
                     execute dbcon "INSERT INTO pet (nome) VALUES (?)" [nome :: Text]
                modifyResponse $ setResponseStatus 200 "OK"            
            BackendRoute_ClienteJson :/ () -> method POST $ do
                    client <- A.decode <$> readRequestBody 5000
                    case client of
                         Just cliente -> do
                             liftIO $ do
                                 execute_ dbcon migrateCliente
                                 execute dbcon "INSERT INTO cliente (nome,contato) VALUES (?,?)"
                                    (nome cliente, contato cliente) 
                             modifyResponse $ setResponseStatus 200 "OK"
                         Nothing -> modifyResponse $ setResponseStatus 500 "ERRO"            
            BackendRoute_AgendaJson :/ () -> method POST $ do
                    agend <- A.decode <$> readRequestBody 4000
                    case agend of
                         Just agenda -> do
                             liftIO $ do
                                 execute_ dbcon migrateAgenda
                                 execute dbcon "INSERT INTO agenda (tutorId, dataAgenda, preco, nomeServico) VALUES (?, ?, ?, ?)"
                                    (tutorId agenda, dataAgenda agenda, preco agenda, nomeServico agenda) 
                             modifyResponse $ setResponseStatus 200 "OK"
                         Nothing -> modifyResponse $ setResponseStatus 500 "ERRO"
            BackendRoute_PetJson :/ () -> method POST $ do
                    pet <- A.decode <$> readRequestBody 5000
                    case pet of
                         Just animal -> do
                             liftIO $ do
                                 execute_ dbcon migratePet
                                 execute dbcon "INSERT INTO petz (tutorId, nome, tipo) VALUES (?, ?, ?)"
                                    (tutorId animal, nomePet animal, racaPet animal) 
                             modifyResponse $ setResponseStatus 200 "OK"
                         Nothing -> modifyResponse $ setResponseStatus 500 "ERRO"            
            _ -> return ()
        return ()
  , _backend_routeEncoder = fullRouteEncoder
  }
