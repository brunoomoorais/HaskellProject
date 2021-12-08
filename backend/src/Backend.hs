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
migratePet = "CREATE TABLE IF NOT EXISTS petz (id SERIAL, tutorId INT, nome TEXT NOT NULL, tipo TEXT NOT NULL, constraint pk_pet primary key (id), constraint fk_pet foreign key (tutorId) references cliente)"

migrateAgenda :: Query
migrateAgenda = "CREATE TABLE IF NOT EXISTS agenda (id SERIAL, tutorId INT, dataAgenda DATE NOT NULL, preco DOUBLE PRECISION, nomeServico TEXT NOT NULL, constraint pk_agenda primary key (id), constraint fk_agenda foreign key (tutorId) references cliente)"


getConn :: ConnectInfo
getConn = ConnectInfo ""
                      5432 -- porta
                      ""
                      ""
                      ""

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
        dbcon <- connect getConn
        serve $ do 
          \case            
            --gets--
            BackendRoute_ClienteListar :/ () -> method GET $ do
                    res :: [ClienteJson] <- liftIO $ do
                        execute_ dbcon migrateCliente
                        query_ dbcon "SELECT * from cliente" 
                    modifyResponse $ setResponseStatus 200 "OK"
                    writeLazyText (encodeToLazyText res)
            BackendRoute_AgendaListar :/ () -> method GET $ do
                    res :: [GetAgendaJson] <- liftIO $ do
                        execute_ dbcon migrateAgenda
                        query_ dbcon "select a.id, a.tutorId, c.nome, c.contato, to_char(a.dataAgenda, 'dd/MM/yyyy'), a.preco, a.nomeservico from agenda a join cliente c on c.id = a.tutorId order by a.dataAgenda desc" 
                    modifyResponse $ setResponseStatus 200 "OK"
                    writeLazyText (encodeToLazyText res)
            BackendRoute_PetListar :/ () -> method GET $ do
                    res :: [GetPetJsonObject] <- liftIO $ do
                        execute_ dbcon migratePet
                        query_ dbcon "select p.id, p.nome, p.tipo, c.id, c.nome, c.contato from petz p join cliente c on c.id = p.tutorId order by c.nome" 
                    modifyResponse $ setResponseStatus 200 "OK"
                    writeLazyText (encodeToLazyText res)
            --inserts--
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
                                    (clienteAgendaId agenda, dataAgenda agenda, preco agenda, nomeServico agenda) 
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
            --getId--
            BackendRoute_ClienteBuscar :/ cid -> method GET $ do
                    res :: [ClienteJson] <- liftIO $ do
                        execute_ dbcon migrateCliente
                        query dbcon "SELECT * from cliente WHERE id=?" (Only (cid :: Int))
                    if res /= [] then do
                        modifyResponse $ setResponseStatus 200 "OK"
                        writeLazyText (encodeToLazyText (Prelude.head res))
                    else
                        modifyResponse $ setResponseStatus 404 "NOT FOUND"
            BackendRoute_AgendaBuscar :/ aid -> method GET $ do
                    res :: [GetAgendaJson] <- liftIO $ do
                        execute_ dbcon migrateAgenda
                        query dbcon "select a.id, a.tutorId, c.nome, c.contato, to_char(a.dataAgenda, 'yyyy-MM-dd'), a.preco, a.nomeservico from agenda a join cliente c on c.id = a.tutorId WHERE a.id=?" (Only (aid :: Int))
                    if res /= [] then do
                        modifyResponse $ setResponseStatus 200 "OK"
                        writeLazyText (encodeToLazyText (Prelude.head res))
                    else
                        modifyResponse $ setResponseStatus 404 "NOT FOUND"
            BackendRoute_AgendaBuscarId :/ aid -> method GET $ do
                    res :: [GetAgendaJson] <- liftIO $ do
                        execute_ dbcon migrateAgenda
                        query dbcon "select a.id, a.tutorId, c.nome, c.contato, to_char(a.dataAgenda, 'dd/MM/yyyy'), a.preco, a.nomeservico from agenda a join cliente c on c.id = a.tutorId WHERE a.id=?" (Only (aid :: Int))
                    if res /= [] then do
                        modifyResponse $ setResponseStatus 200 "OK"
                        writeLazyText (encodeToLazyText (Prelude.head res))
                    else
                        modifyResponse $ setResponseStatus 404 "NOT FOUND"
            BackendRoute_PetBuscar :/ pid -> method GET $ do
                    res :: [PetJsonObject] <- liftIO $ do
                        execute_ dbcon migratePet
                        query dbcon "SELECT * from petz WHERE id=?" (Only (pid :: Int))
                    if res /= [] then do
                        modifyResponse $ setResponseStatus 200 "OK"
                        writeLazyText (encodeToLazyText (Prelude.head res))
                    else
                        modifyResponse $ setResponseStatus 404 "NOT FOUND"
            --delete--
            BackendRoute_ClienteDelete :/ dcid -> method POST $ do
                res :: [ClienteJson] <- liftIO $ do
                    execute_ dbcon migrateCliente
                    query dbcon "SELECT * from cliente where id=?" (Only (dcid :: Int))
                if res /= [] then do
                    liftIO $ do
                        execute_ dbcon migrateCliente
                        execute dbcon "DELETE from cliente where id=?" (Only (dcid :: Int))
                    modifyResponse $ setResponseStatus 200 "OK"   
                else
                    modifyResponse $ setResponseStatus 404 "NOT FOUND"  
            BackendRoute_AgendaDelete :/ dcid -> method POST $ do
                res :: [GetAgendaJson] <- liftIO $ do
                    execute_ dbcon migrateAgenda
                    query dbcon "select a.id, a.tutorId, c.nome, c.contato, to_char(a.dataAgenda, 'dd/MM/yyyy'), a.preco, a.nomeservico from agenda a join cliente c on c.id = a.tutorId WHERE a.id=?" (Only (dcid :: Int))
                if res /= [] then do
                    liftIO $ do
                        execute_ dbcon migrateCliente
                        execute dbcon "DELETE from agenda where id=?" (Only (dcid :: Int))
                    modifyResponse $ setResponseStatus 200 "OK"   
                else
                    modifyResponse $ setResponseStatus 404 "NOT FOUND"  
            BackendRoute_PetDelete :/ dcid -> method POST $ do
                res :: [PetJsonObject] <- liftIO $ do
                    execute_ dbcon migratePet
                    query dbcon "SELECT * from petz where id = ?" (Only (dcid :: Int))
                if res /= [] then do
                    liftIO $ do
                        execute_ dbcon migrateCliente
                        execute dbcon "DELETE from petz where id=?" (Only (dcid :: Int))
                    modifyResponse $ setResponseStatus 200 "OK"   
                else
                    modifyResponse $ setResponseStatus 404 "NOT FOUND"  
            ----------update------------
            BackendRoute_ClienteEditar :/ pid -> method POST $ do
                client <- A.decode <$> readRequestBody 2000
                case client of
                    Just cliente -> do
                        liftIO $ do
                            execute_ dbcon migrateCliente
                            execute dbcon "UPDATE cliente SET nome = ?, contato = ? WHERE id = ?" 
                                       (nome cliente, contato cliente, pid)
                        modifyResponse $ setResponseStatus 200 "OK"
                    Nothing -> modifyResponse $ setResponseStatus 500 "ERRO"
            BackendRoute_AgendaEditar :/ pid -> method POST $ do
                scheduler <- A.decode <$> readRequestBody 2000
                case scheduler of
                    Just agenda -> do
                        liftIO $ do
                            execute_ dbcon migrateAgenda
                            execute dbcon "UPDATE agenda SET dataAgenda = ?, preco = ?, nomeServico = ? WHERE id = ?" 
                                       (dataAgenda agenda, preco agenda, nomeServico agenda, pid)
                        modifyResponse $ setResponseStatus 200 "OK"
                    Nothing -> modifyResponse $ setResponseStatus 500 "ERRO"
            BackendRoute_PetEditar :/ pid -> method POST $ do
                pet <- A.decode <$> readRequestBody 2000
                case pet of
                    Just animal -> do
                        liftIO $ do
                            execute_ dbcon migratePet
                            execute dbcon "UPDATE petz SET nome = ?, tipo = ? WHERE id = ?" 
                                       (nomePet animal, racaPet animal, pid)
                        modifyResponse $ setResponseStatus 200 "OK"
                    Nothing -> modifyResponse $ setResponseStatus 500 "ERRO"
            _ -> return ()
  , _backend_routeEncoder = fullRouteEncoder
  }
