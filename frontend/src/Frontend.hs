{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications, ScopedTypeVariables #-}

module Frontend where

import Control.Monad
import qualified Data.Text as T
import Language.Javascript.JSaddle (eval, liftJSM)
import Control.Monad.Fix
import Obelisk.Frontend
import Obelisk.Configs
import Obelisk.Route
import Obelisk.Generated.Static
import Data.Map (Map)
import Reflex.Dom.Core
import Text.Read
import Data.Maybe
import Common.Api
import Common.Route
import Data.Aeson


-- This runs in a monad that can be run on the client or the server.
-- To run code in a pure client or pure server context, use one of the
-- `prerender` functions.

------BACKEND-----------

getPath :: R BackendRoute -> T.Text
getPath r = renderBackendRoute checFullREnc r

getListReqClienteCliente :: XhrRequest ()
getListReqClienteCliente = xhrRequest "GET" (getPath (BackendRoute_ClienteListar :/ ())) def

-- deleteReqClienteCliente :: XhrRequest ()
-- deleteReqClienteCliente = xhrRequest "GET" (getPath (BackendRoute_ClienteListar :/ ())) def

getListReqAgenda :: XhrRequest ()
getListReqAgenda = xhrRequest "GET" (getPath (BackendRoute_AgendaListar :/ ())) def

-- deleteReqAgenda :: Int -> XhrRequest ()
-- deleteReqAgenda id = xhrRequest "DELETE" (getPath (BackendRoute_AgendaDelete :/ id)) def

-- deleteReqCliente :: Int -> XhrRequest ()
-- deleteReqCliente id = xhrRequest "DELETE" (getPath (BackendRoute_ClienteDelete :/ id)) def

-- deleteReqPet :: Int -> XhrRequest ()
-- deleteReqPet id = xhrRequest "DELETE" (getPath (BackendRoute_PetDelete :/ id)) def

getListReqPet :: XhrRequest ()
getListReqPet = xhrRequest "GET" (getPath (BackendRoute_PetListar :/ ())) def

sendRequest :: ToJSON a => R BackendRoute -> a -> XhrRequest T.Text
sendRequest r dados = postJson (getPath r) dados


---------------------- INSERTS -------------------------------------
-- pagReqPet' :: ( DomBuilder t m
--            , Prerender js t m
--            ) => m ()
-- pagReqPet' = do
--     el "h3" (text "Pet - Adicionar")
--     el "hr" (blank)
--     elAttr "p" ("class" =: "title") (text "Id do cliente:") 
--     id <- numberInputSecond
--     elAttr "p" ("class" =: "title") (text "Nome do pet:") 
--     nome <- inputElement def
--     elAttr "p" ("class" =: "title") (text "Tipo do pet:") 
--     tipo <- inputElement def
--     (backBtn,_) <- elAttr' "button" ("class"=:"btn btn-danger") (text "Voltar")    
--     let object = fmap (\((i,n),r) -> PetJsonObject 0 i n r) (zipDyn (zipDyn id  (_inputElement_value nome)) (_inputElement_value tipo))
--     (submitBtn,_) <- elAttr' "button" ("class"=:"btn btn-primary") (text "Adicionar")
--     let click = domEvent Click submitBtn
--     let prodEvt = tag (current object) click
--     _ :: Dynamic t (Event t (Maybe T.Text)) <- prerender
--         (pure never)
--         (fmap decodeXhrResponse <$> performRequestAsync (sendRequest (BackendRoute_PetJson :/ ()) <$> prodEvt))
--     return ()

pagReqCliente :: ( DomBuilder t m
           , Prerender js t m
           ) => m ()
pagReqCliente = do
    el "h3" (text "Cliente - Adicionar")
    el "hr" (blank)
    elAttr "p" ("class" =: "title") (text "Nome:") 
    nome <- inputElement def
    elAttr "p" ("class" =: "title") (text "Contato:") 
    contato <- inputElement def
    (backBtn,_) <- elAttr' "button" ("class"=:"btn btn-danger") (text "Voltar")
    -- let back = ((\_ -> Cliente) <$> domEvent Click backBtn)    
    let object = fmap (\(n,c) -> ClienteJson 0 n c) (zipDyn (_inputElement_value nome) (_inputElement_value contato))
    (submitBtn,_) <- elAttr' "button" ("class"=:"btn btn-primary") (text "Adicionar")
    let click = domEvent Click submitBtn
    let prodEvt = tag (current object) click
    _ :: Dynamic t (Event t (Maybe T.Text)) <- prerender
        (pure never)
        (fmap decodeXhrResponse <$> performRequestAsync (sendRequest (BackendRoute_ClienteJson :/ ()) <$> prodEvt))    
    return () 

pagReqAgenda :: ( DomBuilder t m
           , Prerender js t m
           ) => m ()
pagReqAgenda = do
    el "h3" (text "Agenda - Adicionar")
    el "hr" (blank)
    elAttr "p" ("class" =: "title") (text "Id do cliente:") 
    id <- numberInputSecond
    elAttr "p" ("class" =: "title") (text "Data do serviço:") 
    date <- inputElement def
    elAttr "p" ("class" =: "title") (text "Preço:") 
    preco <- numberInputSecond
    elAttr "p" ("class" =: "title") (text "Nome do serviço:") 
    nomeServico <- inputElement def
    (backBtn,_) <- elAttr' "button" ("class"=:"btn btn-danger") (text "Voltar") 
    let object = fmap (\((c,d),(p, n)) -> AgendaJson 0 c d p n) (zipDyn (zipDyn id  (_inputElement_value date)) (zipDyn preco  (_inputElement_value nomeServico)))
    (submitBtn,_) <- elAttr' "button" ("class"=:"btn btn-primary") (text "Adicionar")
    let clickAgenda = domEvent Click submitBtn
    let prodEvtAgenda = tag (current object) clickAgenda
    _ :: Dynamic t (Event t (Maybe T.Text)) <- prerender
        (pure never)
        (fmap decodeXhrResponse <$> performRequestAsync (sendRequest (BackendRoute_AgendaJson :/ ()) <$> prodEvtAgenda))
    return () 
-------------------------- CLIENTE CRUD ------------------------------------

tabRegistroCliente :: (PostBuild t m, DomBuilder t m) => Dynamic t ClienteJson -> m (Event t AcaoCliente)
tabRegistroCliente pr = do     
    el "tr" $ do
        el "td" (dynText $ fmap (T.pack . show . clienteId) pr)
        el "td" (dynText $ fmap nome pr)
        el "td" (dynText $ fmap contato pr)        
        evt1 <- elAttr "td" ("class" =: "update" <> "colspawn" =: "3") $ (fmap (fmap (const Edit)) (button "Editar"))        
        (btn,_) <- elAttr' "button" ("class"=: "btn btn-primary hidden" <> "id"=:"listar") (text "Consultar")    
        evt2 <- elAttr "td" ("class" =: "get") $ (fmap (fmap (const GetId)) (button "Consultar"))
        evt3 <- elAttr "td" ("class" =: "delete") $ (fmap (fmap (const Del)) (button "Excluir"))        
        evt4 <- elAttr "td" ("class" =: "add") $ (fmap (fmap (const Add)) (button "Adicionar Pet"))
        return (attachPromptlyDynWith (flip ($)) (fmap clienteId pr) (leftmost [evt1, evt2, evt3, evt4]))

reqTabelaCliente' :: ( DomBuilder t m
            , Prerender js t m
            , MonadHold t m
            , MonadFix m
            , PostBuild t m) => Workflow t m T.Text
reqTabelaCliente' = Workflow $ do
    el "div" $ do 
        el "h3" (text "Clientes") 
        el "hr" blank
    (btn,_) <- elAttr' "button" ("class"=: "btn btn-primary hidden" <> "id"=:"listar") (text "Listar Clientes")    
    let click = domEvent Click btn
    (btnAdd,_) <- elAttr' "button" ("class"=: "btn btn-primary") (text "Adicionar novo cliente")        
    let clickAdd = domEvent Click btnAdd    
    prods <- prerender
        (pure never)
        (fmap decodeXhrResponse <$> performRequestAsync (const getListReqClienteCliente <$> click))
    evt <- return (fmap (fromMaybe []) $ switchDyn prods)
    dynP <- foldDyn (++) [] evt    
    tb <- elAttr "table" ("class"=:"table") $ do
        el "thead" $ do
            -- evt <- elAttr "td" ("class" =: "add") $ (fmap (fmap (const Add)) (button " + "))
            el "tr" $ do                
                elAttr "th" ("scope"=:"col") (text "Id")
                elAttr "th" ("scope"=:"col") (text "Nome")
                elAttr "th" ("scope"=:"col") (text "Contato")              
                elAttr "th" ("scope"=:"col") (text "")
                elAttr "th" ("scope"=:"col") (text "")
                elAttr "th" ("scope"=:"col") (text "")
                elAttr "th" ("scope"=:"col") (text "")
        
        el "tbody" $ do
            simpleList dynP tabRegistroCliente
    tb' <- return $ switchDyn $ fmap leftmost tb        
    return ("", escolherPag <$> tb')
    where
        escolherPag (GetId pid) = pagClienteIdFlow pid
        escolherPag (Edit pid) = editarCliente pid
        escolherPag (Del pid) = deleteClienteConfirm pid
        escolherPag (Add pid) = adicionarPet pid


getIdReqCliente :: Int -> XhrRequest ()
getIdReqCliente id = xhrRequest "GET" (getPath (BackendRoute_ClienteBuscar :/ id)) def

pagClienteIdFlow :: ( DomBuilder t m
            , Prerender js t m
            , MonadHold t m
            , MonadFix m
            , PostBuild t m) => Int -> Workflow t m T.Text
pagClienteIdFlow pid = Workflow $ do
    el "div" $ do 
        el "h3" (text "Cliente - Visualizar") 
        el "hr" blank    
    (btnret,_) <- elAttr' "button" ("class"=: "btn btn-danger" <> "onclick"=:"loadList()") (text "Voltar")
    let ret = domEvent Click btnret   
    (btn,_) <- elAttr' "button" ("class"=: "btn btn-primary hidden" <> "id"=:"mostrar") (text "Mostrar")
    let click = domEvent Click btn    
    prod <- prerender
        (pure never)
        (fmap decodeXhrResponse <$> performRequestAsync (const (getIdReqCliente pid) <$> click))
    mdyn <- holdDyn Nothing (switchDyn prod)
    dynP <- return ((fromMaybe (ClienteJson 0 "" "")) <$> mdyn)    
    el "div" $ do
        elAttr "p" ("class" =: "title") (text "Id do cliente:") 
        el "p" (dynText $ fmap (T.pack . show . clienteId) dynP)        
        elAttr "p" ("class" =: "title") (text "Nome do cliente:") 
        el "p" (dynText $ fmap nome dynP)
        elAttr "p" ("class" =: "title") (text "Contato do cliente:") 
        el "p" (dynText $ fmap contato dynP)

    return ("" <> "", reqTabelaCliente' <$ ret)  

reqListaCliente :: ( DomBuilder t m
            , Prerender js t m
            , MonadHold t m
            , MonadFix m
            , PostBuild t m) => m ()
reqListaCliente = do    
    r <- workflow reqTabelaCliente'
    el "div" (dynText r)        

editarCliente :: ( DomBuilder t m
            , Prerender js t m
            , MonadHold t m
            , MonadFix m
            , PostBuild t m) => Int -> Workflow t m T.Text
editarCliente pid = Workflow $ do
    el "div" $ do 
        el "h3" (text "Cliente - Editar") 
        el "hr" blank    
    (btn,_) <- elAttr' "button" ("class"=: "btn btn-primary hidden" <> "id"=:"mostrar") (text "Mostrar")
    (btnSuccess, _) <- elAttr' "button" ("class"=: "btn btn-success" <> "onclick"=:"loadList()") (text "Salvar Alterações")
    let submitBtn = domEvent Click btnSuccess
    let click = domEvent Click btn
    prod :: Dynamic t (Event t (Maybe ClienteJson)) <- prerender
        (pure never)
        (fmap decodeXhrResponse <$> performRequestAsync
           (const (getIdReqCliente pid) <$> click))
    mdyn <- return (switchDyn prod)
    dynE <- return ((fromMaybe (ClienteJson 0 "" "")) <$> mdyn)

    el "div" $ do
    elAttr "p" ("class" =: "title") (text "Nome do cliente:") 
    nome <- inputElement $ 
        def & inputElementConfig_setValue .~ (fmap nome dynE)
    elAttr "p" ("class" =: "title") (text "Contato do cliente:") 
    contato <- inputElement $ 
        def & inputElementConfig_setValue .~ (fmap contato dynE)
    let prod = fmap (\(n,c) -> ClienteJson 0 n c) (zipDyn (_inputElement_value nome) (_inputElement_value contato))
            
    let prodEvt = tag (current prod) submitBtn
    _ :: Dynamic t (Event t (Maybe T.Text)) <- prerender
        (pure never)
        (fmap decodeXhrResponse <$> 
            performRequestAsync (sendRequest (BackendRoute_ClienteEditar :/ pid) 
            <$> prodEvt)) 
    return ("Id do Cliente: " <> (T.pack $ show pid), reqTabelaCliente' <$ submitBtn)  

deleteClienteConfirm  :: ( DomBuilder t m
            , Prerender js t m
            , MonadHold t m
            , MonadFix m
            , PostBuild t m) => Int -> Workflow t m T.Text
deleteClienteConfirm  pid = Workflow $ do
    el "div" $ do 
        el "h3" (text "Cliente - Deletar") 
        el "hr" blank      
    elAttr "p" ("class" =: "title") (text "Deseja mesmo deletar o usuário?")    
    (btnSim,x) <- elAttr' "button" ("class"=: "btn btn-success" <> "onclick"=:"loadList()") (text "Sim, tenho certeza")

    let simEvt = domEvent Click btnSim
    x :: Dynamic t (Event t (Maybe T.Text)) <- prerender
        (pure never)
        (fmap decodeXhrResponse <$> 
            performRequestAsync (sendRequest (BackendRoute_ClienteDelete :/ pid) 
            <$> simEvt))    

    return ("" <> "", reqTabelaCliente' <$ simEvt)
    

adicionarCliente :: ( DomBuilder t m
            , Prerender js t m
            , MonadHold t m
            , MonadFix m
            , PostBuild t m) => Workflow t m T.Text
adicionarCliente = Workflow $ do
    el "h3" (text "Cliente - Adicionar")
    el "hr" (blank)
    elAttr "p" ("class" =: "title") (text "Nome:") 
    nome <- inputElement def
    elAttr "p" ("class" =: "title") (text "Contato:") 
    contato <- inputElement def
    (backBtn,_) <- elAttr' "button" ("class"=:"btn btn-danger" <> "onclick"=:"loadList()") (text "Voltar")
    let back = domEvent Click backBtn
    let object = fmap (\(n,c) -> ClienteJson 0 n c) (zipDyn (_inputElement_value nome) (_inputElement_value contato))
    (submitBtn,_) <- elAttr' "button" ("class"=:"btn btn-primary") (text "Adicionar")
    let click = domEvent Click submitBtn
    let prodEvt = tag (current object) click
    _ :: Dynamic t (Event t (Maybe T.Text)) <- prerender
        (pure never)
        (fmap decodeXhrResponse <$> performRequestAsync (sendRequest (BackendRoute_ClienteJson :/ ()) <$> prodEvt))        
    return ("" <> "", reqTabelaCliente' <$ back)  












-----------------------PET CRUD---------------------------------------------------------
tabRegistroPet :: (PostBuild t m, DomBuilder t m) => Dynamic t GetPetJsonObject -> m (Event t AcaoPet)
tabRegistroPet pr = do     
    el "tr" $ do
        el "td" (dynText $ fmap (T.pack . show . petIdGet) pr)
        el "td" (dynText $ fmap nomePetGet pr)
        el "td" (dynText $ fmap racaPetGet pr)
        el "td" (dynText $ fmap (T.pack . show . tutorIdGet) pr)
        el "td" (dynText $ fmap tutorNomeGet pr)
        el "td" (dynText $ fmap tutorContatoGet pr)
        evt1 <- elAttr "td" ("class" =: "update" <> "colspawn" =: "3") $ (fmap (fmap (const PetEdit)) (button "Editar"))        
        (btn,_) <- elAttr' "button" ("class"=: "btn btn-primary hidden" <> "id"=:"listar") (text "Consultar")    
        evt2 <- elAttr "td" ("class" =: "get") $ (fmap (fmap (const PetGetId)) (button "Consultar"))
        evt3 <- elAttr "td" ("class" =: "delete") $ (fmap (fmap (const PetDel)) (button "Excluir"))        
        -- evt4 <- elAttr "td" ("class" =: "add") $ (fmap (fmap (const PetAdd')) (button " + "))
        return (attachPromptlyDynWith (flip ($)) (fmap petIdGet pr) (leftmost [evt1, evt2, evt3]))

reqTabelaPet' :: ( DomBuilder t m
            , Prerender js t m
            , MonadHold t m
            , MonadFix m
            , PostBuild t m) => Workflow t m T.Text
reqTabelaPet' = Workflow $ do
    el "div" $ do 
        el "h3" (text "Pets") 
        el "hr" blank
    (btn,_) <- elAttr' "button" ("class"=: "btn btn-primary hidden" <> "id"=:"listar") (text "Listar Pets")    
    let click = domEvent Click btn
    prods <- prerender
        (pure never)
        (fmap decodeXhrResponse <$> performRequestAsync (const getListReqPet <$> click))
    evt <- return (fmap (fromMaybe []) $ switchDyn prods)
    dynP <- foldDyn (++) [] evt    
    tb <- elAttr "table" ("class"=:"table") $ do
        el "thead" $ do
            el "tr" $ do                
                elAttr "th" ("scope"=:"col") (text "Id do pet")
                elAttr "th" ("scope"=:"col") (text "Nome do pet")
                elAttr "th" ("scope"=:"col") (text "Tipo do pet")
                elAttr "th" ("scope"=:"col") (text "Id do dono")
                elAttr "th" ("scope"=:"col") (text "Nome do dono")
                elAttr "th" ("scope"=:"col") (text "Contato do dono")
                elAttr "th" ("scope"=:"col") (text "")
                elAttr "th" ("scope"=:"col") (text "")
                elAttr "th" ("scope"=:"col") (text "")
        
        el "tbody" $ do
            simpleList dynP tabRegistroPet
    tb' <- return $ switchDyn $ fmap leftmost tb        
    return ("", escolherPag <$> tb')
    where
        escolherPag (PetGetId pid) = pagPetIdFlow pid
        escolherPag (PetEdit pid) = editarPetFunc pid
        escolherPag (PetDel pid) = deletePetConfirm pid
        -- escolherPag (PetAdd' _) = adicionarPet


getIdReqPet :: Int -> XhrRequest ()
getIdReqPet id = xhrRequest "GET" (getPath (BackendRoute_PetBuscar :/ id)) def

pagPetIdFlow :: ( DomBuilder t m
            , Prerender js t m
            , MonadHold t m
            , MonadFix m
            , PostBuild t m) => Int -> Workflow t m T.Text
pagPetIdFlow pid = Workflow $ do
    el "div" $ do 
        el "h3" (text "Pet - Visualizar") 
        el "hr" blank    
    (btnret,_) <- elAttr' "button" ("class"=: "btn btn-danger" <> "onclick"=:"loadList()") (text "Voltar")
    let ret = domEvent Click btnret   
    (btn,_) <- elAttr' "button" ("class"=: "btn btn-primary hidden" <> "id"=:"mostrar") (text "Mostrar")
    let click = domEvent Click btn    
    prod <- prerender
        (pure never)
        (fmap decodeXhrResponse <$> performRequestAsync (const (getIdReqPet pid) <$> click))
    mdyn <- holdDyn Nothing (switchDyn prod)
    dynP <- return ((fromMaybe (PetJsonObject 0 0 "" "")) <$> mdyn)    
    el "div" $ do
        elAttr "p" ("class" =: "title") (text "Id do pet:") 
        el "p" (dynText $ fmap (T.pack . show . petId) dynP)        
        elAttr "p" ("class" =: "title") (text "Id do cliente:") 
        el "p" (dynText $ fmap (T.pack . show . tutorId) dynP)        
        elAttr "p" ("class" =: "title") (text "Nome do pet:") 
        el "p" (dynText $ fmap nomePet dynP)
        elAttr "p" ("class" =: "title") (text "Tipo do pet:") 
        el "p" (dynText $ fmap racaPet dynP)

    return ("" <> "", reqTabelaPet' <$ ret)  

reqListaPet :: ( DomBuilder t m
            , Prerender js t m
            , MonadHold t m
            , MonadFix m
            , PostBuild t m) => m ()
reqListaPet = do    
    r <- workflow reqTabelaPet'
    el "div" (dynText r)        

editarPetFunc :: ( DomBuilder t m
            , Prerender js t m
            , MonadHold t m
            , MonadFix m
            , PostBuild t m) => Int -> Workflow t m T.Text
editarPetFunc pid = Workflow $ do
    el "div" $ do 
        el "h3" (text "Pet - Editar") 
        el "hr" blank    
    (btn,_) <- elAttr' "button" ("class"=: "btn btn-primary hidden" <> "id"=:"mostrar") (text "Mostrar")
    (btnSuccess, _) <- elAttr' "button" ("class"=: "btn btn-success" <> "onclick"=:"loadList()") (text "Salvar Alterações")
    let submitBtn = domEvent Click btnSuccess
    let click = domEvent Click btn
    prod :: Dynamic t (Event t (Maybe PetJsonObject)) <- prerender
        (pure never)
        (fmap decodeXhrResponse <$> performRequestAsync
           (const (getIdReqPet pid) <$> click))
    mdyn <- return (switchDyn prod)
    dynE <- return ((fromMaybe (PetJsonObject 0 0 "" "")) <$> mdyn)

    el "div" $ do
    -- elAttr "p" ("class" =: "title") (text "Id do tutor:") 
    -- id <- 0
    elAttr "p" ("class" =: "title") (text "Nome do pet:") 
    nome <- inputElement $ 
        def & inputElementConfig_setValue .~ (fmap nomePet dynE)
    elAttr "p" ("class" =: "title") (text "Tipo do pet:") 
    tipo <- inputElement $ 
        def & inputElementConfig_setValue .~ (fmap racaPet dynE)

    let prod = fmap (\(n,r) -> PetJsonObject 0 0 n r) (zipDyn (_inputElement_value nome) (_inputElement_value tipo))
            
    let prodEvt = tag (current prod) submitBtn
    _ :: Dynamic t (Event t (Maybe T.Text)) <- prerender
        (pure never)
        (fmap decodeXhrResponse <$> 
            performRequestAsync (sendRequest (BackendRoute_PetEditar :/ pid) 
            <$> prodEvt)) 
    return ("Id do pet: " <> (T.pack $ show pid), reqTabelaPet' <$ submitBtn)  

deletePetConfirm  :: ( DomBuilder t m
            , Prerender js t m
            , MonadHold t m
            , MonadFix m
            , PostBuild t m) => Int -> Workflow t m T.Text
deletePetConfirm  pid = Workflow $ do
    el "div" $ do 
        el "h3" (text "Pet - Deletar") 
        el "hr" blank      
    elAttr "p" ("class" =: "title") (text "Deseja mesmo deletar o pet?")    
    (btnSim,x) <- elAttr' "button" ("class"=: "btn btn-success" <> "onclick"=:"loadList()") (text "Sim, tenho certeza")

    let simEvt = domEvent Click btnSim
    x :: Dynamic t (Event t (Maybe T.Text)) <- prerender
        (pure never)
        (fmap decodeXhrResponse <$> 
            performRequestAsync (sendRequest (BackendRoute_PetDelete :/ pid) 
            <$> simEvt))    

    return ("" <> "", reqTabelaPet' <$ simEvt)

adicionarPet :: ( DomBuilder t m
            , Prerender js t m
            , MonadHold t m
            , MonadFix m
            , PostBuild t m) => Int -> Workflow t m T.Text
adicionarPet pid = Workflow $ do
    el "h3" (text "Pet - Adicionar")
    el "hr" (blank)
    elAttr "p" ("class" =: "title") (text "Nome do pet:") 
    nome <- inputElement def
    elAttr "p" ("class" =: "title") (text "Tipo do pet:") 
    tipo <- inputElement def
    (backBtn,_) <- elAttr' "button" ("class"=:"btn btn-danger" <> "onclick"=:"loadList()") (text "Voltar")    
    let back = domEvent Click backBtn
    --fmap (\(n,r) -> PetJsonObject 0 0 n r) (zipDyn (_inputElement_value nome) (_inputElement_value tipo))
    let object = fmap (\(n,r) -> PetJsonObject 0 pid n r) (zipDyn (_inputElement_value nome) (_inputElement_value tipo))
    (submitBtn,_) <- elAttr' "button" ("class"=:"btn btn-primary") (text "Adicionar")
    let click = domEvent Click submitBtn
    let prodEvt = tag (current object) click
    _ :: Dynamic t (Event t (Maybe T.Text)) <- prerender
        (pure never)
        (fmap decodeXhrResponse <$> performRequestAsync (sendRequest (BackendRoute_PetJson :/ ()) <$> prodEvt))
    return ("" <> "", reqTabelaPet' <$ back)  













-------------------------- AGENDA CRUD ------------------------------------
tabRegistroAgenda :: (PostBuild t m, DomBuilder t m) => Dynamic t GetAgendaJson -> m (Event t AcaoAgenda)
tabRegistroAgenda pr = do     
    el "tr" $ do
        el "td" (dynText $ fmap (T.pack . show . donoIdGet) pr)
        el "td" (dynText $ fmap donoNomeGet pr)
        el "td" (dynText $ fmap donoContatoGet pr)
        el "td" (dynText $ fmap dataAgendaGet pr)
        el "td" (dynText $ fmap (T.pack . show . precoGet) pr)
        el "td" (dynText $ fmap nomeServicoGet pr)            
        evt1 <- elAttr "td" ("class" =: "update" <> "colspawn" =: "3") $ (fmap (fmap (const AgendaEdit)) (button "Editar"))        
        evt2 <- elAttr "td" ("class" =: "get") $ (fmap (fmap (const AgendaGetId)) (button "Consultar"))
        evt3 <- elAttr "td" ("class" =: "delete") $ (fmap (fmap (const AgendaDel)) (button "Excluir"))        
        return (attachPromptlyDynWith (flip ($)) (fmap agendaIdGet pr) (leftmost [evt1, evt2, evt3]))

reqTabelaAgenda' :: ( DomBuilder t m
            , Prerender js t m
            , MonadHold t m
            , MonadFix m
            , PostBuild t m) => Workflow t m T.Text
reqTabelaAgenda' = Workflow $ do
    el "div" $ do 
        el "h3" (text "Agendamentos") 
        el "hr" blank
    (btn,_) <- elAttr' "button" ("class"=: "btn btn-primary hidden" <> "id"=:"listar") (text "Listar Agenda")    
    let click = domEvent Click btn
    (btnAdd,_) <- elAttr' "button" ("class"=: "btn btn-primary") (text "Adicionar novo agendamento")        
    let clickAdd = domEvent Click btnAdd    
    prods <- prerender
        (pure never)
        (fmap decodeXhrResponse <$> performRequestAsync (const getListReqAgenda <$> click))
    evt <- return (fmap (fromMaybe []) $ switchDyn prods)
    dynP <- foldDyn (++) [] evt    
    tb <- elAttr "table" ("class"=:"table") $ do
        el "thead" $ do            
            el "tr" $ do                
                elAttr "th" ("scope"=:"col") (text "Id do dono")
                elAttr "th" ("scope"=:"col") (text "Nome do dono")
                elAttr "th" ("scope"=:"col") (text "Contato do dono")
                elAttr "th" ("scope"=:"col") (text "Data")     
                elAttr "th" ("scope"=:"col") (text "Preço")                   
                elAttr "th" ("scope"=:"col") (text "Serviço")                
                elAttr "th" ("scope"=:"col") (text "")
                elAttr "th" ("scope"=:"col") (text "")
                elAttr "th" ("scope"=:"col") (text "")
        
        el "tbody" $ do
            simpleList dynP tabRegistroAgenda
    tb' <- return $ switchDyn $ fmap leftmost tb        
    return ("", escolherPag <$> tb')
    where
        escolherPag (AgendaGetId pid) = pagAgendaIdFlow pid
        escolherPag (AgendaEdit pid) = editarAgenda pid
        escolherPag (AgendaDel pid) = deleteAgendaConfirm pid
        -- escolherPag (Add pid) = adicionarPet pid


getIdReqAgenda :: Int -> XhrRequest ()
getIdReqAgenda id = xhrRequest "GET" (getPath (BackendRoute_AgendaBuscar :/ id)) def

getIdReqAgendaId :: Int -> XhrRequest ()
getIdReqAgendaId id = xhrRequest "GET" (getPath (BackendRoute_AgendaBuscarId :/ id)) def

pagAgendaIdFlow :: ( DomBuilder t m
            , Prerender js t m
            , MonadHold t m
            , MonadFix m
            , PostBuild t m) => Int -> Workflow t m T.Text
pagAgendaIdFlow pid = Workflow $ do
    el "div" $ do 
        el "h3" (text "Agenda - Visualizar") 
        el "hr" blank    
    (btnret,_) <- elAttr' "button" ("class"=: "btn btn-danger" <> "onclick"=:"loadList()") (text "Voltar")
    let ret = domEvent Click btnret   
    (btn,_) <- elAttr' "button" ("class"=: "btn btn-primary hidden" <> "id"=:"mostrar") (text "Mostrar")
    let click = domEvent Click btn    
    prod <- prerender
        (pure never)
        (fmap decodeXhrResponse <$> performRequestAsync (const (getIdReqAgendaId pid) <$> click))
    mdyn <- holdDyn Nothing (switchDyn prod)
    dynP <- return ((fromMaybe (GetAgendaJson 0 0 "" "" "" 0 "")) <$> mdyn)    
    el "div" $ do
        elAttr "p" ("class" =: "title") (text "Data:") 
        el "p" (dynText $ fmap dataAgendaGet dynP)
        elAttr "p" ("class" =: "title") (text "Nome do serviço:") 
        el "p" (dynText $ fmap nomeServicoGet dynP)
        elAttr "p" ("class" =: "title") (text "Preço:") 
        el "p" (dynText $ fmap (T.pack . show . precoGet) dynP)
        elAttr "p" ("class" =: "title") (text "Nome do dono:") 
        el "p" (dynText $ fmap donoContatoGet dynP)     
        elAttr "p" ("class" =: "title") (text "Contato:") 
        el "p" (dynText $ fmap donoContatoGet dynP)

    return ("" <> "", reqTabelaAgenda' <$ ret)  

reqListaAgenda :: ( DomBuilder t m
            , Prerender js t m
            , MonadHold t m
            , MonadFix m
            , PostBuild t m) => m ()
reqListaAgenda = do    
    r <- workflow reqTabelaAgenda'
    el "div" (dynText r)        

editarAgenda :: ( DomBuilder t m
            , Prerender js t m
            , MonadHold t m
            , MonadFix m
            , PostBuild t m) => Int -> Workflow t m T.Text
editarAgenda pid = Workflow $ do
    el "div" $ do 
        el "h3" (text "Agenda - Editar") 
        el "hr" blank    
    (btn,_) <- elAttr' "button" ("class"=: "btn btn-primary hidden" <> "id"=:"mostrar") (text "Mostrar")
    (btnSuccess, _) <- elAttr' "button" ("class"=: "btn btn-success" <> "onclick"=:"loadList()") (text "Salvar Alterações")
    let submitBtn = domEvent Click btnSuccess
    let click = domEvent Click btn
    prod :: Dynamic t (Event t (Maybe GetAgendaJson)) <- prerender
        (pure never)
        (fmap decodeXhrResponse <$> performRequestAsync
           (const (getIdReqAgenda pid) <$> click))
    mdyn <- return (switchDyn prod)
    dynE <- return ((fromMaybe (GetAgendaJson 0 0 "" "" "" 0 "")) <$> mdyn)

    el "div" $ do
    elAttr "p" ("class" =: "title") (text "Data:") 
    date <- inputElement $ 
        def & inputElementConfig_setValue .~ (fmap dataAgendaGet dynE)
    elAttr "p" ("class" =: "title") (text "Nome do serviço:") 
    nomeServico <- inputElement $ 
        def & inputElementConfig_setValue .~ (fmap nomeServicoGet dynE)
    elAttr "p" ("class" =: "title") (text "Preço:") 
    preco <- numberInputDyn (fmap precoGet dynE)
    let prod = fmap (\((p,d),n) -> AgendaJson 0 0 d p n) (zipDyn (zipDyn preco (_inputElement_value date))  (_inputElement_value nomeServico))
    --let prod = fmap (\(n,c) -> ClienteJson 0 n c) (zipDyn (_inputElement_value nome) (_inputElement_value contato))            
    let prodEvt = tag (current prod) submitBtn
    _ :: Dynamic t (Event t (Maybe T.Text)) <- prerender
        (pure never)
        (fmap decodeXhrResponse <$> 
            performRequestAsync (sendRequest (BackendRoute_AgendaEditar :/ pid) 
            <$> prodEvt)) 
    return ("Id da Agenda: " <> (T.pack $ show pid), reqTabelaAgenda' <$ submitBtn)  

deleteAgendaConfirm  :: ( DomBuilder t m
            , Prerender js t m
            , MonadHold t m
            , MonadFix m
            , PostBuild t m) => Int -> Workflow t m T.Text
deleteAgendaConfirm  pid = Workflow $ do
    el "div" $ do 
        el "h3" (text "Agenda - Deletar") 
        el "hr" blank      
    elAttr "p" ("class" =: "title") (text "Deseja mesmo deletar o agendamento?")    
    (btnSim,x) <- elAttr' "button" ("class"=: "btn btn-success" <> "onclick"=:"loadList()") (text "Sim, tenho certeza")

    let simEvt = domEvent Click btnSim
    x :: Dynamic t (Event t (Maybe T.Text)) <- prerender
        (pure never)
        (fmap decodeXhrResponse <$> 
            performRequestAsync (sendRequest (BackendRoute_AgendaDelete :/ pid) 
            <$> simEvt))    
    return ("" <> "", reqTabelaAgenda' <$ simEvt)
    

adicionarAgenda :: ( DomBuilder t m
            , Prerender js t m
            , MonadHold t m
            , MonadFix m
            , PostBuild t m) => Workflow t m T.Text
adicionarAgenda = Workflow $ do
    el "h3" (text "Agenda - Adicionar")
    el "hr" (blank)
    elAttr "p" ("class" =: "title") (text "Nome:") 
    nome <- inputElement def
    elAttr "p" ("class" =: "title") (text "Contato:") 
    contato <- inputElement def
    (backBtn,_) <- elAttr' "button" ("class"=:"btn btn-danger" <> "onclick"=:"loadList()") (text "Voltar")
    let back = domEvent Click backBtn
    let object = fmap (\(n,c) -> ClienteJson 0 n c) (zipDyn (_inputElement_value nome) (_inputElement_value contato))
    (submitBtn,_) <- elAttr' "button" ("class"=:"btn btn-primary") (text "Adicionar")
    let click = domEvent Click submitBtn
    let prodEvt = tag (current object) click
    _ :: Dynamic t (Event t (Maybe T.Text)) <- prerender
        (pure never)
        (fmap decodeXhrResponse <$> performRequestAsync (sendRequest (BackendRoute_ClienteJson :/ ()) <$> prodEvt))        
    return ("" <> "", reqTabelaCliente' <$ back)  















-- tabAgenda :: ( DomBuilder t m
--             , Prerender js t m
--             , MonadHold t m
--             , MonadFix m
--             , PostBuild t m) => GetAgendaJson -> m ()
-- tabAgenda pr = do 
--     el "tr" $ do
--         el "td" (text $ T.pack $ show $ donoIdGet pr)
--         el "td" (text $ donoNomeGet pr)
--         el "td" (text $ donoContatoGet pr)
--         el "td" (text $ dataAgendaGet pr)
--         el "td" (text $ T.pack $ show $ precoGet pr)
--         el "td" (text $ nomeServicoGet pr)        
--         --el "td" (elAttr "button" ("class"=: "btn btn-danger") (text "Excluir"))
--         (submitBtn,_) <- el "td" $ elAttr' "button" ("class"=: "btn btn-danger") (text "Excluir")                
--         el "td" (elAttr "button" ("class"=: "btn btn-primary") (text "Editar"))        
--         el "td" (elAttr "button" ("class"=: "btn btn-secondary") (text "Consultar"))        
--         let click = domEvent Click submitBtn
--         _ :: Dynamic t (Event t (Maybe T.Text)) <- prerender
--             (pure never)
--             -- const getListReqClienteCliente
--             (fmap decodeXhrResponse <$> performRequestAsync (const (deleteReqAgenda (agendaIdGet pr)) <$> click))
--         return()

-- tabPet :: ( DomBuilder t m
--             , Prerender js t m
--             , MonadHold t m
--             , MonadFix m
--             , PostBuild t m) => GetPetJsonObject -> m ()
-- tabPet pr = do 
--     el "tr" $ do
--         el "td" (text $ T.pack $ show $ petIdGet pr)
--         el "td" (text $ nomePetGet pr)
--         el "td" (text $ racaPetGet pr)
--         el "td" (text $ T.pack $ show $ tutorIdGet pr)
--         el "td" (text $ tutorNomeGet pr)
--         el "td" (text $ tutorContatoGet pr)
--         (submitBtn,_) <- el "td" $ elAttr' "button" ("class"=: "btn btn-danger") (text "Excluir")                
--         el "td" (elAttr "button" ("class"=: "btn btn-primary") (text "Editar"))        
--         el "td" (elAttr "button" ("class"=: "btn btn-secondary") (text "Consultar"))        
--         let click = domEvent Click submitBtn
--         _ :: Dynamic t (Event t (Maybe T.Text)) <- prerender
--             (pure never)
--             (fmap decodeXhrResponse <$> performRequestAsync (const (deleteReqPet (petIdGet pr)) <$> click))
--         return()

-- reqListaAgenda :: ( DomBuilder t m
--             , Prerender js t m
--             , MonadHold t m
--             , MonadFix m
--             , PostBuild t m) => m ()
-- reqListaAgenda = do
--     el "h3" (text "Agendamentos")
--     el "hr" $ blank
--     (btn, _) <- elAttr' "button" ("class"=: "btn btn-primary") (text "Listar agenda")
--     let click = domEvent Click btn
--     (btnAdd, _) <- elAttr' "button" ("class"=: "btn btn-primary") (text "Adicionar")
--     prods :: Dynamic t (Event t (Maybe [GetAgendaJson])) <- prerender
--         (pure never)
--         (fmap decodeXhrResponse <$> performRequestAsync (const getListReqAgenda <$> click))
--     dynP <- foldDyn (\ps d -> case ps of
--                             Nothing -> []
--                             Just p -> d++p) [] (switchDyn prods)
--     elAttr "table" ("class"=:"table") $ do
--         el "thead" $ do
--             el "tr" $ do
--                 elAttr "th" ("scope"=:"col") (text "Id do dono")
--                 elAttr "th" ("scope"=:"col") (text "Nome do dono")
--                 elAttr "th" ("scope"=:"col") (text "Contato do dono")
--                 elAttr "th" ("scope"=:"col") (text "Data")     
--                 elAttr "th" ("scope"=:"col") (text "Preço")                   
--                 elAttr "th" ("scope"=:"col") (text "Serviço")                
--                 elAttr "th" ("scope"=:"col") (text "")
--                 elAttr "th" ("scope"=:"col") (text "")
--                 elAttr "th" ("scope"=:"col") (text "")                   

--         el "tbody" $ do
--             dyn_ (fmap sequence (ffor dynP (fmap tabAgenda)))

------------------------


------FRONTEND-----------
data Pagina = HomePage | Pet | PetAdd | PagPetEdit Int | PagPetId Int | Agenda | PagAgendaEdit Int | PagAgendaId Int | Sobre | Cliente | ClienteAdd | AgendaAdd
data AcaoCliente = GetId Int | Edit Int | Del Int | Add Int
data AcaoPet = PetGetId Int | PetEdit Int | PetDel Int
--AgendaAdd Int
data AcaoAgenda = AgendaGetId Int | AgendaEdit Int | AgendaDel Int

clickLi :: DomBuilder t m => Pagina -> T.Text -> T.Text -> m (Event t Pagina)
clickLi p t d = do
    (ev, _) <- el' "li" (elAttr "a" ("onclick"=:d <> "class" =: "nav-link active" <> "aria-current" =: "page" <> "href" =: "#") (text t))
    return ((\_ -> p) <$> domEvent Click ev)
    
menuLi :: (DomBuilder t m, MonadHold t m) => m (Dynamic t Pagina)
menuLi = do
    evs <- elAttr "div" ("class" =: "container-fluid") $
            elAttr "ul" ("class" =: "navbar-nav") $ do
        p1 <- clickLi HomePage "Home" ""
        p3 <- clickLi Agenda "Agendamentos" "loadList()"
        p4 <- clickLi Cliente "Clientes" "loadList()"
        p2 <- clickLi Pet "Pets" "loadList()"                
        --p5 <- clickLi Sobre "Sobre"
        --p6 <- clickLi PetAdd "Pet - Add" ""
        p7 <- clickLi ClienteAdd "Cliente - Add" ""
        p8 <- clickLi AgendaAdd "Agenda - Add" ""        
        return (leftmost [p1,p3,p2,p4, p7, p8])
    holdDyn HomePage evs    
    
currPag :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m, Prerender js t m) => Pagina -> m ()
currPag p = 
    case p of
         HomePage -> homePage
         Pet -> reqListaPet--petPage
         Agenda -> reqListaAgenda--agendaPage
         Cliente -> reqListaCliente --reqListaCliente --clientePage
         Sobre -> sobrePage
         --PetAdd -> pagReqPet'
         ClienteAdd -> pagReqCliente
         AgendaAdd -> pagReqAgenda
         
mainPag :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m, Prerender js t m) => m ()
mainPag = do
    pag <- elAttr "nav" ("class" =: "navbar navbar-expand-lg navbar-light bg-light") $ menuLi
    dyn_ $ currPag <$> pag  

numberInput :: DomBuilder t m => m (Dynamic t Double)
numberInput = do
      n <- inputElement $ def
        & inputElementConfig_initialValue .~ "0"
        & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("type" =: "number")
      return $ fmap (fromMaybe 0 . readMaybe . T.unpack) $ _inputElement_value n

numberInputSecond :: (DomBuilder t m, Num a, Read a) => m (Dynamic t a)
numberInputSecond = do
      n <- inputElement $ def
        & inputElementConfig_initialValue .~ "0"
        & inputElementConfig_elementConfig 
        . elementConfig_initialAttributes .~ ("type" =: "number")
      return $ fmap (fromMaybe 0 . readMaybe . T.unpack) 
                 (_inputElement_value n)      

numberInputDyn :: (DomBuilder t m, Num a, Read a, Show a) =>
               Event t a -> m (Dynamic t a)
numberInputDyn p = do
      val <- return (fmap (T.pack . show) p)
      n <- inputElement $ def
        & inputElementConfig_setValue .~ val
      return $ fmap (fromMaybe 0 . readMaybe . T.unpack) 
                 (_inputElement_value n)

homePage :: (DomBuilder t m, PostBuild t m) => m ()
homePage = do
  el "h3" (text "Home Page")
  el "hr" $ blank
  el "div" $ do
    elAttr "p" ("class" =: "title") (text "Alunos:")
    elAttr "p" ("class" =: "") (text "Bruno Morais")
    elAttr "p" ("class" =: "") (text "Verônica Marques")
    elAttr "p" ("class" =: "title") (text "Projeto:")
    elAttr "p" ("class" =: "") (text "O projeto tem como objetivo simular o CRUD de Agenda e Pet, com base em um usuário que deseja organizar os horários de passeio e consulta de seus pets")    

petPage :: (DomBuilder t m, PostBuild t m) => m ()
petPage = do
  el "h3" (text "Pet")
  el "hr" $ blank
  el "div" $ do
    el "p" (text "Está na página de pet")              

agendaPage :: (DomBuilder t m, PostBuild t m) => m ()
agendaPage = do
  el "h3" (text "Agenda")
  el "hr" $ blank
  el "div" $ do
    el "p" (text "Está na página de agenda")

clientePage :: (DomBuilder t m, PostBuild t m) => m ()
clientePage = do
  el "h3" (text "Clientes")
  el "hr" $ blank
  el "div" $ do
    el "p" (text "Está na página de clientes")

sobrePage :: (DomBuilder t m, PostBuild t m) => m ()
sobrePage = do
  el "h3" (text "Sobre")
  el "hr" $ blank
  el "div" $ do
    elAttr "p" ("class" =: "title") (text "Alunos:")
    elAttr "p" ("class" =: "") (text "Bruno Morais")
    elAttr "p" ("class" =: "") (text "Verônica Marques")
    elAttr "p" ("class" =: "title") (text "Projeto:")
    elAttr "p" ("class" =: "") (text "O projeto tem como objetivo simular o CRUD de Agenda e Pet, com base em um usuário que deseja organizar os horários de passeio e consulta de seus pets")    



-----------------------
frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do      
      el "title" $ text "Happy Pet"            
      elAttr "link" ("href" =: static @"bootstrap.min.css" <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
      elAttr "link" ("href" =: static @"main.css" <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
      elAttr "meta" ("charset" =: "utf-8") blank
      elAttr "meta" ("name" =: "viewport" <> "content" =: "width=device-width, initial-scale=1") blank            
      elAttr "script" ("type"=:"text/javascript") (text "function mostrar(){setTimeout(function(){document.getElementById(\"mostrar\").click();}, 400);};function loadList(){setTimeout(function(){document.getElementById(\"listar\").click();}, 400);setTimeout(function(){document.querySelectorAll(\"td button\").forEach(x => {x.onclick = mostrar;});}, 1000);};")
    
  , _frontend_body = do
      mainPag      
      return ()
  }
