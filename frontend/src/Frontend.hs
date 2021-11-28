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

-- getListReq :: XhrRequest ()
-- getListReq = xhrRequest "GET" (getPath (BackendRoute_Listar :/ ())) def

sendRequest :: ToJSON a => R BackendRoute -> a -> XhrRequest T.Text
sendRequest r dados = postJson (getPath r) dados

getPathPet :: T.Text
getPathPet = renderBackendRoute checFullREnc $ BackendRoute_PetRoute :/ ()

nomeRequest :: T.Text -> XhrRequest T.Text
nomeRequest s = postJson getPathPet (PetJson s)

-- pagReqPet :: ( DomBuilder t m
--           , Prerender js t m
--           ) => m (Event t T.Text)
-- pagReqPet = do
--     el "h3" (text "Pet - Adicionar")
--     el "hr" $ blank
--     elAttr "p" ("class" =: "title") (text "Nome do pet:") 
--     inpnome <- inputElement def
--     (submitBtn,_) <- elAttr' "button" ("class"=:"btn btn-primary") (text "Inserir")
--     let click = domEvent Click submitBtn
--     let nm = tag (current $ _inputElement_value inpnome) click
--     st <- prerender
--         (pure never)
--         (fmap decodeXhrResponse <$> performRequestAsync (nomeRequest <$> nm))
--     return (fromMaybe "" <$> switchDyn st) 

pagReqPet' :: ( DomBuilder t m
           , Prerender js t m
           ) => m ()
pagReqPet' = do
    el "h3" (text "Pet - Adicionar")
    el "hr" (blank)
    elAttr "p" ("class" =: "title") (text "Id do cliente:") 
    id <- numberInputSecond
    elAttr "p" ("class" =: "title") (text "Nome do pet:") 
    nome <- inputElement def
    elAttr "p" ("class" =: "title") (text "Tipo do pet:") 
    tipo <- inputElement def
    let object = fmap (\((i,n),r) -> PetJsonObject 0 i n r) (zipDyn (zipDyn id  (_inputElement_value nome)) (_inputElement_value tipo))
    (submitBtn,_) <- elAttr' "button" ("class"=:"btn btn-primary") (text "Adicionar")
    let click = domEvent Click submitBtn
    let prodEvt = tag (current object) click
    _ :: Dynamic t (Event t (Maybe T.Text)) <- prerender
        (pure never)
        (fmap decodeXhrResponse <$> performRequestAsync (sendRequest (BackendRoute_PetJson :/ ()) <$> prodEvt))
    return ()

--let object = fmap (\((c,d),(p, n)) -> AgendaJson 0 c d p n) (zipDyn (zipDyn id  (_inputElement_value date)) (zipDyn preco  (_inputElement_value nomeServico)))

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
    let object = fmap (\((c,d),(p, n)) -> AgendaJson 0 c d p n) (zipDyn (zipDyn id  (_inputElement_value date)) (zipDyn preco  (_inputElement_value nomeServico)))
    (submitBtn,_) <- elAttr' "button" ("class"=:"btn btn-primary") (text "Adicionar")
    let click = domEvent Click submitBtn
    let prodEvt = tag (current object) click
    _ :: Dynamic t (Event t (Maybe T.Text)) <- prerender
        (pure never)
        (fmap decodeXhrResponse <$> performRequestAsync (sendRequest (BackendRoute_AgendaJson :/ ()) <$> prodEvt))
    return () 
    
-- paginaInserePet :: ( DomBuilder t m
--        , PostBuild t m
--        , MonadHold t m
--        , Prerender js t m
--        ) => m ()
-- paginaInserePet = do
--     st <- pagReqPet
--     tx <- holdDyn "" st
--     el "div" (dynText tx)

-- paginaInsereCliente :: ( DomBuilder t m
--        , PostBuild t m
--        , MonadHold t m
--        , Prerender js t m
--        ) => m ()
-- paginaInsereCliente = do
--     st <- pagReqCliente
--     tx <- holdDyn "" st
--     el "div" (dynText tx)        

------------------------


------FRONTEND-----------
data Pagina = HomePage | Pet | PetAdd | Agenda | Sobre | Cliente | ClienteAdd | AgendaAdd

clickLi :: DomBuilder t m => Pagina -> T.Text -> m (Event t Pagina)
clickLi p t = do
    (ev, _) <- el' "li" (elAttr "a" ("class" =: "nav-link active" <> "aria-current" =: "page" <> "href" =: "#") (text t))
    return ((\_ -> p) <$> domEvent Click ev)
    
menuLi :: (DomBuilder t m, MonadHold t m) => m (Dynamic t Pagina)
menuLi = do
    evs <- elAttr "div" ("class" =: "container-fluid") $
            elAttr "ul" ("class" =: "navbar-nav") $ do
        p1 <- clickLi HomePage "Home"
        p2 <- clickLi Pet "Pets"
        p3 <- clickLi Agenda "Agendamentos"
        p4 <- clickLi Cliente "Clientes"
        p5 <- clickLi Sobre "Sobre"
        p6 <- clickLi PetAdd "Pet - Add"        
        p7 <- clickLi ClienteAdd "Cliente - Add"
        p8 <- clickLi AgendaAdd "Agenda - Add"        
        return (leftmost [p1,p2,p3,p4, p5, p6, p7, p8])
    holdDyn HomePage evs    
    
currPag :: (DomBuilder t m, MonadHold t m, PostBuild t m, Prerender js0 t m) => Pagina -> m ()
currPag p = 
    case p of
         HomePage -> homePage
         Pet -> petPage--petPage
         Agenda -> agendaPage
         Cliente -> clientePage
         Sobre -> sobrePage
         PetAdd -> pagReqPet'
         ClienteAdd -> pagReqCliente
         AgendaAdd -> pagReqAgenda
         
mainPag :: (DomBuilder t m, MonadHold t m, PostBuild t m, Prerender js0 t m) => m ()
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

caixaSoma :: (DomBuilder t m, PostBuild t m) => m ()
caixaSoma = do
    el "p" $ text "Primeiro Numero:"
    n1 <- numberInput -- m (Dynamic t Double)    
    el "br" $ blank
    el "p" $ text "Segundo número:"    
    n2 <- numberInput -- m (Dynamic t Double)
    text " "
    dynText (fmap (T.pack . show) (zipDynWith (+) n1 n2))

homePage :: (DomBuilder t m, PostBuild t m) => m ()
homePage = do
  el "h3" (text "Home Page")
  el "hr" $ blank
  el "div" $ do
    el "p" (text "Está na home")

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

   
caixas :: (DomBuilder t m, PostBuild t m) => m ()
caixas = do
    el "p" $ text "Nome:"
    t1 <- inputElement def -- m (Dynamic Text)
    el "br" $ blank
    el "p" $ text "Sobrenome:"
    t2 <- inputElement def -- m (Dynamic Text)
    text " "
    dynText (zipDynWith (<>) (_inputElement_value t1) (_inputElement_value t2))

revText :: T.Text -> T.Text
revText t = T.pack (reverse (T.unpack t))
   
buttonClick :: (DomBuilder t m, PostBuild t m, MonadHold t m) => m (Event t T.Text)
buttonClick = do
    t <- inputElement def
    (e,_) <- el' "button" (text "OK")
    return $ attachPromptlyDynWith const 
                                   (fmap revText (_inputElement_value t)) 
                                   (domEvent Click e)            

bttnEvt :: (DomBuilder t m, PostBuild t m, MonadHold t m) => m ()
bttnEvt = do
    evt <- buttonClick
    hl <-  holdDyn "" evt -- Event -> Dynamic 
    el "div" (dynText hl)
    

sumButton :: (DomBuilder t m, PostBuild t m, MonadHold t m) 
          => m (Event t Double)
sumButton = do
    n1 <- numberInput
    text " "
    n2 <- numberInput
    text " "
    (e,_) <- el' "button" (text "OK")
    let dynDouble = zipDynWith (+) n1  n2
    return $ attachPromptlyDynWith const    
                                   dynDouble 
                                   (domEvent Click e)

sumEvt :: (DomBuilder t m, PostBuild t m, MonadHold t m) => m ()
sumEvt = do
    evt <- sumButton
    s <- holdDyn 0 evt 
    el "div" (dynText $ fmap (T.pack . show) s) 


-----------------------
frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do      
      el "title" $ text "Happy Pet"            
      elAttr "link" ("href" =: static @"bootstrap.min.css" <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
      elAttr "link" ("href" =: static @"main.css" <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
      elAttr "meta" ("charset" =: "utf-8") blank
      elAttr "meta" ("name" =: "viewport" <> "content" =: "width=device-width, initial-scale=1") blank      

      -- <!-- Bootstrap CSS -->
      -- elAttr "link" ("href" =: "https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/css/bootstrap.min.css" <> "integrity" =: "sha384-1BmE4kWBq78iYhFldvKuhfTAU6auU8tT94WrHftjDbrCEXSU1oBoqyl2QvZ6jIW3" <> "crossorigin" =: "anonymous") blank
  , _frontend_body = do
      mainPag
      -- caixas
      -- el "br" $ blank
      -- el "br" $ blank
      -- caixaSoma
      -- el "br" $ blank
      -- el "br" $ blank
      -- bttnEvt
      -- el "br" $ blank
      -- el "br" $ blank
      -- sumEvt
      return ()
  }
