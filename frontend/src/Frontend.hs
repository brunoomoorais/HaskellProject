{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Frontend where

import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Language.Javascript.JSaddle (eval, liftJSM)
import Data.Maybe
import Obelisk.Frontend
import Obelisk.Configs
import Obelisk.Route
import Obelisk.Generated.Static
import Text.Read (readMaybe)
import Reflex.Dom.Core

import Common.Api
import Common.Route


-- This runs in a monad that can be run on the client or the server.
-- To run code in a pure client or pure server context, use one of the
-- `prerender` functions.

------BACKEND-----------

getPath :: T.Text
getPath = renderBackendRoute checFullREnc $ BackendRoute_PetRoute :/ ()

nomeRequest :: T.Text -> XhrRequest T.Text
nomeRequest s = postJson getPath (PetJson s)

pagReq :: ( DomBuilder t m
          , Prerender js t m
          ) => m (Event t T.Text)
pagReq = do   
    elAttr "p" ("class" =: "title") (text "Nome do pet:") 
    inpnome <- inputElement def
    (submitBtn,_) <- el' "button" (text "Inserir")
    let click = domEvent Click submitBtn
    let nm = tag (current $ _inputElement_value inpnome) click
    st <- prerender
        (pure never)
        (fmap decodeXhrResponse <$> performRequestAsync (nomeRequest <$> nm))
    return (fromMaybe "" <$> switchDyn st) 
    
paginaInserePet :: ( DomBuilder t m
       , PostBuild t m
       , MonadHold t m
       , Prerender js t m
       ) => m ()
paginaInserePet = do
    st <- pagReq 
    tx <- holdDyn "" st
    el "div" (dynText tx)        

------------------------


------FRONTEND-----------
data Pagina = HomePage | Pet | Agenda | Sobre | PetAdd

clickLi :: DomBuilder t m => Pagina -> T.Text -> m (Event t Pagina)
clickLi p t = do
    (ev, _) <- el' "li" (elAttr "a" ("class" =: "nav-link active" <> "aria-current" =: "page" <> "href" =: "#") (text t))
    return ((\_ -> p) <$> domEvent Click ev)
    
menuLi :: (DomBuilder t m, MonadHold t m) => m (Dynamic t Pagina)
menuLi = do
    evs <- elAttr "div" ("class" =: "container-fluid") $
            elAttr "ul" ("class" =: "navbar-nav") $ do
        p1 <- clickLi HomePage "Home"
        p2 <- clickLi Pet "Pet"
        p3 <- clickLi Agenda "Agenda"
        p4 <- clickLi Sobre "Sobre"
        return (leftmost [p1,p2,p3,p4])
    holdDyn HomePage evs    
    
currPag :: (DomBuilder t m, MonadHold t m, PostBuild t m, Prerender js0 t m) => Pagina -> m ()
currPag p = 
    case p of
         HomePage -> homePage
         Pet -> paginaInserePet--petPage
         Agenda -> agendaPage
         Sobre -> sobrePage
         PetAdd -> paginaInserePet
         
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
