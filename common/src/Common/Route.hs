{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Common.Route where

{- -- You will probably want these imports for composing Encoders.
import Prelude hiding (id, (.))
import Control.Category
-}

import Data.Text (Text, unpack)
import Data.Function
import Data.Functor.Identity

import Obelisk.Route
import Obelisk.Route.TH
  

data BackendRoute :: * -> * where
  -- | Used to handle unparseable routes.
  BackendRoute_Missing :: BackendRoute ()
  -- You can define any routes that will be handled specially by the backend here.
  -- i.e. These do not serve the frontend, but do something different, such as serving static files.
  -- BackendRoute_PetRoute :: BackendRoute ()
  BackendRoute_AgendaJson :: BackendRoute ()
  BackendRoute_ClienteJson :: BackendRoute ()
  BackendRoute_PetJson :: BackendRoute ()

  BackendRoute_ClienteListar :: BackendRoute ()
  BackendRoute_PetListar :: BackendRoute ()
  BackendRoute_AgendaListar :: BackendRoute ()

  BackendRoute_ClienteBuscar :: BackendRoute Int
  BackendRoute_AgendaBuscar :: BackendRoute Int
  BackendRoute_PetBuscar :: BackendRoute Int

  BackendRoute_ClienteDelete :: BackendRoute Int
  BackendRoute_AgendaDelete :: BackendRoute Int
  BackendRoute_PetDelete :: BackendRoute Int

  BackendRoute_ClienteEditar :: BackendRoute Int
  BackendRoute_AgendaEditar :: BackendRoute Int
  BackendRoute_PetEditar :: BackendRoute Int

data FrontendRoute :: * -> * where
  FrontendRoute_Main :: FrontendRoute ()
  -- This type is used to define frontend routes, i.e. ones for which the backend will serve the frontend.

checFullREnc
  :: Encoder Identity Identity (R (FullRoute BackendRoute FrontendRoute)) PageName
checFullREnc = checkEncoder fullRouteEncoder & \case
  Left err -> error $ unpack err
  Right encoder -> encoder    

fullRouteEncoder
  :: Encoder (Either Text) Identity (R (FullRoute BackendRoute FrontendRoute)) PageName
fullRouteEncoder = mkFullRouteEncoder
  (FullRoute_Backend BackendRoute_Missing :/ ())
  (\case
      BackendRoute_Missing -> PathSegment "missing" $ unitEncoder mempty
      BackendRoute_AgendaJson -> PathSegment "agenda" $ unitEncoder mempty
      -- BackendRoute_PetRoute -> PathSegment "petz" $ unitEncoder mempty
      BackendRoute_ClienteJson -> PathSegment "cliente" $ unitEncoder mempty
      BackendRoute_PetJson  -> PathSegment "pet" $ unitEncoder mempty     

      BackendRoute_ClienteListar ->  PathSegment "cliente/lista" $ unitEncoder mempty     
      BackendRoute_PetListar ->  PathSegment "pet/lista" $ unitEncoder mempty     
      BackendRoute_AgendaListar ->  PathSegment "agenda/lista" $ unitEncoder mempty     

      BackendRoute_ClienteBuscar  -> PathSegment "findCliente" readShowEncoder 
      BackendRoute_AgendaBuscar  -> PathSegment "findAgenda" readShowEncoder 
      BackendRoute_PetBuscar  -> PathSegment "findPet" readShowEncoder 

      BackendRoute_ClienteDelete  -> PathSegment "deleteCliente" readShowEncoder       
      BackendRoute_AgendaDelete  -> PathSegment "deleteAgenda" readShowEncoder 
      BackendRoute_PetDelete  -> PathSegment "deletePet" readShowEncoder 

      BackendRoute_ClienteEditar  -> PathSegment "editCliente" readShowEncoder 
      BackendRoute_AgendaEditar  -> PathSegment "editAgenda" readShowEncoder 
      BackendRoute_PetEditar  -> PathSegment "editPet" readShowEncoder 
      )
  (\case
      FrontendRoute_Main -> PathEnd $ unitEncoder mempty)

concat <$> mapM deriveRouteComponent
  [ ''BackendRoute
  , ''FrontendRoute
  ]
