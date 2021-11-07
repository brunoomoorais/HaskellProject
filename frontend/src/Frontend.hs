{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Frontend where

import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Language.Javascript.JSaddle (eval, liftJSM)

import Obelisk.Frontend
import Obelisk.Configs
import Obelisk.Route
import Obelisk.Generated.Static

import Reflex.Dom.Core

import Common.Api
import Common.Route


-- This runs in a monad that can be run on the client or the server.
-- To run code in a pure client or pure server context, use one of the
-- `prerender` functions.
frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Obelisk Minimal Example"      
      elAttr "link" ("href" =: static @"main.css" <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
      elAttr "link" ("href" =: static @"bootstrap.min.css" <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
      elAttr "meta" ("charset" =: "utf-8") blank
      elAttr "meta" ("name" =: "viewport" <> "content" =: "width=device-width, initial-scale=1") blank

      -- <!-- Bootstrap CSS -->
      -- elAttr "link" ("href" =: "https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/css/bootstrap.min.css" <> "integrity" =: "sha384-1BmE4kWBq78iYhFldvKuhfTAU6auU8tT94WrHftjDbrCEXSU1oBoqyl2QvZ6jIW3" <> "crossorigin" =: "anonymous") blank
  , _frontend_body = do
      el "h1" $ text "Welcome to ProjectHaskell!"
      el "p" $ text $ T.pack commonStuff

      elAttr "nav" ("class" =: "navbar navbar-expand-lg navbar-light bg-light") $
        elAttr "div" ("class" =: "container-fluid") $
          elAttr "ul" ("class" =: "navbar-nav") $ do             
            (elAttr "li" ("class" =: "navbar-nav") $ 
              elAttr "a" ("class" =: "nav-link active" <> "aria-current" =: "page" <> "href" =: "#") $ text "Home")
            (elAttr "li" ("class" =: "navbar-nav") $ 
              elAttr "a" ("class" =: "nav-link active" <> "aria-current" =: "page" <> "href" =: "#") $ text "Contato")            
            (elAttr "li" ("class" =: "navbar-nav") $ 
              elAttr "a" ("class" =: "nav-link active" <> "aria-current" =: "page" <> "href" =: "#") $ text "Sobre")            

        -- <nav class="navbar navbar-expand-lg navbar-light bg-light">
        --   <div class="container-fluid">
        --     
        --     <div class="collapse navbar-collapse" id="navbarNav">
        --       <ul class="navbar-nav">
        --         <li class="nav-item">
        --           <a class="nav-link active" aria-current="page" href="#">Home</a>
        --         </li>
        --         <li class="nav-item">
        --           <a class="nav-link" href="#">Features</a>
        --         </li>
        --         <li class="nav-item">
        --           <a class="nav-link" href="#">Pricing</a>
        --         </li>
        --         <li class="nav-item">
        --           <a class="nav-link disabled">Disabled</a>
        --         </li>
        --       </ul>
        --     </div>
        --   </div>
        -- </nav>

      -- <nav class="navbar navbar-light bg-light">
      --   <div class="container-fluid">
      --     <a class="navbar-brand" href="#">Navbar</a>
      --   </div>
      -- </nav>
      -- elAttr "nav" ("class" =: "navbar navbar-light bg-light") $ 
      --   elAttr "div" ("class" =: "container-fluid") $
      --     elAttr "a" ("class" =: "navbar-brand" <> "href" =: "https://www.google.com" <> "target" =: "_blank") $
      --       text "aloou1"

      -- elAttr "nav" ("class" =: "navbar navbar-light bg-light") $ 
      --   elAttr "div" ("class" =: "container-fluid") $
      --     elAttr "a" ("class" =: "navbar-brand" <> "href" =: "https://www.google.com" <> "target" =: "_blank") $
      --       text "aloou2"

      -- elAttr "nav" ("class" =: "navbar navbar-light bg-light") $ 
      --   elAttr "div" ("class" =: "container-fluid") $
      --     elAttr "a" ("class" =: "navbar-brand" <> "href" =: "https://www.google.com" <> "target" =: "_blank") $
      --       text "aloou3"

      -- `prerender` and `prerender_` let you choose a widget to run on the server
      -- during prerendering and a different widget to run on the client with
      -- JavaScript. The following will generate a `blank` widget on the server and
      -- print "Hello, World!" on the client.
      prerender_ blank $ liftJSM $ void $ eval ("console.log('Hello, World!')" :: T.Text)

      elAttr "img" ("src" =: static @"logo.jpg") blank
      el "div" $ do
        exampleConfig <- getConfig "common/example"
        case exampleConfig of
          Nothing -> text "No config file found in config/common/example"
          Just s -> text $ T.decodeUtf8 s       
      -- <script src="https://cdn.jsdelivr.net/npm/@popperjs/core@2.10.2/dist/umd/popper.min.js" integrity="sha384-7+zCNj/IqJ95wo16oMtfsKbZ9ccEh31eOz1HGyDuCQ6wgnyJNSYdrPa03rtR1zdB" crossorigin="anonymous"></script>
      -- <script src="https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/js/bootstrap.min.js" integrity="sha384-QJHtvGhmr9XOIpI6YVutG+2QOK9T+ZnN4kzFN1RtK3zEFEIsxhlmWl5/YESvpZ13" crossorigin="anonymous"></script>
      -- <script src="https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/js/bootstrap.bundle.min.js" integrity="sha384-ka7Sk0Gln4gmtz2MlQnikT1wXgYsOg+OMhuP+IlRH9sENBO0LRn5q+8nbTov4+1p" crossorigin="anonymous"></script>
      -- elAttr "script" ("src" =: "https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/js/bootstrap.bundle.min.js" <> "integrity" =: "sha384-ka7Sk0Gln4gmtz2MlQnikT1wXgYsOg+OMhuP+IlRH9sENBO0LRn5q+8nbTov4+1p" <> "crossorigin" =: "anonymous") blank
      -- elAttr "script" ("src" =: "https://cdn.jsdelivr.net/npm/@popperjs/core@2.10.2/dist/umd/popper.min.js" <> "integrity" =: "sha384-7+zCNj/IqJ95wo16oMtfsKbZ9ccEh31eOz1HGyDuCQ6wgnyJNSYdrPa03rtR1zdB" <> "crossorigin" =: "anonymous") blank
      -- elAttr "script" ("src" =: "https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/js/bootstrap.min.js" <> "integrity" =: "sha384-QJHtvGhmr9XOIpI6YVutG+2QOK9T+ZnN4kzFN1RtK3zEFEIsxhlmWl5/YESvpZ13" <> "crossorigin" =: "anonymous") blank
      return ()
  }
