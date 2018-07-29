{-# LANGUAGE OverloadedStrings, RankNTypes, KindSignatures #-}
module Main where

import Web.Spock
import Web.Spock.Config
import Control.Monad (forM_)
import Data.Monoid
import Data.IORef
import Data.Text
import Control.Monad.IO.Class (liftIO, MonadIO)
import Web.Spock.Lucid (lucid)
import Lucid
import Data.Semigroup ((<>))
import Web.HttpApiData

-- data MySession = EmptySession
-- data MyAppState = DummyAppState (IORef Int)
type Server a = SpockM () () ServerState a

newtype ServerState = ServerState { notes :: IORef [Note] }

data Note = Note { author :: Text, contents :: Text }

-- main :: IO ()
-- main =
--     do ref <- newIORef 0
--        spockCfg <- defaultSpockCfg EmptySession PCNoDatabase (DummyAppState ref)
--        runSpock 8080 (spock spockCfg app)
main :: IO ()
main = do
  st <- ServerState <$>
    newIORef [ Note "Reproducible Builds" "Done"
             , Note "Backpack" "Add more tests"
             , Note "Eta-android" "Work in progress! Add documentation."
             , Note "Etlas website" "Work on package-specific layout"
             ]
  cfg <- defaultSpockCfg () PCNoDatabase st
  runSpock 8080 (spock cfg app)

app :: Server ()
app = do
  get root $ do
    notes' <- getState >>= (liftIO . readIORef . notes)
    lucid $ do
      title_ "Eta Spock Example"
      style_ [] ("body { background-color: #2cd4d9; } ul{ color: #ffffff;} " :: Text)
      style_ [] ("li { font-size: 18px; line-height: 1.6} " :: Text)
      h1_ [id_ "notes-title",style_ "text-align: center; color: #ffffff; padding-top: 50px;}"] "Eta's To-Do"
      div_ ""
      (ul_ [id_ "header",style_ "background-color: #43414e; width: 400px; padding: 60px; margin: auto; border-radius: 10px; -webkit-box-shadow: 0 10px 6px -6px #777; -moz-box-shadow: 0 10px 6px -6px #777; box-shadow: 0 10px 6px -6px #777;"] $ forM_ notes' $ \note -> li_ $ do
        toHtml (author note)
        ": "
        toHtml (contents note))
      h2_ [id_ "new-note",style_ "color:white; text-align: center; padding-top: 30px;"] "Add New Note"
      form_ [style_"margin: auto; background-color: #43414e; width: 400px; padding: 60px; border-radius: 10px; -webkit-box-shadow: 0 10px 6px -6px #777; -moz-box-shadow: 0 10px 6px -6px #777; box-shadow: 0 10px 6px -6px #777;",method_ "post"] $ do
        label_ [style_"color: #ffffff; padding-left: 100px;"] $ do
          "Feature:  "
          input_ [name_ "author"]
        div_ ""
        label_ [style_"color: #ffffff; padding-left: 50px; margin: 40px;"] $ do
          "Remarks:  "
          textarea_ [name_ "contents",rows_ "2",cols_ "18"] ""
        div_ ""
        input_ [style_"margin-left: 170px; margin-top: 20px", type_ "submit", value_ "Add Note"]
  post root $ do
    author <- param' "author"
    contents <- param' "contents"
    notesRef <- notes <$> getState
    liftIO $ atomicModifyIORef' notesRef $ \notes ->
      (notes <> [Note author contents], ())
    redirect "/"


-- app :: SpockM () MySession MyAppState ()
-- app =
--     do get root $
--            text "Hello World!"
--        get ("hello" <//> var) $ \name ->
--            do (DummyAppState ref) <- getState
--               visitorNumber <- liftIO $ atomicModifyIORef' ref $ \i -> (i+1, i+1)
--               text ("Hello " <> name <> ", you are visitor number " <> T.pack (show visitorNumber))
