{- Copyright (C) 2015 Calvin Beck
   Permission is hereby granted, free of charge, to any person
   obtaining a copy of this software and associated documentation files
   (the "Software"), to deal in the Software without restriction,
   including without limitation the rights to use, copy, modify, merge,
   publish, distribute, sublicense, and/or sell copies of the Software,
   and to permit persons to whom the Software is furnished to do so,
   subject to the following conditions:
   The above copyright notice and this permission notice shall be
   included in all copies or substantial portions of the Software.
   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
   BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
   ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE.
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Servant
import Servant.Server
import Servant.HTML.Lucid
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import qualified Data.Text as T
import Data.Aeson
import Lucid
import Control.Monad.Trans.Either
import Control.Monad.State.Strict
import Control.Monad.Reader
import Data.Acid
import Data.SafeCopy
import Database                 

type PasteAPI = "view" :> Capture "numPosts" Int :> Get '[JSON, HTML] [Paste]
                :<|> "paste" :> ReqBody '[JSON] T.Text :> Post '[JSON, HTML] T.Text

pasteAPI :: Proxy PasteAPI
pasteAPI = Proxy

type PasteM = ReaderT (AcidState PasteDB) (EitherT ServantErr IO)

pasteServer :: ServerT PasteAPI PasteM
pasteServer = viewPastes :<|> makePaste
  where viewPastes :: Int -> PasteM [Paste]
        viewPastes n = do db <- ask
                          liftIO $ query db (ViewPastes n)

        makePaste :: T.Text -> PasteM T.Text
        makePaste t = do db <- ask
                         liftIO $ update db (AddPaste t)

                         return "Paste successfull!"

acidToEither :: AcidState PasteDB -> PasteM :~> EitherT ServantErr IO
acidToEither db = Nat $ acidToEither' db
  where acidToEither' :: forall a. AcidState PasteDB -> PasteM a -> EitherT ServantErr IO a
        acidToEither' db r = runReaderT r db

app :: AcidState PasteDB -> Application
app db = serve pasteAPI (enter (acidToEither db) pasteServer)

myPastes :: [Paste]
myPastes = zipWith Paste [1..] ["Yay", "OMG #COOLPASTE}\\", "<em>SCARY HTML</em>"]

main :: IO ()
main = do db <- openLocalStateFrom ".pastedb/" (PasteDB (0,[]))
          run 8001 (app db)
