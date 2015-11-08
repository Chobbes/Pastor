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

module Database where

import Data.Aeson
import Lucid
import GHC.Generics
import qualified Data.Text as T
import Data.SafeCopy
import Control.Monad.State
import Control.Monad.Reader
import Data.Acid
import qualified Data.IntMap as IM
import Data.Maybe


-- | Pastes have a text titles, and content.
data Paste = Paste { pasteTitle :: T.Text
                   , pasteContent :: T.Text
                   }
             deriving (Generic, Show, Eq)

instance ToJSON Paste
instance FromJSON Paste

instance ToHtml Paste where
  toHtml paste = div_ . toHtml $ pasteContent paste

instance ToHtml [Paste] where
  toHtml pastes = div_ $ foldMap toHtml pastes

instance ToHtml (Maybe Paste) where
  toHtml Nothing = div_ . toHtml $ ("No such paste, sorry!" :: T.Text)
  toHtml (Just paste) = toHtml paste

data PasteDB = PasteDB (Int, IM.IntMap Paste)
$(deriveSafeCopy 0 'base ''Paste)
$(deriveSafeCopy 0 'base ''PasteDB)


addPaste :: Paste -> Update PasteDB ()
addPaste paste = do PasteDB (id, pastes) <- get
                    put $ PasteDB ((id+1), IM.insert (id+1) paste pastes)


viewPaste :: Int -> Query PasteDB (Maybe Paste)
viewPaste n = do PasteDB (_, pasteMap) <- ask
                 return $ IM.lookup n pasteMap


latestPastes :: Int -> Query PasteDB [Paste]
latestPastes n = do PasteDB (lastId, pasteMap) <- ask
                    return . take n . catMaybes $ [IM.lookup id | id <- [lastId,lastId-1..0]] <*> (return pasteMap)


$(makeAcidic ''PasteDB ['addPaste, 'viewPaste, 'latestPastes])
