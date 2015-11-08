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

-- | Pastes are given a unique integer identifier, and have text content.
data Paste = Paste { pasteId :: Int
                   , pasteContent :: T.Text
                   }
             deriving (Generic, Show, Eq)

instance ToJSON Paste
instance FromJSON Paste

instance ToHtml Paste where
  toHtml paste = div_ . toHtml $ pasteContent paste

instance ToHtml [Paste] where
  toHtml pastes = div_ $ foldMap toHtml pastes

data PasteDB = PasteDB (Int, [Paste])
$(deriveSafeCopy 0 'base ''Paste)
$(deriveSafeCopy 0 'base ''PasteDB)

addPaste :: T.Text -> Update PasteDB ()
addPaste t = do PasteDB (id, pastes) <- get

                let paste = Paste (id+1) t
                put $ PasteDB ((id+1), paste:pastes)

viewPastes :: Int -> Query PasteDB [Paste]
viewPastes n = do PasteDB (_, pastes) <- ask
                  return $ take n pastes

$(makeAcidic ''PasteDB ['addPaste, 'viewPastes])
