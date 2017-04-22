{-# LANGUAGE TemplateHaskell #-}

module SexEnum where

import Database.Persist.TH

data Sex = Male | Female | Unspecified deriving (Eq, Enum, Show, Read)
derivePersistField "Sex"
