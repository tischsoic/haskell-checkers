module Board(
    DataBox(..),
    getDumbText
) where

getDumbText = "Asdf is not dumb text!"

data DataBox a = DataBox a String
    deriving (Read, Eq, Show)
