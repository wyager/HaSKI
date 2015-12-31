module Model.Model (SKI(S,K,I,T,L)) where

data SKI = S | K | I | T SKI SKI | L Char deriving Show

