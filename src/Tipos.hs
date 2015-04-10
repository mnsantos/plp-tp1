module Tipos where

import Grafo

type Mundo = Integer
type Prop = String
data Modelo = K (Grafo Mundo) (Prop -> [Mundo])

data Exp = Var Prop | Not Exp | Or Exp Exp | And Exp Exp | D Exp | B Exp deriving (Show, Eq)
