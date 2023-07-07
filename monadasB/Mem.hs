module Mem where
-- La definición del TAD Mem
type Variable = String

class Mem mem where
  enBlanco :: mem
  cuantoVale :: Variable -> mem -> Maybe Int
  recordar :: Variable -> Int -> mem -> mem