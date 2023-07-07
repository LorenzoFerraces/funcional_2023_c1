module ListMem where
-- Una posible implementación del TAD Mem
import Mem
data ListMem = LM [(Variable, Int)]

instance Mem ListMem where
  enBlanco = LM []
  cuantoVale v (LM vns) = lookup v vns
  recordar v n (LM vns) = LM (recordarRep v n vns)

recordarRep v n [] = [(v,n)]
recordarRep v n ((v',n'):vns) =
  if v==v'
    then (v',n) : vns
    else (v',n') : recordarRep v n vns