module Eval where
import Monadas
-- Un tipo de expresiones para probar mónadas
data E = Cte Float | Div E E

-- Una operación con bottom
-- (porque el (/) ahora usa la porquería de IEEE 1.#INF...)
_ ./. 0 = error "Undefined"
n ./. m = n/m

ej0 = Div (Cte 1) (Cte 0)
ej1 = Div (Cte 18) (Cte 3)
ej2 = Div (Div (Cte 80) (Cte 10))
          (Div (Cte 20) (Cte 5))

armarDiv v1 v2 = show v1 ++ " / "
              ++ show v2 ++ " = "
              ++ show (v1./.v2)

-- El evaluador básico (este se puede ejecutar sin más trámite)
eval :: E -> Float
eval (Cte n) = n
eval (Div e1 e2) = eval e1 ./. eval e2

-- El evaluador monádico (para poder ejecutar este hay que indicar
--  de alguna forma cuál es la mónada. Una forma típica de hacerlo
--  es dar el tipo del resultado:
--    evalME (Cte 2) :: Maybe Float )
evalM :: Monad m => E -> m Float
evalM (Cte n) = return n
evalM (Div e1 e2) = evalM e1 <./.> evalM e2

(<./.>) :: Monad m => m Float -> m Float -> m Float
m1 <./.> m2 = liftM2 (./.) m1 m2
liftM2 f m1 m2 =  m1 >>= \v1 ->
                  m2 >>= \v2 ->
                  return (f v1 v2)

-- El evaluador, con control de error (mismo tema que con todos los
-- que tienen clases en las restricciones de contexto)
evalME :: ErrorMonad m => E -> m Float
evalME (Cte n) = return n
evalME (Div e1 e2) = evalME e1 </> evalME e2

(</>) :: ErrorMonad m => m Float -> m Float -> m Float
m1 </> m2 = m1 >>= \v1 ->
            m2 >>= \v2 ->
            if v2==0
              then throw "No puedo dividir por cero"
              else return (v1./.v2)

-- El evaluador, con impresión de traza (mismo tema que con todos
-- los que tienen clases en las restricciones de contexto)
evalMP :: PrintMonad m => E -> m Float
evalMP (Cte n) = return n
evalMP (Div e1 e2) = evalMP e1 <<./.>> evalMP e2

(<<./.>>) :: PrintMonad m => m Float -> m Float -> m Float
m1 <<./.>> m2 = m1 >>= \v1 ->
                m2 >>= \v2 ->
                printf (armarDiv v1 v2) >>= \_ ->
                return (v1./.v2)

-- El evaluador, combinando traza y control de error (mismo tema
-- que con todos los que tienen clases en las restricciones
-- de contexto)
evalMPE :: (ErrorMonad m, PrintMonad m) => E -> m Float
evalMPE (Cte n) = return n
evalMPE (Div e1 e2) = evalMPE e1 <</>> evalMPE e2

(<</>>) :: (ErrorMonad m, PrintMonad m) => m Float -> m Float -> m Float
m1 <</>> m2 = m1 >>= \v1 ->
              m2 >>= \v2 ->
              if v2==0
                then throw "No puedo dividir por cero"
                else printf (armarDiv v1 v2) >>= \_ ->
                      return (v1./.v2)