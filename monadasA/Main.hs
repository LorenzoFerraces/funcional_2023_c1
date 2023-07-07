import Monadas -- Las definiciones de las clases necesarias
import MaybeMonad -- La definición de la mónada Maybe
import ErrorMonad -- La definición de la mónada Error
import OutputMonad -- La definición de la mónada Output
import RevOutputMonad -- La definición de la mónada RevOutput
import PowerOutputMonad -- La definición de la mónada PowerOutput
import OutputErrorMonad -- La definición de la mónada OutputError
import Eval -- La aplicación (diferentes evals para E)

main = probar 2

probar :: Int -> IO ()
probar i = do print (prueba i)
              print (pruebaM1 i)
              print (pruebaM2 i)
              print (pruebaME1 i)
              print (pruebaME2 i)
              print (pruebaMP1 i)
              print (pruebaMP2 i)
              print (pruebaMP3 i)
              print (pruebaMPE i)

prueba :: Int -> Float
prueba 0 = eval ej0
prueba 1 = eval ej1
prueba 2 = eval ej2

pruebaM1 :: Int -> Maybe Float
pruebaM1 0 = evalM ej0
pruebaM1 1 = evalM ej1
pruebaM1 2 = evalM ej2

pruebaM2 :: Int -> Error Float
pruebaM2 0 = evalM ej0
pruebaM2 1 = evalM ej1
pruebaM2 2 = evalM ej2

pruebaME1 :: Int -> Maybe Float
pruebaME1 0 = evalME ej0
pruebaME1 1 = evalME ej1
pruebaME1 2 = evalME ej2

pruebaME2 :: Int -> Error Float
pruebaME2 0 = evalME ej0
pruebaME2 1 = evalME ej1
pruebaME2 2 = evalME ej2

pruebaMP1 :: Int -> Output Float
pruebaMP1 0 = evalMP ej0
pruebaMP1 1 = evalMP ej1
pruebaMP1 2 = evalMP ej2

pruebaMP2 :: Int -> RevOutput Float
pruebaMP2 0 = evalMP ej0
pruebaMP2 1 = evalMP ej1
pruebaMP2 2 = evalMP ej2

pruebaMP3 :: Int -> PowerOutput Float
pruebaMP3 0 = evalMP ej0
pruebaMP3 1 = evalMP ej1
pruebaMP3 2 = evalMP ej2

pruebaMPE :: Int -> OutputError Float
pruebaMPE 0 = evalMPE ej0
pruebaMPE 1 = evalMPE ej1
pruebaMPE 2 = evalMPE ej2