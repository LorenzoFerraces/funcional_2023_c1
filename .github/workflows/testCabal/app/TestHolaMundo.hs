import Test.HUnit
import HolaMundo (holaMundo)

testHolaMundo :: Test
testHolaMundo = TestCase (assertEqual "Testing holaMundo" "Hola Mundo" (holaMundo))

main :: IO ()
main = do
    runTestTT $ TestList [testHolaMundo]
    return ()
    
