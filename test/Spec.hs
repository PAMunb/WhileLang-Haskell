import CFGFactorialTest
import CFGPowerTest
import Test.HUnit 

main :: IO ()
main = do 
  runTestTT testFactorialSuite
  runTestTT testPowerSuite
  putStrLn "Done."
