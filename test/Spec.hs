-- import CFGFactorialTest
-- import CFGPowerTest
import DFATest
import Test.HUnit 

main :: IO ()
main = do 
  -- runTestTT testFactorialSuite
  -- runTestTT testPowerSuite
  runTestTT testDFASuite
  putStrLn "Done."
