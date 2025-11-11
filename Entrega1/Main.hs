import BellLaPadula


main :: IO ()
main = do
  let alice = User "Alice" Secret
      report = File "report.txt" Confidential
  putStrLn ("Can Alice read? " ++ show (grant alice report Read))
  putStrLn ("Can Alice write? " ++ show (grant alice report Write))
