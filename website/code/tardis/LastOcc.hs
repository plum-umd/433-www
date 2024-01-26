import Control.Monad.Tardis
import Control.Monad

import qualified Data.Set as Set 

import Data.Foldable(foldr')

-- lastOccurrences [1,2,3,4,3,1] == [False,True,False,True,True,True]

lastOccurrences :: [Int] -> [Bool]
lastOccurrences xs =
  fst $ foldr' (\x (xs', set) -> ((not $ Set.member x set) : xs', Set.insert x set)) ([], Set.empty) xs

lastOccurrencesTardis :: [Int] -> [Bool]
lastOccurrencesTardis xs =
  flip evalTardis (Set.empty, ()) $ forM xs $ \el -> do
    modifyBackwards (Set.insert el)
    not <$> getsFuture (Set.member el)

main = do
  let lst = [1..100] ++ [100,99..1] :: [Int]
  putStrLn $ show $ lastOccurrencesTardis lst


