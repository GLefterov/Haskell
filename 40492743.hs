{-
 
REPLACE the function definitions for each of the questions. 
The names of the functions correspond to the names given in the document cwk21handout.pdf. 
 
DO NOT CHANGE THE TYPE SIGNATURES, OR THE ORDER OF THE ARGUMENTS!
 
You may add as many extra or helper functions as you wish, but do not make any "import" statements.
-}
 
-- QUESTION 1: Sets
 
bigUnion :: (Eq a) => [[a]] -> [a]
bigUnion = concat
 
partialSums :: [Int] -> [Int]
partialSums = tail . reverse . foldl (\(sum:rest) x -> (sum + x):sum:rest) [0]
 
maxIndex :: [Int] -> Maybe Int
maxIndex = go . zip [0..] . partialSums where
    go (x : xs) = Just $ (+ 1) $ fst $ foldl select_max x xs
    go [] = Nothing
    select_max y x = if snd x > snd y then x else y
 
-- TEST SET FOR Q1
{-
Your functions should have the following behaviour:
 
bigUnion [[1,2,3],[3,4,5],[2,4,6,8]] = [1,2,3,4,5,6,8]
bigUnion ["list a", "list b"] = "list ab"
 
THE ORDER OF ELEMENTS IN THE RESULTS bigUnion IS NOT IMPORTANT.
 
partialSums [1,2,3,4,5] = [1,3,6,10,15]
partialSums [-1,1,-1,1,-1] = [-1,0,-1,0,-1]
 
maxIndex [1,2,3,4,5] = Just 5
maxIndex [-1,1,-1,1,-1] = Just 2
 
-}
 
 
 
-- QUESTION 2: Functions and relations
 
makeCommutative :: [((Int,Int),Int)] -> [((Int,Int),Int)]
makeCommutative xss = foldl maybeInsert xss $ cartesian domain domain where
    domain = unique $ foldl (\out ((a,b),y) -> a:b:out) [] xss
    cartesian xs ys = [(x,y) | x <- xs, y <- ys]
    maybeInsert f (a,b) = case (lookup (a,b) f, lookup (b,a) f) of
        (Just y,Nothing) -> ((b,a),y) : f
        (Nothing,Nothing) -> case a == b of
            True -> ((a,a),a) : f
            _ -> ((a,b),a) : ((b,a),a) : f
        _ -> f
 
unique [] = []
unique (x:xs) = x : unique (filter (not . (== x)) xs)
 
oneHop :: (Eq a) => a -> [(a,a)] -> [a]
oneHop y = map snd . filter ((== y) . fst)
 
nextSteps :: (Eq a) => [a] -> [(a,a)] -> [[a]]
nextSteps [] xs = map (\x -> [x]) $ unique $ foldl (\out (a,b) -> a:b:out) [] xs
nextSteps ps xs = map (\x -> ps ++ [x]) $ oneHop (last ps) xs
 
allElementsReachable :: (Eq a) => Int -> a -> [(a,a)] -> [a]
allElementsReachable 0 x rs = [x]
allElementsReachable n x rs = unique $ concatMap (flip oneHop rs) almost where
    almost = allElementsReachable (n-1) x rs
 
-- TEST SET FOR Q2
{-
Your functions should have the following behaviour:
 
makeCommutative [((1,2),3),((3,2),5),((1,4),0)] = 
    [((2,1),3),((2,3),5),((4,1),0),((1,2),3),((3,2),5),((1,4),0),((3,3),3),((1,1),1),((2,2),2),((4,4),4)]
 
makeCommutative [((4,1),0)] =
    [((1,4),0),((4,1),0),((4,4),4),((1,1),1)]
 
oneHop 3 [(1,3),(3,2),(3,4),(3,1),(2,3),(1,4)] = [2,4,1]
oneHop 1 [(1,3),(3,2),(3,4),(3,1),(2,3),(1,4)] = [3,4]
 
DO NOT WORRY ABOUT THE ORDER OF THE ELEMENTS IN THE RETURN LIST FOR oneHop
 
nextSteps [1,3] [(1,3),(3,2),(3,4),(3,1),(2,3),(1,4)] = [[1,3,2],[1,3,4],[1,3,1]]
nextSteps [3,4] [(1,3),(3,2),(3,4),(3,1),(2,3),(1,4)] = []
 
DO NOT WORRY ABOUT THE ORDER OF THE ELEMENTS IN THE RETURN LIST (i.e. THE ORDER THE LISTS APPEAR IN THE LIST OF LISTS)
 
allElementsReachable 2 1 [(1,3),(3,2),(3,4),(3,1),(2,3),(1,4)] = [2,4,1]
allElementsReachable 6 4 [(1,3),(3,2),(3,4),(3,1),(2,3),(1,4)] = []
 
DO NOT WORRY ABOUT THE ORDER OF THE ELEMENTS IN THE RETURN LIST FOR allElementsReachable
 
-}
 
 
 
-- QUESTION 3: Primes
 
lastPrimes n = take 3 $ reverse $ takeWhile (< n) $ primes
 
primes = 2 : g (fix g) where
   g xs = 3 : (gaps 5 $ unionAll [[p*p, p*p+2*p..] | p <- xs])
 
unionAll ((x:xs):t) = x : union xs (unionAll $ pairs t) where
  pairs ((x:xs):ys:t) = (x : union xs ys) : pairs t
 
fix g = xs where xs = g xs
 
union (x:xs) (y:ys) = case compare x y of LT -> x : union  xs (y:ys)
                                          EQ -> x : union  xs    ys 
                                          GT -> y : union (x:xs) ys
 
gaps k s@(x:xs) | k<x  = k:gaps (k+2) s    
                | True =   gaps (k+2) xs 
 
primeFactors :: Int -> Maybe [Int]
primeFactors n = case primeFactors' n of
    [] -> Nothing
    out -> Just out
 
primeFactors' n = case allPrimeFactors n of
    [p] | p == n -> []
    out -> out
 
allPrimeFactors n = factors [] n primes where
    factors out 1 _ = out
    factors out m (p : rest) = case m `rem` p of
        0 -> factors (p : out) (fix factor_out_p m) rest
          where
            factor_out_p f x = case x `divMod` p of
                (x', 0) -> f x'
                _ -> x
        _ -> factors out m rest
 
{- 
Leave the error messages in place if you do not want to attempt the parts for the input size. You should remove the guards up to the point you want to attempt. For example, if you were confident of anything up to five digits, the function would look like:
 
primeFactors n
    | n <= 99999 = whatever_your_calculation_is
    | n <= 999999 = error "..."
    | otherwise = error "..."
 
 -}
 
 
 
 
-- TEST SET FOR Q3
{-
Your functions should have the following behaviour:
 
lastPrimes 73 = [71,67,61]
lastPrimes 64 = [61,59,53]
 
DO NOT WORRY ABOUT THE ORDER OF THE LIST FOR lastPrimes
 
primeFactors 75 = Just [3,5]
primeFactors 64 = Just [2]
primeFactors 61 = Nothing
 
DO NOT WORRY ABOUT THE ORDER OF THE LIST FOR primeFactors
-}
 
 
 
 
-- QUESTION 4: RSA
 
eTotient :: Int -> Int
eTotient n = undefined
 
encode :: Int -> Int -> Int -> Int -> Maybe Int
encode p q m e = undefined
 
-- TEST SET FOR Q4
{-
Your functions should have the following behaviour:
eTotient 54 = 18
eTotient 73 = 72
encode 37 23 29 5 = Just 347
encode 99 18 108 45 = Nothing
encode 37 17 23 48 = Nothing
-}