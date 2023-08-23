import System.Environment
import Text.Read (readMaybe)

f :: Double -> Double -> Double
f a x = (a^2 * x) - (4*a*x^2) + (4*x^3)

dfdx :: Double -> Double -> Double
dfdx a x = a^2 - (8*a*x) + (12*x^2)

quadraticRoots :: Double -> Double -> Double -> Maybe (Double, Double)
quadraticRoots a b c = 
    let discriminant = b^2 - 4*a*c
    in if discriminant >= 0 then
        let root1 = (-b + sqrt discriminant) / (2*a)
            root2 = (-b - sqrt discriminant) / (2*a)
        in Just (root1, root2)
       else
        Nothing

findBiggestXForA :: Double -> Maybe Double
findBiggestXForA a = do
    (x1, x2) <- quadraticRoots 12.0 (-8 * a) (a^2)
    return $ min x1 x2

main :: IO ()
main = do
    args <- getArgs
    case args of
        (input:_) -> case readMaybe input :: Maybe Double of
            Just a -> case findBiggestXForA a of
                Just biggestX -> do
                    putStrLn $ "the biggest x value for which dfdx = 0 when a = " ++ show a ++ " is: " ++ show biggestX
                    putStrLn $ "f(a, biggestX) = " ++ show (f a biggestX)
                Nothing -> putStrLn "the equation does not have real roots for df/dx = 0 for the given value of a."
            Nothing -> putStrLn "please enter a valid number for a."
        _ -> putStrLn "please provide an argument."
