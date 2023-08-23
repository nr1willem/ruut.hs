import System.Environment
import Text.Read (readMaybe)

{-
Ruudukujulise plekitüki serv on a cm.
Kui suured ruudud tuleb nurkadest ära lõigata, et saada maksimaalse ruumalaga pealt lahtine karp?
-}

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
                    putStrLn $ "ruudu serv a = " ++ show a ++ " et saada maksimaalse ruumalaga pealt lahtine karp peab lõikama nurkadest ruudud mille suuruseks on: " ++ show biggestX
                    putStrLn $ "f(a, biggestX) = " ++ show (f a biggestX)
                Nothing -> putStrLn "lahendid puuduvad"
            Nothing -> putStrLn "palun andke number"
        _ -> putStrLn "palun andke argument"
