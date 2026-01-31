
import Text.Read (readMaybe)
import Control.Monad (when)

-- Настройки
gravity :: Double
gravity = 1.62  -- Луна, м/с²

maxThrust :: Double
maxThrust = 5.0 -- максимальная сила двигателя, м/с²

safeLandingSpeed :: Double
safeLandingSpeed = 2.0 -- м/с

main :: IO ()
main = do
    putStrLn "=== Посадка космического корабля ==="
    putStrLn "Введите начальную высоту (например 100):"
    h0 <- readDouble
    putStrLn "Введите начальное топливо (например 50):"
    fuel0 <- readDouble
    let v0 = 0.0
    loop h0 v0 fuel0

readDouble :: IO Double
readDouble = do
    s <- getLine
    case readMaybe s of
      Just x -> return x
      Nothing -> putStrLn "Ошибка, введите число:" >> readDouble

loop :: Double -> Double -> Double -> IO ()
loop h v fuel
    | h <= 0 = 
        if v <= safeLandingSpeed
        then putStrLn $ "Вы успешно приземлились! Скорость: " ++ show v ++ " м/с"
        else putStrLn $ "Корабль разбился! Скорость: " ++ show v ++ " м/с"
    | otherwise = do
        putStrLn $ "\nВысота: " ++ show h ++ " м, Скорость: " ++ show v ++ " м/с, Топливо: " ++ show fuel
        thrust <- if fuel > 0
                  then do
                      putStrLn $ "Введите силу двигателя (0-" ++ show maxThrust ++ "):"
                      t <- readDouble
                      let t' = max 0 (min maxThrust t)
                      return $ if fuel <= 0 then 0 else min t' fuel 
                  else do
                      putStrLn "Топливо закончилось! Свободное падение."
                      return 0
        let fuel' = max 0 (fuel - thrust)
            a = gravity - thrust  
            v' = v + a
            h' = h - v'
        loop h' v' fuel'
