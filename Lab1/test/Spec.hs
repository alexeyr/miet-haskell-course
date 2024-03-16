import FirstSteps
import Lists
import Luhn
import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "first steps" $ do
        -- Можно вложить глубже: describe "xor" do $ ... чтобы дать названия отдельным тестам
        it "xor" $ do
            xor True True `shouldBe` False
            xor True False `shouldBe` True
            xor False True `shouldBe` True
            xor False False `shouldBe` False
        it "max3" $ do
            max3 1 3 2 `shouldBe` 3
            max3 5 2 5 `shouldBe` 5
            max3 (-10) 0 4 `shouldBe` 4
            max3 (-10) 0 (-4) `shouldBe` 0
            max3 5 5 5 `shouldBe` 5
        it "median3" $ do
            median3 1 4 7 `shouldBe` 4
            median3 1 4 4 `shouldBe` 4
            median3 4 4 4 `shouldBe` 4
            median3 (-6) (-2) 4 `shouldBe` (-2)
        it "rbgToCmyk" $ do
            rbgToCmyk RGB {red = 255, green = 255, blue = 255} `shouldBe` CMYK {cyan = 0.0, magenta = 0.0, yellow = 0.0, black = 0.0}
            rbgToCmyk RGB {red = 255, green = 0, blue = 0}     `shouldBe` CMYK {cyan = 0.0, magenta = 1.0, yellow = 1.0, black = 0.0}
            rbgToCmyk RGB {red = 0, green = 255, blue = 0}     `shouldBe` CMYK {cyan = 1.0, magenta = 0.0, yellow = 1.0, black = 0.0}
            rbgToCmyk RGB {red = 0, green = 0, blue = 255}     `shouldBe` CMYK {cyan = 1.0, magenta = 1.0, yellow = 0.0, black = 0.0}
            rbgToCmyk RGB {red = 255, green = 255, blue = 0}   `shouldBe` CMYK {cyan = 0.0, magenta = 0.0, yellow = 1.0, black = 0.0}
            rbgToCmyk RGB {red = 255, green = 0, blue = 255}   `shouldBe` CMYK {cyan = 0.0, magenta = 1.0, yellow = 0.0, black = 0.0}
            rbgToCmyk RGB {red = 0, green = 255, blue = 255}   `shouldBe` CMYK {cyan = 1.0, magenta = 0.0, yellow = 0.0, black = 0.0}
            rbgToCmyk RGB {red = 0, green = 0, blue = 0}       `shouldBe` CMYK {cyan = 0.0, magenta = 0.0, yellow = 0.0, black = 1.0}
        it "geomProgression" $ do
            geomProgression 5.0 0.5 2 `shouldBe` 1.25
            geomProgression 5.0 2.0 3 `shouldBe` 40.0
        it "coprime" $ do
            coprime 8 10 `shouldBe` False
            coprime (-8) 10 `shouldBe` False
            coprime 17 19 `shouldBe` True
            coprime 14 27  `shouldBe` True
    describe "lists" $ do
        it "distance" $ do
            distance (Point [0.0, 0.0]) (Point [0.0, 1.0]) `shouldBe` 1.0
            distance (Point [0, 0, 0, 0]) (Point [1, 1, 1, 1]) `shouldBe` 2.0 
            distance (Point []) (Point []) `shouldBe` 0.0 
        it "intersect" $ do
            intersect [0, 2, 3, 4, 6] [1, 3, 5, 7, 9] `shouldBe` [3]
            intersect [3, 3, 3, 3] [3, 3, 3] `shouldBe` [3,3,3,3] 
            intersect [1, 2, 4, 6] [5, 4, 2, 5, 7] `shouldBe` [2, 4]
            intersect [1, 3, 9] [] `shouldBe` []
            intersect [] [5, 7] `shouldBe` []
            intersect [] [] `shouldBe` []
        it "zipN" $ do
            zipN [[1, 2, 3], [4, 5, 6], [7, 8, 9]] `shouldBe` [[1, 4, 7], [2, 5, 8], [3, 6, 9]]
            zipN [[1, 2, 3], [4, 5], [6]] `shouldBe` [[1, 4, 6], [2, 5], [3]]
            zipN [[]] `shouldBe` [] 
        it "find" $ do
            find (> 0) [0 , 0, 0, 4] `shouldBe` Just 4
            find (even) [5, 1, -4, 5] `shouldBe` Just (-4)
            find (< 0) [-1, 3, 7] `shouldBe` Just (-1)
        it "findLast" $ do
            findLast (> 0) [-1, 2, -3, 4] `shouldBe` Just 4
            findLast (< 0) [-1, 2, -3, 4] `shouldBe` Just (-3)
            findLast (even) [-1, 1, -3, 5] `shouldBe` Nothing  
        it "mapFuncs" $ do
            mapFuncs [\x -> x*x, (1 +), \x -> if even x then 1 else 0] 3 `shouldBe` [9, 4, 0]
            mapFuncs [\x -> x*x, (1 +), \x -> if even x then 1 else 0] 3 `shouldBe` [9, 4, 0]
            mapFuncs [\x -> sqrt (-x), abs] (-4) `shouldBe` [2.0,4.0]            
        it "satisfiesAll" $ do
            satisfiesAll [] 8 `shouldBe` True
            satisfiesAll [even, \x -> x `rem` 5 == 0] 10 `shouldBe` True
        it "tailNel" $ do
            tailNel (NEL 4 [3,9]) `shouldBe` [3, 9]
            tailNel (NEL 1 [9]) `shouldBe` [9] 
        it "lastNel" $ do
            lastNel (NEL 1 [2,3]) `shouldBe` 3 
            lastNel (NEL 1 [2]) `shouldBe` 2  
        it "zipNel" $ do
            zipNel (NEL 1 [2,3]) (NEL 1 [2,3]) `shouldBe` NEL (1,1) [(2,2),(3,3)]
            zipNel (NEL 9 [2]) (NEL 9 [5]) `shouldBe` NEL (9,9) [(2,5)] 
            zipNel (NEL 3 []) (NEL 5 []) `shouldBe` NEL (3,5) []   
        it "listToNel" $ do
            listToNel [1,2,3] `shouldBe` (NEL 1 [2,3]) 
            listToNel [1] `shouldBe` (NEL 1 [])
        it "nelToList" $ do
            nelToList (NEL 1 [2,3]) `shouldBe` [1, 2, 3]
            nelToList (NEL 7 []) `shouldBe` [7]  
    describe "luhn" $ do
        it "isLuhnValid" $ do 
            isLuhnValid 56372828181828 `shouldBe` False
            isLuhnValid 1204496404322 `shouldBe` False 
            isLuhnValid 4026843483168683 `shouldBe` True
 