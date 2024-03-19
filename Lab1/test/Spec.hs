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
        it "median3" $ do
            median3 1 2 3 `shouldBe` 2
	    median3 1 2 2 `shouldBe` 2
	    median3 2 2 2 `shouldBe` 2
	    median3 3 2 1 `shouldBe` 2
	    median3 5 3 4 `shouldBe` 4
	    median3 4 3 5 `shouldBe` 4
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
            geomProgression 1.0 0.5 2 `shouldBe` 0.25
            geomProgression 1.0 2.0 3 `shouldBe` 8.0
        it "coprime" $ do
            coprime 2 7 `shouldBe` True
            coprime (-2) 7 `shouldBe` True
            coprime 2 4 `shouldBe` False
            coprime (-2) 4  `shouldBe` False
    describe "lists" $ do
        it "distance" $ do
            distance (Point [0.0, 0.0]) (Point [0.0, 1.0]) `shouldBe` 1.0
            distance (Point [0, 0, 0, 0]) (Point [1, 1, 1, 1]) `shouldBe` 2.0 
            distance (Point []) (Point []) `shouldBe` 0.0 
        it "intersect" $ do
            intersect [1, 2, 3] [1, 2, 3] `shouldBe` [1, 2, 3]
            intersect [1, 2, 3] [3, 4, 5] `shouldBe` [3] 
            intersect [1, 2] [3, 4] `shouldBe` []
            intersect [1, 2] [] `shouldBe` []
            intersect [] [1, 2] `shouldBe` []
            intersect [] [] `shouldBe` []
        it "zipN" $ do
            zipN [[1, 2, 3], [4, 5, 6], [7, 8, 9]] `shouldBe` [[1, 4, 7], [2, 5, 8], [3, 6, 9]]
            zipN [[1, 2, 3], [4, 5], [6]] `shouldBe` [[1, 4, 6], [2, 5], [3]]
            zipN [[]] shouldBe []
        it "find" $ do
            find (> 0) [0, 1] `shouldBe` Just 1
            find (even) [1, 2, 3, 4] `shouldBe` Just 2
            find (< 0) [-1, 0] `shouldBe` Just (-1)
        it "findLast" $ do
            findLast (> 0) [-1, 0, 1] `shouldBe` Just 1
            findLast (< 0) [-1, -2, -3, -4] `shouldBe` Just (-4)
            findLast (even) [-1, 1, -3, 3] `shouldBe` Nothing  
        it "mapFuncs" $ do
            mapFuncs [\x -> x*x, \x -> x - 1] 2 `shouldBe` [4, 1]
            mapFuncs [abs] (-9) `shouldBe` [9.0]
        it "satisfiesAll" $ do
            satisfiesAll [] 1 `shouldBe` True
            satisfiesAll [even, \x -> x rem 5 == 0] 10 `shouldBe` True
        it "lastNel" $ do
            lastNel (NEL 1 [2,3]) `shouldBe` 3 
            lastNel (NEL 1 [2]) `shouldBe` 2
        it "zipNel" $ do
            zipNel (NEL 1 [2,3]) (NEL 1 [2,3]) `shouldBe` NEL (1,1) [(2,2),(3,3)]
            zipNel (NEL 1 [2]) (NEL 3 [4]) `shouldBe` NEL (1,3) [(2,4)] 
            zipNel (NEL 1 []) (NEL 2 []) `shouldBe` NEL (1,2) []
        it "listToNel" $ do
            listToNel [1,2,3] `shouldBe` (NEL 1 [2,3]) 
            listToNel [1] `shouldBe` (NEL 1 [])
        it "nelToList" $ do
            nelToList (NEL 1 [2,3]) `shouldBe` [1, 2, 3]
            nelToList (NEL 1 []) `shouldBe` [1]  
    describe "luhn" $ do
        it "intToList" $ do 
            intToList 1 `shouldBe` [1] 
            intToList 123 `shouldBe` [321]
        it "processEven" $ do 
            processEven [] `shouldBe` []
            processEven [1] `shouldBe` [1] 
            processEven [1, 1] `shouldBe` [1, 2]
            processEven [1, 1, 1] `shouldBe` [1, 2, 1]
        it "decreaseLarge" $ do 
            decreaseLarge 1 `shouldBe` 1
            decreaseLarge 18 `shouldBe` 9
        it "decreaseAllLarge" $ do 
            decreaseAllLarge [] `shouldBe` []
            decreaseAllLarge [1] `shouldBe` [1]
            decreaseAllLarge [1, 18, 3] `shouldBe` [1, 9, 3]
        it "isLuhnValid" $ do 
            isLuhnValid 0 `shouldBe` False
            isLuhnValid 1241235125 `shouldBe` False 
            isLuhnValid 4140834708961565 `shouldBe` True
