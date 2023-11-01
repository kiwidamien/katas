import Day03
import Test.Hspec


forest :: [String]
forest = ["..##.......",
	"#...#...#..",
	".#....#..#.",
	"..#.#...#.#",
	".#...##..#.",
	"..#.##.....",
	".#.#.#....#",
	".#........#",
	"#.##...#...",
	"#...##....#",
	".#..#...#.#"]

dividedBy :: Int -> Int -> (Int, Int)
dividedBy x y = (div x y, mod x y)


main :: IO () main = hspec $ do
	describe "Addition" $ do
		it "15 divided by 3 is 5" $ do
     		dividedBy 15 3 `shouldBe` (5, 0)
   		 it "22 divided by 5 is\\ 4 remainder 2" $ do
			dividedBy 22 5 `shouldBe` (4, 2)