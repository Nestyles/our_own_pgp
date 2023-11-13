module Matrix (getMatrix, Matrix, Vector) where
import Data.List ( transpose )
import Data.Word (Word8)

type Vector = [Word8]
type Matrix = [[Word8]]

numRows :: Matrix -> Int
numRows = length

numColumns :: Matrix -> Int
numColumns = length . head

multiplyVector :: Vector -> Word8 -> Vector
multiplyVector vec coef = [ coef * x | x <- vec ]

multiplyMatrix :: Matrix -> Word8 -> Matrix
multiplyMatrix matrix coef = [ multiplyVector row coef | row <- matrix ]

sumVectors :: Vector -> Vector -> Vector
sumVectors = zipWith (+)

sumMatrices :: Matrix -> Matrix -> Matrix
sumMatrices = zipWith sumVectors

dotProduct :: Vector -> Vector -> Word8
dotProduct v w = sum ( zipWith (*) v w )

multiplyMatrices :: Matrix -> Matrix -> Matrix
multiplyMatrices first second = [ map (dotProduct row) (transpose second) | row <- first ]

powMatrix :: Matrix -> Word8 -> Matrix
powMatrix matrix 0 = matrix
powMatrix matrix 1 = matrix
powMatrix matrix pow = powMatrix (multiplyMatrices matrix matrix) (pow - 1)

getMatrix :: [Word8] -> Int -> [[Word8]]
getMatrix [] _ = []
getMatrix args n = take n args : getMatrix (drop n args) n