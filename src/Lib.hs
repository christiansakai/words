module Lib
    ( Coord (MkCoord)
    , Cell (MkCell)
    , Grid (MkGrid)
    , coords
    , printCoords
    , charGrid
    , printCharGrid
    , grid
    , languages
    , makeRandomGrid
    , fillInBlanks
    , reverseXGrid
    , reverseYGrid
    , transposeGrid
    , skewGrid
    , skewRow
    , findWordInRow
    , findWordInGrid
    , firstJust
    , findHorizontal
    , findVertical
    , findDiagonal
    , findWord
    , findWords
    , printCellRows
    , Game ( Game
           , gameGrid
           , gameWords
           )
    , makeGame
    , totalWords
    , totalScore
    , playGame
    , completed
    ) where

import Data.List ( transpose
                 , intersperse
                 )
import Data.Maybe ( isJust
                  , catMaybes
                  )
import qualified Data.Map as M
import Data.Char ( toLower )
import System.Random ( RandomGen
                     , randomRs
                     , split
                     )


newtype Coord = MkCoord (Integer, Integer)

instance Show Coord where
  -- show :: Coord -> String
  show (MkCoord (x, y)) = "(" ++ (show x) ++ ", " ++ (show y) ++ ")"

instance Eq Coord where
  -- (==) :: Coord -> Coord -> Bool
  (MkCoord (i1, j1)) == (MkCoord (i2, j2)) = i1 == i2 && j1 == j2


newtype Cell = MkCell (Coord, Char)

instance Show Cell where
  -- show :: Cell -> String
  show (MkCell (coord, char)) = "(" ++ (show coord) ++ ", '" ++ [char] ++ "')"

instance Eq Cell where
  -- (==) :: Cell -> Cell -> Bool
  (MkCell (coord1, char1)) == (MkCell (coord2, char2)) =
    coord1 == coord2 && char1 == char2


newtype Grid = MkGrid [[Cell]]

instance Show Grid where
  -- show :: Grid -> String
  show (MkGrid cellGrid) = unlines $ map rowToStr cellGrid
    where
      rowToStr :: [Cell] -> String
      rowToStr cellRow = concat $ map show cellRow  


coords :: [[Coord]]
coords = [[MkCoord (y, x) | x <- [0..14]] | y <- [0..11]]

printCoords :: [[Coord]] -> IO ()
printCoords coords = putStrLn . unlines $ map show coords 


charGrid :: [[Char]]
charGrid = [ "__C________R___"
           , "__SI________U__"
           , "__HASKELL____B_"
           , "__A__A_____S__Y"
           , "__R___B___C____"
           , "__PHP____H_____"
           , "____S_LREP_____"
           , "____I__M_Y__L__"
           , "____L_E__T_O___"
           , "_________HB____"
           , "_________O_____"
           , "________CN_____"
           ]


printCharGrid :: [[Char]] -> IO ()
printCharGrid charGrid = putStrLn . unlines $ charGrid


grid :: Grid
grid = MkGrid $ zipWith zipper coords charGrid    
  where
    zipper :: [Coord] -> [Char] -> [Cell]
    zipper coordRow charRow = zipWith zipper' coordRow charRow
      where
        zipper' :: Coord -> Char -> Cell
        zipper' coord char = MkCell (coord, char)


languages :: [String]
languages = [ "BASIC"
            , "COBOL"
            , "CSHARP"
            , "HASKELL"
            , "LISP"
            , "PERL"
            , "PHP"
            , "PYTHON"
            , "RUBY"
            , "SCHEME"
            ]


makeRandomGrid :: RandomGen g => g -> [[Char]]
makeRandomGrid gen =
  let
    (gen1, gen2) = split gen
    row = randomRs ('A', 'Z') gen1
   in
    row : makeRandomGrid gen2


fillInBlanks :: RandomGen g => g -> Grid -> Grid
fillInBlanks gen (MkGrid grid) = MkGrid $ zipWith zipper randCharGrid grid
  where
    randCharGrid :: [[Char]]
    randCharGrid = makeRandomGrid gen

    zipper :: [Char] -> [Cell] -> [Cell]
    zipper randCharRow cellRow = zipWith zipper' randCharRow cellRow
      where
        zipper' :: Char -> Cell -> Cell
        zipper' randCh (MkCell (coord, '_'))  = MkCell (coord, randCh)
        zipper' randCh cell                   = cell


reverseXGrid :: Grid -> Grid
reverseXGrid (MkGrid grid) = MkGrid (map reverse grid)


reverseYGrid :: Grid -> Grid
reverseYGrid (MkGrid grid) = MkGrid (reverse grid)


transposeGrid :: Grid -> Grid
transposeGrid (MkGrid grid) = MkGrid (transpose grid)


skewGrid :: Grid -> Grid
skewGrid (MkGrid grid) = MkGrid (skew 0 grid [])
  where
    skew :: Integer -> [[Cell]] -> [[Cell]] -> [[Cell]]
    skew n originalGrid skewedGrid
      | n == toInteger (length originalGrid) = skewedGrid
      | otherwise                            =
        skew (n + 1) originalGrid (skewedGrid ++ [(skewRow n (originalGrid !! (fromInteger n)))]) 


skewRow :: Integer -> [Cell] -> [Cell]
skewRow skewBy row = [MkCell (MkCoord (-1, -1), '_') | c <- [0..skewBy - 1]] <> row


findWordInRow :: String -> [Cell] -> Maybe [Cell]
findWordInRow word cellRow = findWordInRow' word cellRow []
  where
    findWordInRow' :: String -> [Cell] -> [Cell] -> Maybe [Cell]
    findWordInRow' "" [] acc = Just . reverse $ acc
    findWordInRow' _ [] _    = Nothing
    findWordInRow' "" _ acc  = Just . reverse $ acc
    findWordInRow' word@(w:ws) (c:cs) acc
      | w == getChar c = findWordInRow' ws cs (c : acc) 
      | otherwise      = findWordInRow' word cs acc

    getChar :: Cell -> Char
    getChar (MkCell (_, c)) = c


findWordInGrid :: Grid -> String -> Maybe [Cell]
findWordInGrid (MkGrid grid) word = firstJust $ map (findWordInRow word) grid


firstJust :: [Maybe a] -> Maybe a
firstJust [] = Nothing 
firstJust (Just(el):ms) = Just el
firstJust (_:ms) = firstJust ms


findHorizontal :: Grid -> String -> Maybe [Cell]
findHorizontal grid word = firstJust [foundLeftRight, foundRightLeft]
  where
    foundLeftRight = findWordInGrid grid word
    foundRightLeft = findWordInGrid (reverseXGrid grid) word  


findVertical :: Grid -> String -> Maybe [Cell]
findVertical grid word = firstJust [foundTopBottom, foundBottomTop]
  where
    foundTopBottom = findWordInGrid (transposeGrid grid) word
    foundBottomTop = findWordInGrid (transposeGrid . reverseYGrid $ grid) word


findDiagonal :: Grid -> String -> Maybe [Cell]
findDiagonal grid word = firstJust [ foundTopRightBottomLeft
                                   , foundBottomLeftTopRight
                                   , foundBottomRightTopLeft
                                   , foundTopLeftBottomRight
                                   ]
  where
    foundTopRightBottomLeft = findWordInGrid (transposeGrid . skewGrid $ grid) word
    foundTopLeftBottomRight = findWordInGrid (transposeGrid . skewGrid . reverseXGrid $ grid) word

    foundBottomRightTopLeft = findWordInGrid (transposeGrid . reverseYGrid . skewGrid . reverseXGrid $ grid) word
    foundBottomLeftTopRight = findWordInGrid (transposeGrid . reverseYGrid . skewGrid $ grid) word


findWord :: Grid -> String -> Maybe [Cell]
findWord grid word = firstJust [foundHorizontal, foundVertical, foundDiagonal]
  where
    foundHorizontal = findHorizontal grid word
    foundVertical = findVertical grid word
    foundDiagonal = findDiagonal grid word


findWords :: Grid -> [String] -> [[Cell]]
findWords grid languages = catMaybes allResult
  where
    allResult :: [Maybe [Cell]]
    allResult = map (findWord grid) languages


printCellRows :: [[Cell]] -> IO ()
printCellRows cellRows = putStrLn . unlines $ map show cellRows


data Game = Game {
    gameGrid :: Grid
  , gameWords :: M.Map String (Maybe [Cell])
  }

instance Show Game where
  -- show :: Game -> String
  show game =
    let
      grid :: Grid
      grid = gameGrid game

      words :: M.Map String (Maybe [Cell])
      words = gameWords game

      currentFoundCells :: [Cell]
      currentFoundCells = concat . catMaybes . M.elems $ words

      lowercaseCell :: Cell -> Cell
      lowercaseCell (MkCell (coord, char)) = MkCell (coord, toLower char)

      getCells :: Grid -> [[Cell]]
      getCells (MkGrid cells) = cells

      gridFormattedWithFoundWords :: Grid
      gridFormattedWithFoundWords = MkGrid $ formatted
        where
          formatted :: [[Cell]]
          formatted = map formatRow (getCells grid)
            where
              formatRow :: [Cell] -> [Cell]
              formatRow row = map formatCell row 
                where
                  formatCell :: Cell -> Cell
                  formatCell cell
                    | cell `elem` currentFoundCells = cell
                    | otherwise                     = lowercaseCell cell

      charGrid :: [[Char]]
      charGrid = map removeCoordFromRow (getCells gridFormattedWithFoundWords)
        where
          removeCoordFromRow :: [Cell] -> [Char]
          removeCoordFromRow cellRow = map removeCoordFromCol cellRow

          removeCoordFromCol :: Cell -> Char
          removeCoordFromCol (MkCell (_, ch)) = ch

      charGridSpaced :: [[Char]]
      charGridSpaced = map formatRow charGrid
        where
          formatRow :: [Char] -> [Char]
          formatRow row = intersperse ' ' row

      showCharGrid :: String
      showCharGrid = unlines charGridSpaced

      showScoreTotal :: String
      showScoreTotal = "Score/Total: " ++ (show . totalScore $ game) ++ "/" ++ (show . totalWords $ game) 

    in
      showCharGrid ++ "\n" ++ showScoreTotal


makeGame :: Grid -> [String] -> Game
makeGame grid languages = Game {
    gameGrid = grid
  , gameWords = M.fromList (map (\l -> (l, Nothing)) languages)
  }


totalWords :: Game -> Integer
totalWords game = toInteger . length . M.keys $ gameWords game


totalScore :: Game -> Integer
totalScore game = toInteger . length . catMaybes . M.elems $ gameWords game


completed :: Game -> Bool
completed game = totalWords game == totalScore game


playGame :: Game -> String -> Game
playGame game word =
  let
    words :: M.Map String (Maybe [Cell])
    words = gameWords game

    grid :: Grid
    grid = gameGrid game
  in
    case M.member word words of
      False -> game
      True ->
        let
          found :: Maybe [Cell]
          found = findWord grid word
        in
          Game {
              gameGrid = grid
            , gameWords = M.insert word found words
          }
