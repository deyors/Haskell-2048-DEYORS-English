---------------------HASKELL-2048-DEYORS-JANUARY-2020---------------------
--------------------------------2048 GAME--------------------------------

{-Instructions: Use WASD keys to move the numbers of your panel in the
right direction. If there are adjacent numbers in that direction and 
the numbers are the same as the numbers you moved, they will sum and 
lead to a bigger number. The objective is to reach 2048! Good luck.-}

--PROGRAMMING:

import Data.List (transpose) --We need to calculate the transpose matrix
import System.Random --To generate random numbers
import Prelude hiding (Left, Right)

data Move = Up | Down | Left | Right
type Panel = [[Int]]

------------------------SHOWING THE PANEL ON THE SCREEN-------------------

{- The first need is, using IO to join all in the central program, to show
a panel in the screen. To reach that:-}

--We show a number:

showElement :: Int -> String
showElement n = show n

{- I give to different ways to represent the panel becouse sometimes 
it's tiring to play with too much design :).-}


--We show a row:

showRow :: [Int] -> String
showRow [] = "|"
showRow (x:xs)
     | (long == 1) = "|    " ++ mE ++ "   " ++ mF
     | (long == 2) = "|   " ++ mE ++ "   " ++ mF
     | (long == 3) = "|   " ++ mE ++ "  " ++ mF
     | (long == 4) = "|  " ++ mE ++ "  " ++ mF
         where long = length (showElement x)
               mE = showElement x
               mF = showRow xs

--(We consider the length of the element to be centered)

showRow2 :: [Int] -> String
showRow2 [] = " "
showRow2 (x:xs)
     | (long == 1) = "     " ++ mE ++ "   " ++ mF
     | (long == 2) = "    " ++ mE ++ "   " ++ mF
     | (long == 3) = "    " ++ mE ++ "  " ++ mF
     | (long == 4) = "   " ++ mE ++ "  " ++ mF
         where long = length (showElement x)
               mE = showElement x
               mF = showRow2 xs

--We show all the panel:

{- With the first two functions we try to encapsulate up and down all 
the information.-}

rods :: Int -> Char -> String
rods 0 _ = ""
rods n ch = ch:(rods (n-1) ch)

coverUp :: Int -> String
coverUp 0 = "+"
coverUp n = "+" ++ (rods 8 '-') ++ (coverUp (n-1))

showPanel :: Panel -> IO ()
showPanel [x] = do 
         putStrLn $ coverUp (length x)
         putStrLn (showRow x)
         putStrLn $ coverUp (length x)
showPanel (x:xs) = do
         putStrLn (coverUp (length x))
         putStrLn (showRow x)
         showPanel (xs)

showPanel2 :: Panel -> IO ()
showPanel2 [x] = do 
         putStrLn "   "
         putStrLn (showRow2 x)
         putStrLn "   "
showPanel2 (x:xs) = do
         putStrLn "   "
         putStrLn (showRow2 x)
         showPanel2 (xs)

--(The first showPanel function is the design mode)

--------------------------REACTION TO MOVEMENT----------------------------

{-The objective is, given a panel, to apply the WASD movement. To reach
that, I'll define a Move type element, and every time I'll aplly the 
movement to the panel, it will return another panel with the movement
applied.-}

{-I'll start by the Left Move, and I'll be focused in one row. That row
will be a list where every two adjacent elements or elements separated by 
zeroes will sum or move to the left side leaving empty spaces. That 
spaces will be filled by zeroes.-}

{- joinToLeft, given a list, will add recursively to another list the 
sum of the adjacent element or separated by zero, alongside to all
the elements that cannot be summed, in an orderly way.-}

joinToLeft :: [Int] -> [Int]
joinToLeft [] = []
joinToLeft [x] = [x]
joinToLeft (x:y:xs)
     | (x == 0) = joinToLeft(y:xs)
     | (y == 0) = joinToLeft(x:xs)
     | (x == y) = (x + y):joinToLeft(xs)
     | otherwise = x:joinToLeft(y:xs)

{- toLeft will return the elements of joinToLeft adding zeroes to 
the right side. This will make no different between the dimensions
of the panel.-}

toLeft :: [Int] -> [Int]
toLeft xs = (joinToLeft xs) ++ ceros
     where ceros = replicate (length xs - length (joinToLeft xs)) 0

{-Now it's time to generalize this with all the moves. I'll define 
a funcion that, given a Move element, returns a function that accepts
a Panel and returns that Panel applying the move.

Moving a panel to the right side is like putting all the rows backwards
and moving all to the left, so we can take advantage of the functions 
above.

To move the panel up and down I'll have to take the original one and 
calculate the transposed panel. Every column will be a row now, so 
using the Right Move is like using the Up Move, and Left Move is like 
using the Down Move.-}

imove :: Move -> Panel -> Panel
imove Left = map toLeft
imove Right = map (reverse . toLeft . reverse)
imove Up = transpose . imove Left . transpose
imove Down = transpose . imove Right . transpose

{-Now it's time to link the WASD keys with the moves. The next function
receives the keys and returns a new panel with the right move applied.-}

movementPanel :: Char -> Panel -> Panel
movementPanel x tab
     |(x `elem` "wW") = imove Up tab
     |(x `elem` "aA") = imove Left tab
     |(x `elem` "sS") = imove Down tab
     |(x `elem` "dD") = imove Right tab
     |otherwise = tab

---------------------ADDING NEW ELEMENTS TO THE PANEL---------------------

{- After every movement in the panel is updated, the panel will have to 
add two random numbers in a random free space of the panel (filled by zeroes).
The two random numbers can only be "2" or "4".-}

-- 1. COORDINATES:

{-First, I'll need to map all the coordinates of the panel, in the form of
pairs (row,column), according to the panel dimension.-}

{- singleRow, given the dimension of the panel and a number, returns a list.
The length of the list is the dimension of the panel and the elements of the 
list will be the number added.-}

singleRow :: Int -> Int -> [Int]
singleRow dim x = replicate dim (x-1)

{- listFrom, given a number, returns a list. The elements of the list 
will be the number added going through all the previous numbers to zero.-}

listFrom :: Int -> [Int]
listFrom 0 = [0]
listFrom x = (x):listFrom(x-1)

{- listTo, given a number, returns a list. The elements of the list 
will be zero going through all the next numbers until reaching the added
number.-}

listTo :: Int -> [Int]
listTo x = reverse (tail (listFrom x))

{- coord1 is an auxiliary function that, given the dimension of the panel 
and a number, returns all the possible combinations of the coordinates of
the panel starting by zero until the previous number of the number inserted.-}

coord1 :: Int -> Int -> [(Int,Int)]
coord1 dim 1 = zip (singleRow dim 1) (listTo dim)
coord1 dim x = coordInitial ++ coordFinal
     where coordFinal = zip (singleRow dim x) (listTo dim)
           coordInitial = coord1 dim (x-1)

{- coordinates, given a panel, returns a list of pairs with all the possible 
coordinates of the panel, in order.-}

coordinates :: Panel -> [(Int,Int)]
coordinates tab = coord1 (length tab) (length tab)

{- We will add random numbers on panel numbers that are zero,
therefore we will have to identify the coordinates of said
numbers:-}

giveZeroes :: Panel -> [(Int, Int)]
giveZeroes tab = filter filterZeroes (coordinates tab)
     where filterZeroes (row,col) = (tab!!row)!!col == 0

{- changeValue is a function that given a Panel, some coordinates and a
value, it is able to change the old value of that element by a new one-}

changeValue :: Panel -> (Int,Int) -> Int -> Panel
changeValue tab (row,col) val = left ++ [med] ++ right
     where left = take row tab
           med = left1 ++ [val] ++ right1
                 where left1 = take col (tab!!row)
                       right1 = drop (col + 1) (tab!!row)
           right = drop (row + 1) tab

{- (First moves the rows aside to focus on the row of the coordinate,
and then in that row separate the elements to one side and the other of the
coordinate to focus on the item to be changed)-}

-- 2. ADD RANDOM NUMBERS:

{- To generate random numbers we will use "choose". This function asks you
a list with elements and it returns a random element from said list,
but within the IO monad.

This is achieved thanks to randomRIO, a function of System.Random that
takes a minimum and maximum number and returns a random number between
those numbers using the global random number seed from the
computer. This seed is automatically generated with data apparently
random such as time, time the computer has been on, keys
pulsed, etc.-}

choose :: [a] -> IO a
choose xs = do
    i <- randomRIO (0, length xs-1)
    return (xs !! i)

-- 3. PUT RANDOM NUMBERS INTO ZEROES ON THE PANEL:

{-The insertRandom function takes a Panel, finds all zeros and
put in a list, take a random item from that list, take another
random element from a list of twos and fours and substitute that zero
for the chosen value.-}

insertRandom :: Panel -> IO Panel
insertRandom tab = do
    let candidates = giveZeroes tab
    candidatoQueQuiero <- choose candidates
    val  <- choose [2,2,2,2,2,2,2,2,4,4]
    let nuevoTab = changeValue tab candidatoQueQuiero val
    return nuevoTab

{-Normally a four tends to annoy and make it more tedious to put together
numbers, so we put more twos than fours.-}

-----------------------WHEN WE WIN OR LOSE--------------------------

{-iWin gives True if any Panel element is 2048. We do it
pulling out all Panel elements with concat and filtering.-}

iWin :: Panel -> Bool
iWin tab
     |(filter (== 2048) (concat tab) == []) = False
     |otherwise = True

{-iLose is True when all Panel movements leave it
like this, that is, there are no more zeros or movements available-}

iLose :: Panel -> Bool
iLose tab
     |(w && a && s && d) = True
     |otherwise = False
         where w = (imove Up tab == tab)
               a = (imove Left tab == tab)
               s = (imove Down tab == tab)
               d = (imove Right tab == tab)

---------------------------MAIN AND THE GAME ITSELF---------------------

{-We create the first Panel, putting two numbers at random to a Panel
empty.-}

emptyPanel :: Int -> Panel
emptyPanel dim = replicate dim (replicate dim 0)

firstPanel :: Int -> IO Panel
firstPanel dim = do
     tab' <- insertRandom (emptyPanel dim)
     insertRandom tab'

{-nextPanel, given a Panel, ask you for a character and with that character,
updates the Panel according to the address you have inserted..-}

nextPanel :: Panel -> IO Panel
nextPanel tab = do
     ch <- getChar
     if ch `elem` "mM" then error "You have left the game."
         else do 
             let newTab = movementPanel ch tab
             return newTab

{-According to the chosen modality (simple or with design), we will make two games,
and in each one the Panel will be accepted, it will be verified if it has been won
or lost, an address will be requested and the Panel will be changed
correspondent.-}

game :: Panel -> IO ()
game tab
     |iWin tab = do
             showPanel tab
             putStrLn "YOU WON!"
     |iLose tab = do
             showPanel tab
             putStrLn "The game is over, you've lost."
     |otherwise = do
             putStrLn "\n"
             showPanel tab
             newTab <- nextPanel tab
             if tab /= newTab then do
                                     newTab2 <- insertRandom newTab
                                     game newTab2
                                else do
                                     putStrLn phrase
                                     game tab
     where phrase = "\nTry another key, the Panel remains the same"

game2 :: Panel -> IO ()
game2 tab
     |iWin tab = do
             showPanel2 tab
             putStrLn "YOU WON!"
     |iLose tab = do
             showPanel2 tab
             putStrLn "The game is over, you've lost."
     |otherwise = do
             putStrLn "\n"
             showPanel2 tab
             newTab <- nextPanel tab
             if tab /= newTab then do
                                     newTab2 <- insertRandom newTab
                                     game2 newTab2
                                else do
                                     putStrLn phrase
                                     game2 tab
     where phrase = "\nTry another key, the Panel remains the same"

{- We create an auxiliary function that asks the dimension of the Panel
and according to the answer create a firstPanel of that dimension.-}


charToInt :: Char -> Int
charToInt ch = (read [ch] :: Int)


askDim :: IO Panel
askDim = do
     putStrLn "\nChoose the panel dimension:"
     putStrLn "Recommended size: 4"
     ch <- getChar
     let dim = (charToInt ch)
     putStrLn ("\nLoading panel with dimension " ++ [ch] ++ ":")
     firstPanel dim

{- We create an auxiliary function that is in charge of running the game,
asking for the size of the Panel, if we want it simple or with margins 
and starting to play.-}

play :: IO ()
play = do
     putStrLn (phrase1 ++ phrase2)
     ch <- getChar
     if ch `elem` "sS" then do
                         tab <- askDim
                         game2 tab
     else do
         tab <- askDim
         game tab

     where phrase1 = "\nPress S if you want the Panel to be simple"
           phrase2 = ", or another letter if you want the game with margins"

{- We create a helper function that takes care of the instructions-}

instructions :: IO ()
instructions = do
     putStrLn "\nUse the WASD keys to move the numbers on your "
     putStrLn "Panel at the appropriate address. If there are adjacent "
     putStrLn "numbers in that direction that are equal will be added and "
     putStrLn "will lead to a larger number. The goal is to get 2048!"
     putStrLn "Good luck."
     putStrLn "Press S to play..."
     ch <- getChar
     if ch `elem` "sS" then do play else return ()

{- We create a helper function that takes care of the tests-}

immediatePanel :: IO Panel
immediatePanel = return ([[8,1024,4],[1024,1024,1024],[16,1024,16]])

impossiblePanel :: IO Panel
impossiblePanel = return ([[4,32,8,32],[16,64,4,16],[8,2,0,32],[16,32,8,16]])

tests :: IO ()
tests = do 
     putStrLn "\nPress A if you want to solve an immediate Panel"
     putStrLn "Press B if you want to solve an impossible Panel"
     ch <- getChar
     if ch `elem` "aA" then do
                         tab <- immediatePanel
                         game tab
                       else do
                         if ch `elem` "bB" then do
                                             tab <- impossiblePanel
                                             game tab
                                           else return ()

{- We create an auxiliary function that is in charge of filtering the different
game modalities-}

myFilter :: Char -> IO ()
myFilter ch
     |ch `elem` "sS" = do play 
     |ch `elem` "iI" = do instructions
     |ch `elem` "pP" = do tests
     |otherwise = return ()

{-And now it is the turn of the central function, where information will be given
of the game modalities and a response will be requested, applying the
myFilter function to redirect to helper functions-}

main :: IO ()
main = do
     putStrLn "Welcome to 2048."
     putStrLn "Programmed by Deyors."
     putStrLn "January 2020."
     putStrLn "Press S to play."
     putStrLn "Press I for instructions."
     putStrLn "Press P to enter Test mode."
     putStrLn "Press M at any time to exit."
     ch <- getChar
     myFilter ch