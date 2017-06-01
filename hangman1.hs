-- Game structure
-- 
-- 1. Get the word the player needs to guess
--    (I will use a predefined word for now)
-- 
-- 2. Convert the word to a string of stars
-- 
-- 3. Let the player guess (letter or the whole word)
--      (Print them list of chars they've already guessed)     
-- 
--      3.1 If player gives one character => guessing a character
--          3.1.1 If player has already guessed that character
-- 
--      3.2 If player gives string longer than 1 char, they are
--          guessing the whole word
--          3.2.1 Just compare the word

-- IMPORTS --
import System.IO
import Data.List
import Data.Char
import Control.Monad

-- Predefined word
goalword = "hangman"


-- The guessed word
guessedWord = (hideWord)



-- Start the game
hangman = do    
    -- TODO Tell player the rules of the game
    print("This is the Hangman game")

    -- Cover the letters in the goal word
    let toBeGuessed = (hideWord goalword)
    let guessedLetters = [] :: [Char]

    print("Guess the word: " ++ toBeGuessed)

    -- Move to guessing the word
    gameplay 0 toBeGuessed guessedLetters



-- | Run the actual gameplay
gameplay :: Int -> String -> [Char] -> IO ()
gameplay guessNumber word letters = do

    -- Show the player the letters they've guessed already
    when (letters /= []) (print("Guessed letters: " ++ printCharArray letters))

    putStr "Guess a letter or the entire word: "
    guess <- getLine

    print("You guessed: " ++ guess) -- REMOVE


    -- Check if guess was a character and if that character has already been guessed
    -- if so, prompt the player to guess again
    if (length guess == 1) && (checkIfGuessed (head guess) letters) then do
        putStrLn "You have already guessed that! Guess again!"
        gameplay guessNumber word letters
        
    -- If the guess is new, hangle the guess
    else do

        -- Call the guessing function
        let result = (takeAGuess guess word)

        if result == goalword then
            putStrLn "That's correct! YOU WIN!"
            -- THE GAME ENDS. PLAYER WINS.

        -- If guess is wrong...
        else do

            -- Print the current state of the word
            putStrLn result

            -- Print the drawing
            let drawing = drawHangman (succ guessNumber)
            putStrLn drawing

            -- If all guesses have been used, the game ends
            if (succ guessNumber) >= 8 then
                putStrLn "GAME OVER"

            -- If there are guesses left, continue the game
            else do
                putStrLn ("\nYou have " ++ (show (7 - (succ guessNumber))) ++ " guesses left\n")

                if(length guess == 1) then
                    gameplay (succ guessNumber) result (letters ++ guess)
                else
                    gameplay (succ guessNumber) result letters


-- | 
charToString :: Char -> String
charToString x = [x]


-- | 
hideWord :: String -> String
hideWord [] = ""
hideWord (x:xs) = "*" ++ (hideWord xs)


-- | (NOT USED)
printLetters :: [Char] -> String
printLetters [] = ""
printLetters (x:xs) = (charToString x) ++ ", " ++ (printLetters xs)

-- | (NOT USED)
printCharArray1 :: [Char] -> String
printCharArray1 array
    | array == [] = ""
    | length array == 1 = (charToString (head array))
    | otherwise = (charToString (head array)) ++ ", " ++ (printCharArray1 (tail array))


printCharArray :: [Char] -> String
printCharArray [] = ""
printCharArray [x] = (charToString x)
printCharArray (x:xs) = (charToString x) ++ ", " ++ (printCharArray xs)

-- | 
takeAGuess :: String -> String -> String
takeAGuess guess word
    | length guess == 1 = guessChar (head guess) goalword word
    | otherwise = guessWord guess


-- | Guessing the entire word. If the guess is right, return the right word. Else return "Unfortunately..."
guessWord :: String -> String
guessWord guess
    | guess == goalword = goalword
    | otherwise = "Unfortunately your guess was wrong"


-- | Check if the letter was guessed already
checkIfGuessed :: Char -> [Char] -> Bool
checkIfGuessed c [] = False
checkIfGuessed c (x:xs)
    | x == c = True
    | otherwise = checkIfGuessed c xs


-- | Check if a given character is in the string
-- | guess, goal word, partly hidden word
guessChar :: Char -> String -> String -> String
guessChar guess [] [] = ""
guessChar guess (x:xs) (y:ys)
    -- Letters match
    | guess == x = (charToString x) ++ guessChar guess xs ys
    | y `elem` ['a'..'z'] = (charToString y) ++ guessChar guess xs ys
    | otherwise = "*" ++ guessChar guess xs ys



-- | Draw the hangman based on how many guesses have been used
drawHangman :: Int -> String
drawHangman guess
    -- The ground
    | guess == 1 = "\n" ++ "\n" ++ "\n" ++ "\n" ++ "\n" ++ "\n" ++ "============"
    -- The post
    | guess == 2 = "\n" ++ "  |\n" ++ "  |\n" ++ "  |\n" ++ "  |\n" ++ "  |\n" ++ "============"
    -- The bar
    | guess == 3 = "  ._______\n" ++ "  |\n" ++ "  |\n" ++ "  |\n" ++ "  |\n" ++ "  |\n" ++ "============"
    -- The rope
    | guess == 4 = "  ._______\n" ++ "  |      |\n" ++ "  |\n" ++ "  |\n" ++ "  |\n" ++ "  |\n" ++ "============"
    -- The head
    | guess == 5 = "  ._______\n" ++ "  |      |\n" ++ "  |      O\n" ++ "  |\n" ++ "  |\n" ++ "  |\n" ++ "============"
    -- The torso
    | guess == 6 = "  ._______\n" ++ "  |      |\n" ++ "  |      O\n" ++ "  |      |\n" ++ "  |\n" ++ "  |\n" ++ "============" 
    -- Hands
    | guess == 7 = "  ._______\n" ++ "  |      |\n" ++ "  |      O\n" ++ "  |     /|\\\n" ++ "  |\n" ++ "  |\n" ++ "============" 
    -- Legs => YOU LOSE!
    | otherwise = "  ._______\n" ++ "  |      |\n" ++ "  |      O\n" ++ "  |     /|\\\n" ++ "  |     / \\\n" ++ "  |       \n" ++ "============"

