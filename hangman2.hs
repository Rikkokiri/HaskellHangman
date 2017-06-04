-- IMPORTS --
import System.IO
import Data.List
import Data.Char
import Control.Monad


-- Predefined word the computer is asking the player to guess
goalword :: String
goalword = "hangman"

-- | Rules of the game
rules :: String
rules = "    H A N G M A N   G A M E \n===============================\nI'm thinking of a word and you must guess what it is. You may guess either guesses or if you're daring, the entire word.\nIf you guess wrong, you'll get one step closer to death. The ninth failure will kill you. Good luck!\n"

-- | 
hideWord :: String -> String
hideWord [] = ""
hideWord (x:xs) = "*" ++ (hideWord xs)


-- | The hangman drawing in 8 steps   
drawing :: [[String]]
drawing = [
    -- The ground
    ["\n", "\n", "\n", "\n", "\n", "\n", "============"],
    -- Pole added
    ["\n", "  |\n", "  |\n", "  |\n", "  |\n", "  |\n", "============"],
    -- Bar added
    ["  ._______\n", "  |\n", "  |\n", "  |\n", "  |\n", "  |\n", "============"],
    -- Rope added
    ["  ._______\n", "  |      |\n", "  |\n", "  |\n", "  |\n", "  |\n", "============"],
    -- Head added
    ["  ._______\n", "  |      |\n", "  |      O\n", "  |\n", "  |\n", "  |\n", "============"],
    -- Torso added
    ["  ._______\n", "  |      |\n", "  |      O\n", "  |      |\n", "  |\n", "  |\n", "============"],
    -- Hands added
    ["  ._______\n", "  |      |\n", "  |      O\n", "  |     /|\\\n", "  |\n", "  |\n", "============"], 
    -- Legs added, drawing complete
    ["  ._______\n", "  |      |\n", "  |      O\n", "  |     /|\\\n", "  |     / \\\n", "  |       \n", "============"]]


-- | Draw the hangman drawing based on the provided number of wrong guesses
drawHangman :: Int -> String
drawHangman wrongGuesses
    | wrongGuesses <= 6 = (intercalate "" (drawing !! wrongGuesses))
    | otherwise = (intercalate " " (drawing !! 7))


-- | The maximum number of available guesses
maxGuesses :: Int
maxGuesses = length drawing


-- | Print the message of how many times the player can still guess wrong before dying
guessesLeftMsg :: Int -> String
guessesLeftMsg guessNumber = ("You have " ++ (show (maxGuesses - (succ guessNumber))) ++ " wrong guesses left")


{- |
    Guessing.
    If @param guess is longer than 1 character, call the player wants to guess a word.
    Otherwise the player is guessing a character.

    @param word is the partially solved word
-}
takeAGuess :: String -> String -> String
takeAGuess guess word
    | length guess > 1 = guessWord guess
    | otherwise = guessChar (head guess) goalword word


-- | Guessing the entire word. If the guess is right, return the right word. Else return a message about guessing wrong.
guessWord :: String -> String
guessWord guess
    | guess == goalword = goalword
    | otherwise = "Unfortunately your guess " ++ guess ++ " was wrong"


{- | Check if a given character is in the string
     @param guess  - The guessed character
     @param (x:xs) - The correct word
     @param (y:ys) - Partially solved word
-}
guessChar :: Char -> String -> String -> String
guessChar guess [] [] = ""
guessChar guess (x:xs) (y:ys)
    | guess == x = (charToString x) ++ guessChar guess xs ys
    | elem y ['a'..'z'] = (charToString y) ++ guessChar guess xs ys
    | otherwise = "*" ++ guessChar guess xs ys


-- | Convert a char to a string
charToString :: Char -> String
charToString x = [x]


-- | Convert a string to lower case
lowerString :: String -> String
lowerString s = map toLower s


validLetter :: Char -> Bool
validLetter l
    | elem l ['A'..'Z'] = True
    | elem l ['a'..'z'] = True
    | otherwise = False


-- | Print a list separated with commas
printArray :: [Char] -> String
printArray [] = ""
printArray [x] = (charToString x)
printArray (x:xs) = (charToString x) ++ ", " ++ (printArray xs)


-- | Start the game
hangman = do

    putStrLn rules

    -- Cover the guesses in the goal word
    let toBeGuessed = (hideWord goalword)
    --    let guessedguesses = [] :: [Char]

    print("Guess the word: " ++ toBeGuessed)

    -- Move to guessing the word
    gameplay 0 toBeGuessed []



validateGuess :: String -> [String] -> String
validateGuess guess guesses
    -- Check that guessed char was a letter
    | (length guess == 1) && not (validLetter (head guess)) = "Your guess " ++ guess ++ " is a not a valid (a-z) letter. Guess again!"
    -- Check if the given guess has already been guessed
    | (elem (lowerString guess) guesses) = "You have already guessed that! Guess again!"
    | otherwise = ""


-- | Run the actual gameplay
gameplay :: Int -> String -> [String] -> IO ()
gameplay guessNumber word guesses = do

    -- Show the player the guesses they've guessed already
    when (guesses /= [])
        (print("Previous guesses: " ++ (intercalate ", " guesses)))

    putStr "Guess a letter or the entire word: "
    guess <- getLine

    -- Check that char was a letter and that the same thing wasn't guessed before
    let validationResult = validateGuess guess guesses

    if validationResult /= "" then do
        putStrLn validationResult
        gameplay guessNumber word guesses
        
    -- If the guess is new, hangle the guess
    else do
        
        -- Call the guessing function
        let result = (takeAGuess guess word)

        -- Print the current state of the word
        putStrLn ("\nTHE WORD: " ++ result)

        if result == goalword then
            putStrLn "That's correct! YOU WIN!"
            -- THE GAME ENDS. PLAYER WINS.

        -- If guess is wrong...
        else do
            -- Print the drawing
            let drawing = drawHangman (succ guessNumber)
            putStrLn drawing

            -- If all guesses have been used, the game ends
            if (succ guessNumber) >= 9 then
                putStrLn "GAME OVER"

            -- If there are guesses left, continue the game
            else do
                putStrLn (guessesLeftMsg guessNumber)
                gameplay (succ guessNumber) result (guesses ++ [lowerString guess])

