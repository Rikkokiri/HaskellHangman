-- IMPORTS --
import System.IO
import Data.List
import Data.Char
import Control.Monad


-- Predefined word the computer is asking the player to guess
goalword :: String
goalword = "hangman"


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


-- | Print the message of how many guesses are left
guessesLeftMsg :: Int -> String
guessesLeftMsg guessNumber = ("\nYou have " ++ (show (maxGuesses - (succ guessNumber))) ++ " guesses left\n")


-- | Guessing. If the length of @param guess == 1, then 
takeAGuess :: String -> String -> String
takeAGuess guess word
    | length guess > 1 = guessWord guess
    | otherwise = guessChar (head guess) goalword word


-- | Guessing the entire word. If the guess is right, return the right word. Else return "Unfortunately..."
guessWord :: String -> String
guessWord guess
    | guess == goalword = goalword
    | otherwise = "Unfortunately your guess was wrong"


-- | Check if a given character is in the string
-- | @param guess, goal word, partly hidden word
guessChar :: Char -> String -> String -> String
guessChar guess [] [] = ""
guessChar guess (x:xs) (y:ys)
    -- Letters match
    | guess == x = (charToString x) ++ guessChar guess xs ys
    | elem y ['a'..'z'] = (charToString y) ++ guessChar guess xs ys
    | otherwise = "*" ++ guessChar guess xs ys


-- | 
charToString :: Char -> String
charToString x = [x]


-- |
printCharArray :: [Char] -> String
printCharArray [] = ""
printCharArray [x] = (charToString x)
printCharArray (x:xs) = (charToString x) ++ ", " ++ (printCharArray xs)


--| Start the game
hangman = do
    -- TODO Tell player the rules of the game
    putStrLn "    H A N G M A N   G A M E"
    putStrLn "==============================="
    putStrLn "I'm thinking of a word and you must guess what it is. You may guess either letters or if you're daring, the entire word."
    putStrLn "If you guess wrong, you'll get one step closer to death. The ninth failure will kill you. Good luck!"

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
    when (letters /= [])
        (print("Guessed letters: " ++ printCharArray letters))

    putStr "Guess a letter or the entire word: "
    guess <- getLine


    -- Check if guess was a character and if that character has already been guessed
    -- if so, prompt the player to guess again
    if (length guess == 1) && (elem (head guess) letters) then do
        putStrLn "You have already guessed that! Guess again!"
        gameplay guessNumber word letters

        
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
                putStrLn guessesLeftMsg

                if(length guess == 1) then
                    gameplay (succ guessNumber) result (letters ++ guess)
                else
                    gameplay (succ guessNumber) result letters
