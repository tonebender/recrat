{-# LANGUAGE OverloadedStrings #-}

module Album (
    Album
    , getAndPrintAlbumRatings
    , getAlbumRatings
) where

import Options.Applicative ((<|>))
import qualified Text.Parsec as P
import qualified Data.Text as T
import Data.Text.Internal (Text)
import Text.Read (readMaybe)
import Data.Maybe (catMaybes)

import Wiki (parseInfobox, findInfoboxProperty, wikiLabel)

data Album = Album
    { albumName :: Text
    , ratings :: [Rating]
    } deriving (Show)

-- Type for one review
data Rating = Rating
    { ratio :: Double
    , score :: Double
    , maxScore :: Double
    , title :: Text
    , ref :: Text
    } deriving (Show)

-- TODO: Handle errors better!
getAlbumRatings :: Text -> IO (Album)
getAlbumRatings wikip = do
    case findInfoboxProperty "name" (parseInfobox wikip) of
        Nothing -> return $ Album "No album name found" []  -- Not ideal
        Just artistName -> do
            case P.parse musicRatingsParser "(music ratings parser)" wikip of
                Right reviews -> return $ Album (wikiLabel artistName) $ catMaybes reviews
                Left err -> return $ Album (T.pack (show err)) []  -- Not ideal

printRatingsMaybe :: [Maybe Rating] -> IO ()
printRatingsMaybe [] = return ()
printRatingsMaybe (Nothing:xs) = do
    putStrLn "-Nothing-"
    printRatingsMaybe xs
printRatingsMaybe (Just x:xs) = do
    putStrLn $ show (title x) ++ ": " ++ show (ratio x)
    printRatingsMaybe xs

-- TODO: Not finished...
-- TODO: Get title from actual page (use parser for this?)

-- Get the album ratings from a wikipedia page text and display the average score
getAndPrintAlbumRatings :: Text -> Text -> IO ()
getAndPrintAlbumRatings wikip albumTitle = do
    -- wikip <- readFile "mock_rubber.txt"
    let rev = P.parse musicRatingsParser "(source)" wikip
    case rev of
        Right r -> do
            print albumTitle
            printRatingsMaybe $ r
            putStrLn $ show (length (catMaybes r)) ++ " ratings"
            putStrLn $ "Average score: " ++ (show $ getAverageScore $ catMaybes r)
        Left err -> putStrLn $ "Parse error:" ++ show err

--    wikip <- readFile "mock_rubber.txt"
--    let ratingsBlock = getRatingsBlockInAlbumPage $ T.pack wikip
--    case ratingsBlock of
--         Nothing -> do putStrLn "Could not extract Music/Album ratings from wiki page. Perhaps there are none?"
--         Just rats -> do
--             let rev = P.parse musicRatingsParser "(source)" rats
--             case rev of
--                 Right r -> printRatingsMaybe $ r
--                 Left err -> putStrLn $ "Parse error:" ++ show err


-- TODO: Check presence of ratings block?
--    if T.isInfixOf music wikiText then

-- Parser for the Music/Album ratings block, retrieving each review and ignoring other lines,
-- returning Maybe Rating for the reviews, and Nothing for ignored lines, putting everything into a
-- list. Note that for now only reviews (including ref tags when found) and title are saved; stuff like
-- aggregate reviews are skipped.
musicRatingsParser :: P.Parsec Text () [Maybe Rating]
musicRatingsParser = do
    _ <- P.manyTill P.anyChar ((P.try (P.string "{{Music ratings\n")) <|> (P.try (P.string "{{Album ratings\n")))
    -- TODO: Check that we actually got one of the template strings before getting revs
    revs <- P.manyTill (P.try reviewParser <|> (P.manyTill P.anyChar P.endOfLine >> return Nothing)) (P.string "}}\n")
    -- TODO: Remove the Maybes with catMaybes before returning
    return revs

-- Parser for a review in the Music/Album ratings block, consisting of "| rev3 = [[Allmusic]]\n| rev3Score = ...",
-- where the ensuing score is parsed by any of the three score parsers below.
reviewParser :: P.Parsec Text () (Maybe Rating)
reviewParser = do
    P.char '|' >> P.spaces >> P.string "rev" >> P.many1 P.digit >> P.spaces >> P.char '=' >> P.spaces
    -- In the title, '' around it are optional, but [[ ]] is not
    title' <- P.optional (P.string "''") *> P.string "[[" *> P.manyTill P.anyChar (P.try (P.string "|" <|> P.string "]]")) <* P.manyTill P.anyChar (P.string "\n")
    P.char '|' >> P.spaces >> P.string "rev" >> P.many1 P.digit >> (P.string "Score" <|> P.string "score") >> P.spaces >> P.char '=' >> P.spaces
    (scr, maxScr) <- scoreInRatingTemplParser <|> scoreAsFragmentParser <|> scoreAsLetterParser
    _ <- P.optional noteParser
    reftag <- (P.try refParser) <|> refSingle <|> (P.string "\n")
    case (scr, maxScr) of
        (Just scr', Just maxScr') -> return $ Just $ Rating (scr' / maxScr') scr' maxScr' (T.pack title') (T.pack reftag)
        (_, _) -> return Nothing

-- Parser for scores that look like this: {{Rating|3.5|5}}
scoreInRatingTemplParser :: P.Parsec Text () (Maybe Double, Maybe Double)
scoreInRatingTemplParser = do
    _ <- P.try (P.string "{{Rating|") <|> (P.string "{{rating|")
    scr <- P.many1 (P.digit <|> P.char '.')
    _ <- P.char '|'
    mx <- P.many1 P.digit
    _ <- P.string "}}"
    return (readMaybe scr, readMaybe mx)

-- Parser for scores that look like this: 5.5/10
scoreAsFragmentParser :: P.Parsec Text () (Maybe Double, Maybe Double)
scoreAsFragmentParser = do
    scr <- P.many1 $ P.digit <|> P.char '.'
    _ <- P.char '/'
    mx <- P.many1 $ P.digit <|> P.char '.'
    return (readMaybe scr, readMaybe mx)

-- Parser for letter scores, where E- to D+ are translated to 1
-- and C- to A+ becomes 2 to 10.
scoreAsLetterParser :: P.Parsec Text () (Maybe Double, Maybe Double)
scoreAsLetterParser = do
    letter <- P.oneOf "ABCDE"
    sign <- P.optionMaybe (P.oneOf "+-−")
    let s = case sign of
         Just '+' -> 1
         Just _ -> (-1)
         Nothing -> 0
    let scr = case letter of
         'E' -> 1
         'D' -> 1
         'C' -> 3 + s
         'B' -> 6 + s
         'A' -> 9 + s
         _ -> 0
    return (Just scr, Just 10)

-- Parser that gets everything inside <ref></ref> that follows most scores
refParser :: P.Parsec Text () String
refParser = P.string "<ref" *> (P.string ">" <|> P.manyTill P.anyChar (P.char '>')) *> P.manyTill P.anyChar (P.string "</ref>") <* P.endOfLine

-- Parser for single ref elements, <ref />
refSingle :: P.Parsec Text () String
refSingle = (P.string "<ref") *> P.manyTill P.anyChar (P.try (P.string "/>")) <* P.endOfLine

-- Parser for note such as {{sfn|Graff|Durchholz|1999|p=88}}
noteParser :: P.Parsec Text () String
noteParser = P.string "{{" *> P.manyTill P.anyChar (P.string "}}") 

getAverageScore :: [Rating] -> Double
getAverageScore scores = (sum [ratio s | s <- scores]) / (fromIntegral (length scores))
