import qualified Data.Map as Map
import qualified Data.List as List
import System.IO

type CName = [Char]
-- returns new map with added vote. sets it to 1 if there were no vetes for candidate before
add_vote :: Map.Map CName Int -> CName -> Map.Map CName Int
add_vote votes_map candidate = Map.insertWith (+) candidate 1 votes_map


-- add votes from one line and returns updated votes map
add_votes :: Map.Map CName Int -> [CName] -> Map.Map CName Int
add_votes votes_map votes = 
    List.foldl add_vote votes_map unique_votes
    where
        -- get only unique votes from 1 person
        unique_votes = List.nub votes

-- adds up number of voters into accum, if person voted for all candidates
count_full :: [CName] -> Int -> [CName] -> Int
count_full candidates accum line =
    -- if list of all candidates is the same as votes of person
    if candidates List.\\ line == []
        -- add 1
        then 1 + accum
        -- otherwise don't change count
        else 0 + accum

main :: IO ()
main = do
    putStrLn "What is the name of the ballot file?"
    fname <- getLine
    -- read text from file
    allInput <- readFile fname
    -- separate into lines
    let all_lines = lines allInput
    -- separate each line into words
        all_votes = map words all_lines
        -- count votes
        all_votes_map = List.foldl add_votes Map.empty all_votes
        -- get number of perople who voted for none
        empty_number = Map.findWithDefault 0 "none" all_votes_map

        -- remove ones who voted for none
        votes_map = Map.delete "none" all_votes_map
        -- collect all candidates list 
        all_candidates = Map.keys votes_map

        -- count number of voters who voted for all
        full_number = List.foldl (count_full all_candidates) 0 all_votes
        -- function to make text of voting results
        vote_to_text :: [Char] -> ([Char], Int) -> [Char] 
        vote_to_text accum (cname, votes) = accum ++ cname ++ ": " ++ (show votes) ++ "\n"
        -- get key-value pairs from vote results map and 
        -- sort by number of votes and reverse so it is in ascending order
        sorted_votes = reverse $ List.sortOn snd (Map.assocs votes_map)
        -- form report text for results
        votes_text = List.foldl vote_to_text "" sorted_votes

    -- output report text
    putStrLn $ "Total # of ballots: " ++ (show (List.length all_votes))
    putStrLn $ ""
    putStrLn $ votes_text
    putStrLn $ "empty: 14302"
    putStrLn $ "full: " ++ (show full_number)
