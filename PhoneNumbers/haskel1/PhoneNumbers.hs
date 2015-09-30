module PhoneNumbers where
  
import Data.List

-- Rule:
-- A phonelist is consistent if the phonelist contains none of that phonelist's prefixes
isConsistent :: [String] -> Bool
isConsistent  phonelist = (phonelist`contains`) `noneOf` (phonelist >>= prefixes)
   where 
     contains = flip elem
     noneOf pred list = not $ any pred list

-- Generates a list of all prefixes of a String
-- inits "abc" == ["","a","ab","abc"]
-- prefixes "abc" == ["a","ab"]
prefixes = tail . init . inits

-- ["abc", "xyz"] >>= prefixes 
-- results in ["a","ab","x","xy"]
-- Which is really mapping prefixes over a phonelist and then flattening it
-- concat $ map prefixes ["abc", "xyz"]
-- In my previous version is aliased '>>=' to 's' to make it read as "a phonelist's prefixes"
