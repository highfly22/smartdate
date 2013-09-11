module DateDict
       where

import AUG

next :: [Gloss]
next = [ toGloss (\c -> (xsucc :: EnumBox -> EnumBox)) ]

-- monday :: [Gloss]
-- monday = [ toGloss (\c -> ())]
