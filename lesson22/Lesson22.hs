module Lesson22 where

import qualified Data.Map as M

quotes = M.fromList $
    [ (1, "Quote 1")
    , (2, "Quote 2")
    , (3, "Quote 3")
    , (4, "Quote 4")
    , (5, "Quote 5") ]

quotePrompt = "Pick number: "
continuePrompt = "Continue? "

main = do
    print prompt
    selected <- read . getContent
    if selected >= 1 && selected <= 5
        then M.lookup selected
        else 