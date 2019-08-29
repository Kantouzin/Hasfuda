module Game.Hanafuda.Hands
    ( goko
    , ameshiko
    , shiko
    , sanko
    , inoshikacho
    , akatan
    , aotan
    , tsukimizake
    , hanamizake
    , tanzaku
    , tane
    , kasu
    ) where

import Game.Hanafuda.Cards

type CapturedCard = [Card]  -- 獲得札
type Score = Int            -- 得点

goko :: CapturedCard -> Maybe Score
goko capturedCard = do
    score <- if isGoko then Just 10 else Nothing
    return score
    where
        cards = [ card | card@(Card _ (Light _)) <- capturedCard]
        isGoko = length cards == 5

ameshiko :: CapturedCard -> Maybe Score
ameshiko capturedCard = do
    score <- if isAmeshiko then Just 8 else Nothing
    return score
    where
        cards = [ card | card@(Card _ (Light _)) <- capturedCard]
        isAmeshiko = length cards == 4 && Card 11 (Light Michikaze) `elem` cards

shiko :: CapturedCard -> Maybe Score
shiko capturedCard = do
    score <- if isShiko then Just 7 else Nothing
    return score
    where
        cards = [ card | card@(Card _ (Light _)) <- capturedCard ]
        isShiko = length cards == 4 && Card 11 (Light Michikaze) `notElem` cards

sanko :: CapturedCard -> Maybe Score
sanko capturedCard = do
    score <- if isSanko then Just 5 else Nothing
    return score
    where
        cards = [ card | card@(Card _ (Light _)) <- capturedCard ]
        isSanko = length cards == 3 && Card 11 (Light Michikaze) `notElem` cards

inoshikacho :: CapturedCard -> Maybe Score
inoshikacho capturedCard = do
    score <- specificHint capturedCard cards 5
    return score
    where
        cards = [ Card  6 (Animal Butterflies)
                , Card  7 (Animal Boar)
                , Card 10 (Animal Deer) ]

akatan :: CapturedCard -> Maybe Score
akatan capturedCard = do
    score <- specificHint capturedCard cards 6
    return score
    where
        cards = [ Card 1 (Ribbon Poetry)
                , Card 2 (Ribbon Poetry)
                , Card 3 (Ribbon Poetry) ]

aotan :: CapturedCard -> Maybe Score
aotan capturedCard = do
    score <- specificHint capturedCard cards 6
    return score
    where
        cards = [ Card  6 (Ribbon Blue)
                , Card  9 (Ribbon Blue)
                , Card 10 (Ribbon Blue) ]

tsukimizake :: CapturedCard -> Maybe Score
tsukimizake capturedCard = do
    score <- specificHint capturedCard cards 5
    return score
    where
        cards = [ Card 8 (Light FullMoon)
                , Card 9 (Animal SakeCup) ]

hanamizake :: CapturedCard -> Maybe Score
hanamizake capturedCard = do
    score <- specificHint capturedCard cards 5
    return score
    where
        cards = [ Card 3 (Light CampCurtain)
                , Card 9 (Animal SakeCup) ]

tanzaku :: CapturedCard -> Maybe Score
tanzaku capturedCard = do
    score <- anyHint cards 5
    return score
    where
        cards = [ card | card@(Card _ (Ribbon _)) <- capturedCard ]

tane :: CapturedCard -> Maybe Score
tane capturedCard = do
    score <- anyHint cards 5
    return score
    where
        cards = [ card | card@(Card _ (Animal _)) <- capturedCard ]

kasu :: CapturedCard -> Maybe Score
kasu capturedCard = do
    score <- anyHint cards 10
    return score
    where
        cards = [ card | card@(Card _ Dreg) <- capturedCard ]

anyHint :: CapturedCard -> Int -> Maybe Score
anyHint capturedCard n =
    if capturedCard /= []
       then Just $ if length capturedCard >= n then 1 + length capturedCard - n else 0
       else Nothing

specificHint :: CapturedCard -> [Card] -> Score -> Maybe Score
specificHint capturedCard cards score =
    if and [ card `elem` capturedCard | card <- cards ]
       then Just score
       else Nothing
