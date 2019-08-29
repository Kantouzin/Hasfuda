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
goko capturedCard =
    if length cards == 5
       then Just 10 else Nothing
    where
        cards = [ card | card@(Card _ (Light _)) <- capturedCard]

ameshiko :: CapturedCard -> Maybe Score
ameshiko capturedCard =
    if length cards == 4 && Card 11 (Light Michikaze) `elem` cards
       then Just 8 else Nothing
    where
        cards = [ card | card@(Card _ (Light _)) <- capturedCard]

shiko :: CapturedCard -> Maybe Score
shiko capturedCard =
    if length cards == 4 && Card 11 (Light Michikaze) `notElem` cards
       then Just 7 else Nothing
    where
        cards = [ card | card@(Card _ (Light _)) <- capturedCard ]

sanko :: CapturedCard -> Maybe Score
sanko capturedCard =
    if length cards == 3 && Card 11 (Light Michikaze) `notElem` cards
       then Just 5 else Nothing
    where
        cards = [ card | card@(Card _ (Light _)) <- capturedCard ]

inoshikacho :: CapturedCard -> Maybe Score
inoshikacho capturedCard =
    if specificHint capturedCard cards then Just 5 else Nothing
    where
        cards = [ Card  6 (Animal Butterflies)
                , Card  7 (Animal Boar)
                , Card 10 (Animal Deer) ]

akatan :: CapturedCard -> Maybe Score
akatan capturedCard =
    if specificHint capturedCard cards then Just 6 else Nothing
    where
        cards = [ Card 1 (Ribbon Poetry)
                , Card 2 (Ribbon Poetry)
                , Card 3 (Ribbon Poetry) ]

aotan :: CapturedCard -> Maybe Score
aotan capturedCard =
    if specificHint capturedCard cards then Just 6 else Nothing
    where
        cards = [ Card  6 (Ribbon Blue)
                , Card  9 (Ribbon Blue)
                , Card 10 (Ribbon Blue) ]

tsukimizake :: CapturedCard -> Maybe Score
tsukimizake capturedCard =
    if specificHint capturedCard cards then Just 5 else Nothing
    where
        cards = [ Card 8 (Light FullMoon)
                , Card 9 (Animal SakeCup) ]

hanamizake :: CapturedCard -> Maybe Score
hanamizake capturedCard =
    if specificHint capturedCard cards then Just 5 else Nothing
    where
        cards = [ Card 3 (Light CampCurtain)
                , Card 9 (Animal SakeCup) ]

tanzaku :: CapturedCard -> Maybe Score
tanzaku capturedCard =
    anyHint cards 5
    where
        cards = [ card | card@(Card _ (Ribbon _)) <- capturedCard ]

tane :: CapturedCard -> Maybe Score
tane capturedCard =
    anyHint cards 5
    where
        cards = [ card | card@(Card _ (Animal _)) <- capturedCard ]

kasu :: CapturedCard -> Maybe Score
kasu capturedCard =
    anyHint cards 10
    where
        cards = [ card | card@(Card _ Dreg) <- capturedCard ]

anyHint :: CapturedCard -> Int -> Maybe Int
anyHint capturedCard n =
    if capturedCard /= []
       then Just $ if length capturedCard >= n then 1 + length capturedCard - n else 0
       else Nothing

specificHint :: CapturedCard -> [Card] -> Bool
specificHint capturedCard cards =
    and [ card `elem` capturedCard | card <- cards ]
