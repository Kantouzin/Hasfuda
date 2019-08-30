module Game.Hanafuda.Hands
    ( HanafudaHand(..)
    , goko
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

data HanafudaHand
    = Goko          -- 五光
    | AmeShiko      -- 雨四光
    | Shiko         -- 四光
    | Sanko         -- 三光
    | Inoshikacho   -- 猪鹿蝶
    | Akatan        -- 赤短
    | Aotan         -- 青短
    | TsukimiZake   -- 月見酒
    | HanamiZake    -- 花見酒
    | Tanzaku       -- 短冊
    | Tane          -- タネ
    | Kasu          -- カス
    deriving (Show, Eq, Ord, Enum)

type CapturedCard = [Card]  -- 獲得札
type Score = Int            -- 得点

goko :: CapturedCard -> Maybe (HanafudaHand, Score)
goko capturedCard = do
    score <- if isGoko then Just 10 else Nothing
    return (Goko, score)
    where
        cards = [ card | card@(Card _ (Light _)) <- capturedCard]
        isGoko = length cards == 5

ameshiko :: CapturedCard -> Maybe (HanafudaHand, Score)
ameshiko capturedCard = do
    score <- if isAmeshiko then Just 8 else Nothing
    return (AmeShiko, score)
    where
        cards = [ card | card@(Card _ (Light _)) <- capturedCard]
        isAmeshiko = length cards == 4 && Card 11 (Light Michikaze) `elem` cards

shiko :: CapturedCard -> Maybe (HanafudaHand, Score)
shiko capturedCard = do
    score <- if isShiko then Just 7 else Nothing
    return (Shiko, score)
    where
        cards = [ card | card@(Card _ (Light _)) <- capturedCard ]
        isShiko = length cards == 4 && Card 11 (Light Michikaze) `notElem` cards

sanko :: CapturedCard -> Maybe (HanafudaHand, Score)
sanko capturedCard = do
    score <- if isSanko then Just 5 else Nothing
    return (Sanko, score)
    where
        cards = [ card | card@(Card _ (Light _)) <- capturedCard ]
        isSanko = length cards == 3 && Card 11 (Light Michikaze) `notElem` cards

inoshikacho :: CapturedCard -> Maybe (HanafudaHand, Score)
inoshikacho capturedCard = do
    score <- specificHint capturedCard cards 5
    return (Inoshikacho, score)
    where
        cards = [ Card  6 (Animal Butterflies)
                , Card  7 (Animal Boar)
                , Card 10 (Animal Deer) ]

akatan :: CapturedCard -> Maybe (HanafudaHand, Score)
akatan capturedCard = do
    score <- specificHint capturedCard cards 6
    return (Akatan, score)
    where
        cards = [ Card 1 (Ribbon Poetry)
                , Card 2 (Ribbon Poetry)
                , Card 3 (Ribbon Poetry) ]

aotan :: CapturedCard -> Maybe (HanafudaHand, Score)
aotan capturedCard = do
    score <- specificHint capturedCard cards 6
    return (Aotan, score)
    where
        cards = [ Card  6 (Ribbon Blue)
                , Card  9 (Ribbon Blue)
                , Card 10 (Ribbon Blue) ]

tsukimizake :: CapturedCard -> Maybe (HanafudaHand, Score)
tsukimizake capturedCard = do
    score <- specificHint capturedCard cards 5
    return (TsukimiZake, score)
    where
        cards = [ Card 8 (Light FullMoon)
                , Card 9 (Animal SakeCup) ]

hanamizake :: CapturedCard -> Maybe (HanafudaHand, Score)
hanamizake capturedCard = do
    score <- specificHint capturedCard cards 5
    return (HanamiZake, score)
    where
        cards = [ Card 3 (Light CampCurtain)
                , Card 9 (Animal SakeCup) ]

tanzaku :: CapturedCard -> Maybe (HanafudaHand, Score)
tanzaku capturedCard = do
    score <- anyHint cards 5
    return (Tanzaku, score)
    where
        cards = [ card | card@(Card _ (Ribbon _)) <- capturedCard ]

tane :: CapturedCard -> Maybe (HanafudaHand, Score)
tane capturedCard = do
    score <- anyHint cards 5
    return (Tane, score)
    where
        cards = [ card | card@(Card _ (Animal _)) <- capturedCard ]

kasu :: CapturedCard -> Maybe (HanafudaHand, Score)
kasu capturedCard = do
    score <- anyHint cards 10
    return (Kasu, score)
    where
        cards = [ card | card@(Card _ Dreg) <- capturedCard ]

anyHint :: CapturedCard -> Int -> Maybe Score
anyHint capturedCard n =
    if length capturedCard >= n
       then Just $ 1 + length capturedCard - n
       else Nothing

specificHint :: CapturedCard -> [Card] -> Score -> Maybe Score
specificHint capturedCard cards score =
    if and [ card `elem` capturedCard | card <- cards ]
       then Just score
       else Nothing
