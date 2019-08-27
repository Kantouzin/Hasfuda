module Game.Hanafuda.Cards
    ( Suit(..)
    , Ribbon(..), Animal(..), Light(..)
    , Month
    , Card
    , cardMonth, cardSuit
    , deck
    ) where

data Suit = Dreg            -- カス
          | Animal Animal   -- タネ
          | Ribbon Ribbon   -- 短冊
          | Light Light     -- 光
          deriving (Show, Eq, Ord)

data Ribbon = Red       -- 短冊
            | Poetry    -- 赤短
            | Blue      -- 青短
            deriving (Show, Eq, Ord)

data Animal = BushWarbler   -- 鶯
            | Cuckoo        -- 不如帰
            | WaterIris     -- 八橋
            | Butterflies   -- 蝶
            | Boar          -- 猪
            | Geese         -- 雁
            | SakeCup       -- 盃
            | Deer          -- 鹿
            | Swallow       -- 燕
            deriving (Show, Eq, Ord)

data Light = CraneAndSun    -- 鶴
           | CampCurtain    -- 幕
           | FullMoon       -- 月
           | Michikaze      -- 小野道風
           | Phoenix        -- 鳳凰
           deriving (Show, Eq, Ord)

type Month = Int

data Card = Card Month Suit
          deriving (Eq, Ord)

instance Show Card where
    show (Card month suit) = showMonth month ++ "に" ++ showSuit suit

cardMonth :: Card -> Month
cardMonth (Card month _) = month

cardSuit :: Card -> Suit
cardSuit (Card _ suit) = suit

showMonth :: Month -> String
showMonth  1 = "松"
showMonth  2 = "梅"
showMonth  3 = "桜"
showMonth  4 = "藤"
showMonth  5 = "菖蒲"
showMonth  6 = "牡丹"
showMonth  7 = "萩"
showMonth  8 = "芒"
showMonth  9 = "菊"
showMonth 10 = "紅葉"
showMonth 11 = "柳"
showMonth 12 = "桐"

showSuit :: Suit -> String
showSuit Dreg = "カス"
showSuit (Ribbon s) = showSuitRibbon s
showSuit (Animal s) = showSuitAnimal s
showSuit (Light s) = showSuitLight s

showSuitRibbon :: Ribbon -> String
showSuitRibbon Red = "短冊"
showSuitRibbon Poetry = "赤短"
showSuitRibbon Blue = "青短"

showSuitAnimal :: Animal -> String
showSuitAnimal BushWarbler = "鶯"
showSuitAnimal Cuckoo = "不如帰"
showSuitAnimal WaterIris = "八橋"
showSuitAnimal Geese = "雁"
showSuitAnimal Swallow = "燕"
showSuitAnimal Butterflies = "蝶"
showSuitAnimal Boar = "猪"
showSuitAnimal Deer = "鹿"
showSuitAnimal SakeCup = "盃"

showSuitLight :: Light -> String
showSuitLight CraneAndSun = "鶴"
showSuitLight CampCurtain = "幕"
showSuitLight FullMoon = "月"
showSuitLight Michikaze = "小野道風"
showSuitLight Phoenix = "鳳凰"

deck :: [Card]
deck =  [ Card  1 suit | suit <- [Dreg, Dreg, Ribbon Poetry, Light CraneAndSun] ]
     ++ [ Card  2 suit | suit <- [Dreg, Dreg, Ribbon Poetry, Animal BushWarbler] ]
     ++ [ Card  3 suit | suit <- [Dreg, Dreg, Ribbon Poetry, Light CampCurtain] ]
     ++ [ Card  4 suit | suit <- [Dreg, Dreg, Ribbon Red, Animal Cuckoo] ]
     ++ [ Card  5 suit | suit <- [Dreg, Dreg, Ribbon Red, Animal WaterIris] ]
     ++ [ Card  6 suit | suit <- [Dreg, Dreg, Ribbon Blue, Animal Butterflies] ]
     ++ [ Card  7 suit | suit <- [Dreg, Dreg, Ribbon Red, Animal Boar] ]
     ++ [ Card  8 suit | suit <- [Dreg, Dreg, Animal Geese, Light FullMoon] ]
     ++ [ Card  9 suit | suit <- [Dreg, Dreg, Ribbon Blue, Animal SakeCup] ]
     ++ [ Card 10 suit | suit <- [Dreg, Dreg, Ribbon Blue, Animal Deer] ]
     ++ [ Card 11 suit | suit <- [Dreg, Ribbon Red, Animal Swallow, Light Michikaze] ]
     ++ [ Card 12 suit | suit <- [Dreg, Dreg, Dreg, Light Phoenix] ]
