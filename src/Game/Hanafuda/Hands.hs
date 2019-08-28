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

type CapturedCard = [Card]

goko :: CapturedCard -> Maybe Int
goko = undefined

ameshiko :: CapturedCard -> Maybe Int
ameshiko = undefined

shiko :: CapturedCard -> Maybe Int
shiko = undefined

sanko :: CapturedCard -> Maybe Int
sanko = undefined

inoshikacho :: CapturedCard -> Maybe Int
inoshikacho = undefined

akatan :: CapturedCard -> Maybe Int
akatan = undefined

aotan :: CapturedCard -> Maybe Int
aotan = undefined

tsukimizake :: CapturedCard -> Maybe Int
tsukimizake = undefined

hanamizake :: CapturedCard -> Maybe Int
hanamizake = undefined

tanzaku :: CapturedCard -> Maybe Int
tanzaku = undefined

tane :: CapturedCard -> Maybe Int
tane = undefined

kasu :: CapturedCard -> Maybe Int
kasu = undefined
