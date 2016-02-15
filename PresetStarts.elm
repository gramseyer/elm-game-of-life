module PresetStarts (blinker, gliderGun) where

import GameOfLife exposing (..)

--taken from wikipedia

blinker = fromListList [[False, True, False], [False, True, False], [False, True, False]]

gliderGun = fromListList [
        [False, False, False, False, False, False, False, False, False, False, False, False, False],
        [False, False, False, False, False, False, False, False, False, False, False, False, False],
        [False, False, False, False, False, False, False, False, False, False, False, False, False],
        [False, False, False, False, False, True,  True,  False, False, False, False, False, False],
        [False, False, False, False, False, True,  True,  False, False, False, False, False, False],
        [False, False, False, False, False, False, False, False, False, False, False, False, False],
        [False, False, False, False, False, False, False, False, False, False, False, False, False],

        [False, False, False, False, False, False, False, False, False, False, False, False, False],
        [False, False, False, False, False, False, False, False, False, False, False, False, False],
        [False, False, False, False, False, False, False, False, False, False, False, False, False],
        [False, False, False, False, False, False, False, False, False, False, False, False, False],
        [False, False, False, False, False, False, False, False, False, False, False, False, False],

        [False, False, False, False, False, False, False, False, False, False, False, False, False],
        [False, False, False, False, False, True,  True,  True,  False, False, False, False, False],
        [False, False, False, False, True,  False, False, False, True,  False, False, False, False],
        [False, False, False, True,  False, False, False, False, False, True,  False, False, False],
        [False, False, False, True,  False, False, False, False, False, True,  False, False, False],

        [False, False, False, False, False, False, True,  False, False, False, False, False, False],
        [False, False, False, False, True,  False, False, False, True,  False, False, False, False],
        [False, False, False, False, False, True,  True,  True,  False, False, False, False, False],
        [False, False, False, False, False, False, True,  False, False, False, False, False, False],
        [False, False, False, False, False, False, False, False, False, False, False, False, False],

        [False, False, False, False, False, False, False, False, False, False, False, False, False],
        [False, False, False, True,  True,  True,  False, False, False, False, False, False, False],
        [False, False, False, True,  True,  True,  False, False, False, False, False, False, False],
        [False, False, True,  False, False, False, True,  False, False, False, False, False, False],
        [False, False, False, False, False, False, False, False, False, False, False, False, False],

        [False, True,  True,  False, False, False, True,  True,  False, False, False, False, False],
        [False, False, False, False, False, False, False, False, False, False, False, False, False],
        [False, False, False, False, False, False, False, False, False, False, False, False, False],
        [False, False, False, False, False, False, False, False, False, False, False, False, False],
        [False, False, False, False, False, False, False, False, False, False, False, False, False],
        [False, False, False, False, False, False, False, False, False, False, False, False, False],
        [False, False, False, False, False, False, False, False, False, False, False, False, False],
        [False, False, False, False, False, False, False, False, False, False, False, False, False],
        [False, False, False, False, False, False, False, False, False, False, False, False, False],
        [False, False, False, False, False, False, False, False, False, False, False, False, False],
        [False, False, False, True,  True,  False, False, False, False, False, False, False, False],
        [False, False, False, True,  True,  False, False, False, False, False, False, False, False],
        [False, False, False, False, False, False, False, False, False, False, False, False, False],
        [False, False, False, False, False, False, False, False, False, False, False, False, False]
        [False, False, False, False, False, False, False, False, False, False, False, False, False]

    ]

