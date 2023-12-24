module Matrix
  ( transpose,
  )
where

import Data.Vector as V
import Parse

transpose :: Matrix -> Matrix
transpose v =
  V.fromList
    [ V.fromList
        [ (v V.! col) V.! row
          | col <- [0 .. maxRow]
        ]
      | row <- [0 .. maxCol]
    ]
  where
    maxRow = V.length v - 1
    maxCol = V.length (v V.! 0) - 1