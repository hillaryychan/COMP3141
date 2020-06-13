fracTree :: Picture
fracTree = tree 10 (Point 400 800) (Vector 0 (-100)) red

tree :: Int -> Point -> Vector -> Colour -> Picture
tree depth base direction colour
  | depth == 0 = drawLine line
  | otherwise
    =  drawLine line
    ++ tree (depth - 1) nextBase left nextColour -- left tree
    ++ tree (depth - 1) nextBase right nextColour -- left tree
  where
    drawLine :: Line -> Picture
    drawLine (Line start end) =
      [ Path [start, end] colour Solid ]

    line = Line base nextBase
    nextBase = offset direction base

    left = rotate (-pi /12) $ scale 0.8 $ direction
    right = rotate (pi /12) $ scale 0.8 $ direction

    nextColour =
      colour { redC = (redC colour) - 24, blueC = (blueC colour) + 24 }

-- Offset a point by a vector
offset :: Vector -> Point -> Point
offset (Vector vx vy) (Point px py)
  = Point (px + vx) (py + vy)

-- Scale a vector
scale :: Float -> Vector -> Vector
scale factor (Vector x y) = Vector (factor * x) (factor * y)

-- Rotate a vector (in radians)
rotate :: Float -> Vector -> Vector
rotate radians (Vector x y) = Vector xRotated yRotated
  where
    -- As polar
    radius = sqrt $ (x * x) + (y * y)
    theta =
      if radius == 0
        then 0
        else if y >= 0
          then acos $ x / radius
          else - (acos $ x / radius)
    -- Rotate theta
    rotated = theta + radians
    -- Back to cartesian
    xRotated = radius * (cos rotated)
    yRotated = radius * (sin rotated)
