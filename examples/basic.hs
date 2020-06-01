toCartesian :: (Double, Double) -> (Double, Double)
toCartesian = \(r, theta) -> let
                               y = r * sin theta
                               x = r * cos theta
                             in (x, y)

toCartesian :: (Double, Double) -> (Double, Double)
toCartesian (r, theta) = (x, y)
  where x = r * cos theta
        y = r * sin theta

