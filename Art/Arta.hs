module Art where

import ShapeGraphics
import Codec.Picture

art :: Picture
art = tree 10 (Point 400 800) (Vector 0 (-80)) (Colour 192 0 0 255)

tree :: Int -> Point -> Vector -> Colour -> Picture
tree depth treeBase treeDirection startColour =
  let
    -- Scale of branches
    branchScale = 0.9
    -- Angle of left branch (radians)
    leftAngle = -0.4
    -- Angle of right branch (radians)
    rightAngle = 0.2
    -- Change in color for each iteration
    colourChange = Colour 0 0 24 255

    recursiveFractal :: Int -> Point -> Vector -> Colour -> [PictureObject]
    recursiveFractal 0 _ _ _ = []
    recursiveFractal depth base direction colour =
      [lineToPath (vectorLine base direction) colour Solid]
      ++ recursiveFractal (depth - 1) topOfRoot leftDirection branchColour
      ++ recursiveFractal (depth - 1) topOfRoot rightDirection branchColour
      where
        topOfRoot = movePoint base direction
        leftDirection =
          scaleVector branchScale $ rotateVector leftAngle direction
        rightDirection =
          scaleVector branchScale $ rotateVector rightAngle direction
        branchColour = addColour colour colourChange
  in
    recursiveFractal depth treeBase treeDirection startColour

-- Produce a line by drawing a vector from a point
vectorLine :: Point -> Vector -> Line
vectorLine base vector = Line base $ movePoint base vector

-- Produce a picture object from a line
lineToPath :: Line -> Colour -> LineStyle -> PictureObject
lineToPath (Line start end) = Path [start, end]

-- Scale a vector by a given factor
scaleVector :: Float -> Vector -> Vector
scaleVector factor (Vector x y) = Vector (factor * x) (factor * y)

-- Rotate a vector by a given angle (in radians)
rotateVector :: Float -> Vector -> Vector
rotateVector angle (Vector x y) = Vector x' y'
  where
    x' = x * (cos angle) - y * (sin angle)
    y' = y * (cos angle) + x * (sin angle)

-- Offset a point using a vector for difference between points
movePoint :: Point -> Vector -> Point
movePoint (Point x y) (Vector dx dy)
  = Point (x + dx) (y + dy)

addColour :: Colour -> Colour -> Colour
addColour (Colour lr lg lb lo) (Colour rr rg rb ro) =
  Colour (mix lr rr) (mix lg rg) (mix lb rb) (mix lo ro)
  where
    mix a b = min 255 (a + b)

writeToFile pic
  = writePng "art.png" (drawPicture 3 art)
