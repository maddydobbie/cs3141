module Ex01 where

-- needed to display the picture in the playground
import Codec.Picture

-- our line graphics programming interface
import ShapeGraphics

-- Part 1
mapToPoints :: [(Float, Float)] -> [Point]
mapToPoints coords = map (\(a,b) -> Point a b) coords

-- picture of a house
housePic :: Picture
housePic = [door, house]
  where
    house :: PictureObject
    house = Path (mapToPoints houseCOs) green Solid
    door :: PictureObject
    door  = Path (mapToPoints doorCOs) red Solid

-- these are the coordinates - convert them to a list of Point
houseCOs :: [(Float, Float)]
houseCOs = [(300, 750), (300, 450), (270, 450), (500, 200),
         (730, 450), (700, 450), (700, 750)]

doorCOs :: [(Float, Float)]
doorCOs = [(550, 750), (550, 550), (650, 550), (650, 750)]

windowCOs :: [(Float, Float)]
windowCOs = [(350,650), (350,550), (450,550), (450,650), (350,650)]

cyan :: Colour
cyan = Colour 96 192 255 255

window :: PictureObject
window = Path (mapToPoints windowCOs) cyan Solid

chimneyHouseCOs :: [(Float, Float)]
chimneyHouseCOs = [(300, 750), (300, 450), (270, 450), (500, 200),(615,325),(615,250),(650,250),(650,363),(730, 450), (700, 450), (700, 750)]

chimneyHouse :: Picture
chimneyHouse = [chimney, door, window]
  where
    chimney :: PictureObject
    chimney = Path (mapToPoints chimneyHouseCOs) green Solid
    door :: PictureObject
    door = Path (mapToPoints doorCOs) red Solid
    window :: PictureObject
    window = Path (mapToPoints windowCOs) cyan Solid


-- Part 2
movePoint :: Vector -> Point -> Point
movePoint (Vector xv yv) (Point x y) = Point (x + xv) (y + yv)

movePoints :: Vector -> [Point] -> [Point]
movePoints vec p = map (movePoint vec) p

movePictureObject :: Vector -> PictureObject -> PictureObject
movePictureObject vec (Path p colour lineStyle) = Path (movePoints vec p) colour lineStyle

-- add other cases
movePictureObject vec (Circle c r colour lineStyle fillStyle)
  = Circle (movePoint vec c) r colour lineStyle fillStyle

movePictureObject vec (Ellipse c w h rot colour lineStyle fillStyle)
  = Ellipse (movePoint vec c) w h rot colour lineStyle fillStyle

movePictureObject vec (Polygon p colour lineStyle fillStyle)
  = Polygon (movePoints vec p) colour lineStyle fillStyle


-- Part 3


-- generate the picture consisting of circles:
-- [Circle (Point 400 400) (400/n) col Solid SolidFill,
--  Circle (Point 400 400) 2 * (400/n) col Solid SolidFill,
--  ....
--  Circle (Point 400 400) 400 col Solid SolidFill]
simpleCirclePic :: Colour -> Float -> Picture
simpleCirclePic col n = map (circlePic col n) [1..n]

circlePic :: Colour -> Float -> Float -> PictureObject
circlePic col n i = Circle (Point 400 400) (i * (400/n)) col Solid SolidFill
 
-- use 'writeToFile' to write a picture to file "ex01.png" to test your
-- program if you are not using Haskell for Mac
-- e.g., call
-- writeToFile [house, door]

writeToFile pic
  = writePng "ex01.png" (drawPicture 3 pic)
