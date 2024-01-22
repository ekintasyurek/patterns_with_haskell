type Point = (Float, Float, Float)

type Triangle = (Point, Point, Point)

type Shape = [Triangle]

createTriangleDef :: Triangle -> String
createTriangleDef ((x1, y1, z1), (x2, y2, z2), (x3, y3, z3)) =
  "  facet\n"
    ++ "    outer loop\n"
    ++ "      vertex "
    ++ (show x1)
    ++ " "
    ++ (show y1)
    ++ " "
    ++ (show z1)
    ++ "\n"
    ++ "      vertex "
    ++ (show x2)
    ++ " "
    ++ (show y2)
    ++ " "
    ++ (show z2)
    ++ "\n"
    ++ "      vertex "
    ++ (show x3)
    ++ " "
    ++ (show y3)
    ++ " "
    ++ (show z3)
    ++ "\n"
    ++ "    endloop\n"
    ++ "  endfacet\n"

createObjectModelString :: Shape -> String
createObjectModelString n = "solid Object01\n" ++ concatMap createTriangleDef n ++ "endsolid Object01"

writeObjModel :: Shape -> String -> IO ()
writeObjModel x filename = writeFile filename (createObjectModelString x)

createCube :: Point -> Float -> Shape
createCube (cx, cy, cz) sideLength =
  let halfSide = sideLength / 2
      points = [(cx + halfSide, cy + halfSide, cz + halfSide),
                (cx + halfSide, cy + halfSide, cz - halfSide),
                (cx + halfSide, cy - halfSide, cz + halfSide),
                (cx + halfSide, cy - halfSide, cz - halfSide),
                (cx - halfSide, cy + halfSide, cz + halfSide),
                (cx - halfSide, cy + halfSide, cz - halfSide),
                (cx - halfSide, cy - halfSide, cz + halfSide),
                (cx - halfSide, cy - halfSide, cz - halfSide)]
  in createCubeTriangles points

createCubeTriangles :: [Point] -> Shape
createCubeTriangles points =
  [ (points !! 0, points !! 1, points !! 2),
    (points !! 1, points !! 3, points !! 2),
    (points !! 4, points !! 5, points !! 6),
    (points !! 5, points !! 7, points !! 6),
    (points !! 0, points !! 1, points !! 5),
    (points !! 0, points !! 5, points !! 4),
    (points !! 2, points !! 3, points !! 7),
    (points !! 2, points !! 7, points !! 6),
    (points !! 0, points !! 2, points !! 6),
    (points !! 0, points !! 6, points !! 4),
    (points !! 1, points !! 3, points !! 7),
    (points !! 1, points !! 7, points !! 5)
  ]

main :: IO ()
main = writeObjModel (createCube (0, 0, 0) 1) "part3.stl"
