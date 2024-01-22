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

midPoint :: Point -> Point -> Point
midPoint (x1, y1, z1) (x2, y2, z2) = ((x1 + x2) / 2, (y1 + y2) / 2, (z1 + z2) / 2)

sierpinski :: Int -> Shape
sierpinski n = sierpinski' (n - 1) t1
  where
    t1 = ((0, 0, 10), (0, 10, 0), (10, 0, 0))

sierpinski' :: Int -> Triangle -> Shape
sierpinski' 0 t = [t]
sierpinski' k (p1, p2, p3) =
    concatMap
    (\(p1', p2', p3') -> sierpinski' (k - 1) (p1', p2', p3'))
    [ (p1, midPoint p1 p2, midPoint p1 p3),
        (midPoint p1 p2, p2, midPoint p2 p3),
        (midPoint p1 p3, midPoint p2 p3, p3)
    ]

main :: IO ()
main = writeObjModel (sierpinski 3) "part1.stl"
