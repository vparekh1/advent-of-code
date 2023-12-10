import Text.Parsec
import Text.Parsec.ByteString (Parser, parseFromFile)
import Data.Graph
import Data.Maybe as Maybe
import Data.HashSet as S
import Data.Tuple.Extra

main :: IO ()
main = do
  f <- parseFromFile aocFile "aoc10.txt"
  case f of
    Left err -> print err
    Right e -> do
        putStrLn "AOC10 Answer 1:"
        let dimension = Prelude.maximum $ (\x -> fst x) <$> e
            firstS = Prelude.filter (\x -> S == (snd x)) e !! 0
            (graph, nodeFromVertex, vertexFromKey) = pipeGraph e
            canReach = fromJust $ reachable graph <$> vertexFromKey (fst firstS)
        print $ ceiling $ (/2) $ fromIntegral $ length canReach
        putStrLn "AOC10 Answer 2:"
        let nodes = nodeFromVertex <$> canReach
            bif = buildInteriorFlood dimension Plus (fst firstS) $ (\x -> (fst3 x, snd3 x)) <$> nodes
            (graph2, nodeFromVertex2, vertexFromKey2) = graphFromEdges bif
            interiorAndPath = snd3 . nodeFromVertex2 <$> (fromJust $ reachable graph2 <$> vertexFromKey2 (snd3 $ nodes !! 1))
            path = (\x -> snd3 x) <$> nodes
        let diff = (S.fromList interiorAndPath) `S.difference` (S.fromList path)
        print $ length $ diff

data Direction = Plus | Minus deriving (Eq, Show)
type Node = (Pipe, Point, [Point])
type Point = (Int, Int) 
data Pipe = NS | EW | NE | NW | SW | SE | G | S deriving (Show, Eq, Ord)

switch :: Direction -> Direction
switch d = case d of
    Plus -> Minus
    Minus -> Plus

runCircuit :: Point -> Direction -> Point -> [(Pipe, Point)] -> [Node]
runCircuit dims direction prev [] = []
runCircuit dims direction prev ((pipe, point):xs) = 
    let (newDir, node) = connections2 (fst dims) direction prev (pipe, point)
    in node : (runCircuit dims newDir point xs)

buildInteriorFlood :: Point -> Direction -> Point -> [(Pipe, Point)] -> [Node]
buildInteriorFlood dimensions dir firstPoint nodeList = 
    let circuitWall = runCircuit dimensions dir firstPoint nodeList
        pointSet = S.fromList $ snd <$> nodeList
        remaining = S.fromList $ [(x,y) | x <- [0..(fst dimensions)], y <- [0..(snd dimensions)]]
        allGrounds = runCircuit dimensions dir firstPoint $ (\x -> (G, x)) <$> (S.toList $ remaining `S.difference` pointSet)
    in allGrounds ++ circuitWall

isAbove :: Point -> Point -> Bool
isAbove (x1,x2) (y1,y2) = x2 < y2

isRightOf :: Point -> Point -> Bool
isRightOf (x1,x2) (y1,y2) = y1 > x1

connections2 :: Int -> Direction -> Point -> (Pipe, Point) -> (Direction, Node)
connections2 maxi dir prev (pipe, point) =
    let n = north maxi
        s = south maxi
        e = east maxi
        w = west maxi
        newDir = case pipe of 
            NE -> switch dir
            SW -> switch dir
            _ -> dir
        keyList = case pipe of 
            NS -> (if dir == Plus then e else w) : [n,s]
            EW -> (if dir == Plus then s else n) : [e,w]
            NE -> if (dir == Plus && prev `isRightOf` point)
                        || (dir == Minus && prev `isAbove` point) then [n,s,e,w] else [n,e]
            NW -> (if dir == Plus then [n,s,e,w] else [n,w])
            SW -> if (dir == Plus && point `isAbove` prev)
                        || (dir == Minus && point `isRightOf` prev) then [n,s,e,w] else [s,w]
            SE -> (if dir == Minus then [n,s,e,w] else [s,e])
            G -> [n,s,e,w]
            S -> []
    in (newDir, (pipe, point, Maybe.catMaybes $ ($ point) <$> keyList))

pipeGraph :: [(Point, Pipe)] -> (Graph, Vertex -> (Pipe, Point, [Point]), Point -> Maybe Vertex)
pipeGraph xs = 
    let conn = connections (Prelude.maximum $ (\x -> fst (fst x)) <$> xs)
    in graphFromEdges $ conn <$> xs

north :: Int -> Point -> Maybe Point
north maxi point 
    | (snd point) - 1 < 0 = Nothing
    | otherwise = Just (fst point, max 0 ((snd point) - 1))

south :: Int -> Point -> Maybe Point 
south maxi point 
    | (snd point) + 1 > maxi = Nothing
    | otherwise = Just (fst point, min maxi $ (snd point) + 1)

east :: Int -> Point -> Maybe Point 
east maxi point
    | (fst point) + 1 > maxi = Nothing
    | otherwise = Just (min maxi $ (fst point) + 1, snd point)

west :: Int -> Point -> Maybe Point 
west maxi point
    | (fst point) - 1 < 0 = Nothing
    | otherwise = Just (max 0 $ (fst point) - 1, snd point)

connections :: Int -> (Point, Pipe) -> (Pipe, Point, [Point])
connections maxi (point, pipe) =
    let n = north maxi
        s = south maxi
        e = east maxi
        w = west maxi
        keyList = case pipe of 
            NS -> [n,s]
            EW -> [e,w]
            NE -> [n,e]
            NW -> [n,w]
            SW -> [s,w]
            SE -> [s,e]
            G -> []
            S -> [n,s,w,e]
    in (pipe, point, Maybe.catMaybes $ ($ point) <$> keyList)

-- Parsers

fromChar :: Char -> Pipe
fromChar x = case x of 
    '|' -> NS
    '-' -> EW
    'L' -> NE
    'J' -> NW
    '7' -> SW
    'F' -> SE
    '.' -> G
    'S' -> S
    _ -> G

lolToMap :: (Num b1, Num a, Enum b1, Enum a) => [[b2]] -> [[((a, b1), b2)]]
lolToMap l = fmap (\x -> 
                let innerList = snd x
                    y = fst x
                    listMap = Prelude.zip [0..] innerList
                in [(((a,y), c)) | (a, c) <- listMap]) $ Prelude.zip [0..] l

aocFile :: Parser [(Point, Pipe)]
aocFile = do
    x <- aocLine `sepEndBy1` endOfLine
    eof
    return $ Prelude.concat $ lolToMap x

aocLine :: Parser [Pipe]
aocLine = do
    x <- many1 (noneOf "\n ")
    return $ fromChar <$> x
