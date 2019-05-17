import Data.List
type Vertex = String
type Graph = [(Vertex, Vertex)]

data Color = Color Int

data ColoredGraph = ColoredGraph Graph 


merge :: Graph -> Vertex -> Vertex -> Graph
merge g x y = uniq $ rename (rename g x newName) y newName 
  where
    newName = sort $ x ++ y

rename :: Graph -> Vertex -> Vertex -> Graph
rename list src dest = map (renameLeft . renameRight) list
  where
    renameLeft (x, y) | x == src = (dest, y)
    renameLeft t = t

    renameRight (x, y) | y == src = (x, dest)
    renameRight t = t

uniq :: Graph -> Graph
uniq = nub . filter (\(x, y) -> x /= y) . map (\(x, y) -> if x > y then (y, x) else (x, y))

getNodes :: Graph -> [Vertex]
getNodes = map head . group . sort . concatMap (\(x,y) -> [x, y])

subGraphs :: Graph -> [Graph]
subGraphs g = map f g
  where
    f = \(x, y) -> merge g x y
    nodes = getNodes g

allSubGraphs :: Graph -> [Graph]
allSubGraphs g = reverse . sortOn length . nub $ subGraphs g ++ concatMap (allSubGraphs) (subGraphs g)


printSize :: [Graph] -> Int -> String
printSize g num = (\x -> x ++ " : " ++ show (length $ filtered g)) . intercalate " " . map show . filtered $ g
  where
    filtered = filter ((==num) . length . getNodes)


printAll :: [Graph] -> String
printAll g = unlines . intersperse "\n" . map (printSize g) $ [0..maxSize]
  where
    maxSize = maximum $ map (length . getNodes) g





  
