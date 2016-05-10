-- ---
-- title: Homework #1, Due Monday 1/26/15
-- ---


-- Haskell Formalities
-- -------------------

-- We declare that this is the Hw1 module and import some libraries:

module Hw1 where
import SOE
import Play
import XMLTypes

-- Part 0: All About You
-- ---------------------

-- Tell us your name, email and student ID, by replacing the respective
-- strings below

myName  = "Amit Borase"
myEmail = "aborase@ucsd.edu"
mySID   = "A53095391"

-- Part 1: Defining and Manipulating Shapes
-- ----------------------------------------

-- You will write all of your code in the `Hw1.hs` file, in the spaces
-- indicated. Do not alter the type annotations --- your code must
-- typecheck with these types to be accepted.

-- The following are the definitions of shapes:

data Shape = Rectangle Side Side
           | Ellipse Radius Radius
           | RtTriangle Side Side
           | Polygon [Vertex]
           deriving Show
-- >
type Radius = Float
type Side   = Float
type Vertex = (Float, Float)

-- 1. Below, define functions `rectangle` and `rtTriangle` as suggested
--    at the end of Section 2.1 (Exercise 2.1). Each should return a Shape
--    built with the Polygon constructor.

rectangle :: Side -> Side -> Shape
rectangle s1 s2 = Polygon [(0, 0), (s1, 0), (0, s2), (s1, s2)]

rtTriangle :: Side -> Side -> Shape
rtTriangle s1 s2 = Polygon [(0, 0), (s1, 0), (s1, s2)]
-- rtTriangle _ = Polygon [(0,0), (0,1), (1,0)]

-- 2. Define a function
--    which returns the number of sides a given shape has.
--    For the purposes of this exercise, an ellipse has 42 sides,
--    and empty polygons, single points, and lines have zero sides.
sides :: Shape -> Int
sides (Rectangle _ _)       = 4
sides (Ellipse _ _)         = 42
sides (RtTriangle _ _)      = 3
sides (Polygon my_list)
    | length my_list <= 2 = 0
    | otherwise             = length my_list

-- 3. Define a function
--    that takes a shape `s` and expansion factor `e` and returns
--    a shape which is the same as (i.e., similar to in the geometric sense)
--    `s` but whose area is `e` times the area of `s`.
bigger :: Shape -> Float -> Shape
bigger (Rectangle x y) factor
    = Rectangle x1 y1
    where
        x1 = x * sqrt factor
        y1 = y * sqrt factor

bigger (Ellipse x y) factor
    = Ellipse x1 y1
    where
        x1 = x * sqrt factor
        y1 = y * sqrt factor

bigger (RtTriangle x y) factor
    = RtTriangle x1 y1
    where
        x1 = x * sqrt factor
        y1 = y * sqrt factor

bigger (Polygon my_list) factor
    | length my_list <= 2 = error "Invalid Polygon passed"
    | otherwise             = Polygon(transform_coord my_list)
    where
        transform_coord :: [Vertex] -> [Vertex]
        transform_coord []              = []
        transform_coord ((p1, p2) : xs) = (p1 * sqrt factor, p2 * sqrt factor) : transform_coord xs

-- 4. The Towers of Hanoi is a puzzle where you are given three pegs,
--    on one of which are stacked $n$ discs in increasing order of size.
--    To solve the puzzle, you must move all the discs from the starting peg
--    to another by moving only one disc at a time and never stacking
--    a larger disc on top of a smaller one.

--    To move $n$ discs from peg $a$ to peg $b$ using peg $c$ as temporary storage:

--    1. Move $n - 1$ discs from peg $a$ to peg $c$.
--    2. Move the remaining disc from peg $a$ to peg $b$.
--    3. Move $n - 1$ discs from peg $c$ to peg $b$.

--    Write a function
--    that, given the number of discs $n$ and peg names $a$, $b$, and $c$,
--    where a is the starting peg,
--    emits the series of moves required to solve the puzzle.
--    For example, running `hanoi 2 "a" "b" "c"`
--    should emit the text
--      ~~~
--      move disc from a to c
--      move disc from a to b
--      move disc from c to b
--      ~~~

hanoi :: Int -> String -> String -> String -> IO ()
hanoi 0 _ _ _ = return ()
hanoi n a b c = do
    hanoi (n-1) a c b
    putStrLn("move disc from " ++a++ " to " ++b)
    hanoi (n-1) c b a

hanoiHelper :: Int -> Int
hanoiHelper _ _ = 1

-- Part 2: Drawing Fractals
-- ------------------------

-- 1. The Sierpinski Carpet is a recursive figure with a structure similar to
--    the Sierpinski Triangle discussed in Chapter 3:

-- ![Sierpinski Carpet](/static/scarpet.png)

-- Write a function `sierpinskiCarpet` that displays this figure on the
-- screen:

sierpinskiFill :: Window -> Int -> Int -> Int -> IO ()
sierpinskiFill w x y s
         = drawInWindow w (withColor Blue
         (polygon[(x,y), (x + s, y), (x + s, y + s), (x, y + s)]))

minSide :: Int
minSide = 1

sierpinskiRecur :: Window -> Int -> Int -> Int -> IO ()
sierpinskiRecur w x y s
    | s <= minSide = sierpinskiFill w x y s
    | otherwise = let s3 = s `div` 3
                      sp = s3 `div` 9
                  in do sierpinskiRecur w x y s3
                        sierpinskiRecur w x (y + s3 + sp) s3
                        sierpinskiRecur w x (y + s3 * 2 +sp * 2) s3
                        sierpinskiRecur w (x + s3 + sp) y s3
                        sierpinskiRecur w (x + s3 + sp) (y + s3 * 2 + sp * 2) s3
                        sierpinskiRecur w (x + s3 * 2 + sp * 2) y s3
                        sierpinskiRecur w (x + s3 * 2 + sp * 2) (y + s3 + sp) s3
                        sierpinskiRecur w (x + s3 * 2 + sp * 2) (y + s3 * 2 + sp * 2) s3

sierpinskiCarpet :: IO ()
sierpinskiCarpet = runGraphics (
                   do
                        w <- openWindow "Sierpinski's Carpet" (320, 320)
                        sierpinskiRecur w 10 10 270
                        k <- getKey w
                        closeWindow w
                   )

-- Note that you either need to run your program in `SOE/src` or add this
-- path to GHC's search path via `-i/path/to/SOE/src/`.
-- Also, the organization of SOE has changed a bit, so that now you use
-- `import SOE` instead of `import SOEGraphics`.

-- 2. Write a function `myFractal` which draws a fractal pattern of your
--    own design.  Be creative!  The only constraint is that it shows some
--    pattern of recursive self-similarity.

fractalFill :: Window -> Int -> Int -> Int  -> Color -> IO ()
fractalFill w x y s c
    = drawInWindow w (withColor c
          (polygon[(x - s, y - s), (x + s, y - s), (x + s, y + s), (x - s, y + s)]))

minSqSide :: Int
minSqSide = 1

fractalRecur :: Window -> Int -> Int -> Int -> IO ()
fractalRecur w x y s
    | s <= minSqSide = fractalFill w x y s Black
    | otherwise = let s4 = s `div` 4
                      s2 = s `div` 2
                      s8 = s `div` 8
                  in do fractalFill w x y s2 Black
                        fractalRecur w (x - s2) (y - s2) s2
                        fractalRecur w (x + s2) (y - s2) s2
                        fractalRecur w (x + s2) (y + s2) s2
                        fractalRecur w (x - s2) (y + s2) s2
                        drawInWindow w (withColor White (ellipse (x - s4, y - s2) (x + s4, y + s2)))
                        drawInWindow w (withColor White (ellipse (x - s2, y - s4) (x + s2, y + s4)))
                        drawInWindow w (withColor Black (ellipse (x - s8, y - s4) (x + s8, y + s4)))
                        drawInWindow w (withColor Black (ellipse (x - s4, y - s8) (x + s4, y + s8)))
                        fractalRecur w x y s4

myFractal :: IO ()
myFractal = runGraphics(
            do
                w <- openWindow "My Fractal pattern" (400, 400)
                drawInWindow w (withColor White
                        (polygon[(0,0), (400,0), (400,400), (0,400)]))
                fractalRecur w 200 200 200
                k <- getKey w
                closeWindow w
            )

-- Part 3: Recursion Etc.
-- ----------------------

-- First, a warmup. Fill in the implementations for the following functions.

-- (Your `maxList` and `minList` functions may assume that the lists
-- they are passed contain at least one element.)

-- Write a *non-recursive* function to compute the length of a list

lengthNonRecursive :: [a] -> Int
lengthNonRecursive list = sum(map (const 1) list)
-- Same as sum (map (\x -> 1) list)

-- `doubleEach [1,20,300,4000]` should return `[2,40,600,8000]`

doubleEach :: [Int] -> [Int]
doubleEach []     = []
doubleEach (x:xs) = 2 * x : doubleEach xs

-- Now write a *non-recursive* version of the above.

doubleEachNonRecursive :: [Int] -> [Int]
doubleEachNonRecursive = map (\x -> 2 * x)

-- `pairAndOne [1,20,300]` should return `[(1,2), (20,21), (300,301)]`

pairAndOne :: [Int] -> [(Int, Int)]
pairAndOne []       = []
pairAndOne (x : xs) = (x, x+1) : pairAndOne xs

-- Now write a *non-recursive* version of the above.

pairAndOneNonRecursive :: [Int] -> [(Int, Int)]
pairAndOneNonRecursive = map (\x -> (x, x+1))

-- `addEachPair [(1,2), (20,21), (300,301)]` should return `[3,41,601]`

addEachPair :: [(Int, Int)] -> [Int]
addEachPair []           = []
addEachPair ((x,y) : xs) = (x+y) : addEachPair xs

-- Now write a *non-recursive* version of the above.

addEachPairNonRecursive :: [(Int, Int)] -> [Int]
addEachPairNonRecursive = map (uncurry (+))

-- `minList` should return the *smallest* value in the list. You may assume the
-- input list is *non-empty*.

minList :: [Int] -> Int
minList [x]      = x
minList (x : xs) = x `min` minList xs

-- Now write a *non-recursive* version of the above.

minListNonRecursive :: [Int] -> Int
minListNonRecursive = foldr min maxBound

-- `maxList` should return the *largest* value in the list. You may assume the
-- input list is *non-empty*.

maxList :: [Int] -> Int
maxList [x]      = x
maxList (x : xs) = x `max` maxList xs

-- Now write a *non-recursive* version of the above.

maxListNonRecursive :: [Int] -> Int
maxListNonRecursive = foldr max minBound

-- Now, a few functions for this `Tree` type.

data Tree a = Leaf a | Branch (Tree a) (Tree a)
              deriving (Show, Eq)

-- `fringe t` should return a list of all the values occurring as a `Leaf`.
-- So: `fringe (Branch (Leaf 1) (Leaf 2))` should return `[1,2]`
-- The best is to use ++ for list construction rather than : as one of the recursive cases cannot be applied to construct lists
fringe :: Tree a -> [a]
fringe (Leaf a)                           = [a]
fringe (Branch (Leaf a) (Leaf b))         = [a, b]
fringe (Branch (Leaf a) (Branch b c))     = fringe (Leaf a) ++ fringe(Branch b c)
fringe (Branch (Branch a b) (Leaf c))     = fringe (Branch a b) ++ fringe(Leaf c)
fringe (Branch (Branch a b) (Branch c d)) = fringe (Branch a b) ++ fringe(Branch c d)

-- `treeSize` should return the number of leaves in the tree.
-- So: `treeSize (Branch (Leaf 1) (Leaf 2))` should return `2`.

treeSize :: Tree a -> Int
treeSize a = length (fringe a)

-- `treeSize` should return the height of the tree.
-- So: `height (Branch (Leaf 1) (Leaf 2))` should return `1`.

treeHeight :: Tree a -> Int
treeHeight (Leaf _)                           = 0
treeHeight (Branch (Leaf _) (Leaf _))         = 1
treeHeight (Branch (Leaf _) (Branch b c))     = 1 + treeHeight (Branch b c)
treeHeight (Branch (Branch a b) (Leaf _))     = treeHeight (Branch a b) + 1
treeHeight (Branch (Branch a b) (Branch c d)) = 1 + max (treeHeight (Branch a b)) (treeHeight (Branch c d))

-- Now, a tree where the values live at the nodes not the leaf.

data InternalTree a = ILeaf | IBranch a (InternalTree a) (InternalTree a)
                      deriving (Show, Eq)

-- `takeTree n t` should cut off the tree at depth `n`.
-- So `takeTree 1 (IBranch 1 (IBranch 2 ILeaf ILeaf) (IBranch 3 ILeaf ILeaf)))`
-- should return `IBranch 1 ILeaf ILeaf`.

takeTree :: Int -> InternalTree a -> InternalTree a
takeTree 1 (IBranch a _ _)             = IBranch a ILeaf ILeaf
takeTree n (IBranch a ILeaf ILeaf) = IBranch a ILeaf ILeaf
takeTree n (IBranch a b c) =
    let leftSubTree  = takeTree (n-1) b
        rightSubTree = takeTree (n-1) c
    in IBranch a leftSubTree rightSubTree

-- `takeTreeWhile p t` should cut of the tree at the nodes that don't satisfy `p`.
-- So: `takeTreeWhile (< 3) (IBranch 1 (IBranch 2 ILeaf ILeaf) (IBranch 3 ILeaf ILeaf)))`
-- should return `(IBranch 1 (IBranch 2 ILeaf ILeaf) ILeaf)`.

takeTreeWhile :: (a -> Bool) -> InternalTree a -> InternalTree a
takeTreeWhile p ILeaf = ILeaf
takeTreeWhile p (IBranch a b c)
    | p a =
        let leftSubTree  = takeTreeWhile p b
            rightSubTree = takeTreeWhile p c
        in IBranch a leftSubTree rightSubTree
    | otherwise = ILeaf

-- Write the function map in terms of foldr:

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\y ys -> f y : ys) []

-- Part 4: Transforming XML Documents
-- ----------------------------------

-- The rest of this assignment involves transforming XML documents.
-- To keep things simple, we will not deal with the full generality of XML,
-- or with issues of parsing. Instead, we will represent XML documents as
-- instances of the following simpliﬁed type:

-- ~~~~
-- data SimpleXML =
--    PCDATA String
--  | Element ElementName [SimpleXML]
--  deriving Show

-- type ElementName = String
-- ~~~~

-- That is, a `SimpleXML` value is either a `PCDATA` ("parsed character
-- data") node containing a string or else an `Element` node containing a
-- tag and a list of sub-nodes.

-- The file `Play.hs` contains a sample XML value. To avoid getting into
-- details of parsing actual XML concrete syntax, we'll work with just
-- this one value for purposes of this assignment. The XML value in
-- `Play.hs` has the following structure (in standard XML syntax):

-- ~~~
-- <PLAY>
--   <TITLE>TITLE OF THE PLAY</TITLE>
--   <PERSONAE>
--     <PERSONA> PERSON1 </PERSONA>
--     <PERSONA> PERSON2 </PERSONA>
--     ... -- MORE PERSONAE
--     </PERSONAE>
--   <ACT>
--     <TITLE>TITLE OF FIRST ACT</TITLE>
--     <SCENE>
--       <TITLE>TITLE OF FIRST SCENE</TITLE>
--       <SPEECH>
--         <SPEAKER> PERSON1 </SPEAKER>
--         <LINE>LINE1</LINE>
--         <LINE>LINE2</LINE>
--         ... -- MORE LINES
--       </SPEECH>
--       ... -- MORE SPEECHES
--     </SCENE>
--     ... -- MORE SCENES
--   </ACT>
--   ... -- MORE ACTS
-- </PLAY>
-- ~~~

-- * `sample.html` contains a (very basic) HTML rendition of the same
--   information as `Play.hs`. You may want to have a look at it in your
--   favorite browser.  The HTML in `sample.html` has the following structure
--   (with whitespace added for readability):

-- ~~~
-- <html>
--   <body>
--     <h1>TITLE OF THE PLAY</h1>
--     <h2>Dramatis Personae</h2>
--     PERSON1<br/>
--     PERSON2<br/>
--     ...
--     <h2>TITLE OF THE FIRST ACT</h2>
--     <h3>TITLE OF THE FIRST SCENE</h3>
--     <b>PERSON1</b><br/>
--     LINE1<br/>
--     LINE2<br/>
--     ...
--     <b>PERSON2</b><br/>
--     LINE1<br/>
--     LINE2<br/>
--     ...
--     <h3>TITLE OF THE SECOND SCENE</h3>
--     <b>PERSON3</b><br/>
--     LINE1<br/>
--     LINE2<br/>
--     ...
--   </body>
-- </html>
-- ~~~

-- You will write a function `formatPlay` that converts an XML structure
-- representing a play to another XML structure that, when printed,
-- yields the HTML speciﬁed above (but with no whitespace except what's
-- in the textual data in the original XML).

{-
    The function changeElementName does the following
    1. Takes as input
        a. A String
        b. A SimpleXML
    2. Returns as output a single SimpleXML contained in a list with the name changed
       to the String value passed as an argument to the function
-}
changeElementName :: ElementName -> SimpleXML -> [SimpleXML]
changeElementName newName (Element _ children) = [Element newName children]
changeElementName _ x                          = [x]

{-
    The function envelopeElement does the following
    1. Takes as input
        a. A String
        b. A SimpleXML
    2. Returns as output a new SimpleXML constructed using the String and the SimpleXML
       passed as input arguments to the function
-}
envelopeElement :: ElementName -> SimpleXML -> SimpleXML
envelopeElement newXml xml = Element newXml [xml]

{-
    The function prependChildXML does the following
    1. Takes as input
        a. A SimpleXML
        b. A SimpleXML
    2. Returns as output a list which contains a Single SimpleXML where the first SimpleXML
       is prepended to the list of SimpleXMLs housed by the second SimpleXML passed
       as input argument to the function
-}
prependChildXML :: SimpleXML -> SimpleXML -> [SimpleXML]
prependChildXML childXML (Element ename children) = [Element ename (childXML : children)]
prependChildXML _ x                               = [x]

{-
    The function appendChildXML does the following
    1. Takes as input
        a. A SimpleXML
        b. A SimpleXML
    2. Returns as output a list which contains a Single SimpleXML where the first SimpleXML
       is appended to the list of SimpleXMLs housed by the second SimpleXML passed
       as input argument to the function
-}
appendChildXML :: SimpleXML->SimpleXML->[SimpleXML]
appendChildXML (Element ename children) childXML = [Element ename (children ++ [childXML])]
appendChildXML _ x                               = [x]

{-
    The function deleteParentXML does the following
    1. Takes as input a SimpleXML
    2. Returns as output all the children of the input SimpleXML
-}
deleteParentXML :: SimpleXML -> [SimpleXML]
deleteParentXML (Element _ children)  = children
deleteParentXML _                     = []

{-
    The function prependXML dpes the following
    1. Takes as input
        a. A SimpleXML
        b. A list of SimpleXMLs
    2. Returns as output a list of SimpleXMLs where the first SimpleXML argument
       is prepended to the list of SimpleXMLs passed as the second argument
-}
prependXML :: SimpleXML-> [SimpleXML] ->[SimpleXML]
prependXML newXml element_list = newXml : element_list

{-
    The function appendXML dpes the following
    1. Takes as input
        a. A SimpleXML
        b. A list of SimpleXMLs
    2. Returns as output a list of SimpleXMLs where the first SimpleXML argument
       is appended to the list of SimpleXMLs passed as the second argument
-}
appendXML :: [SimpleXML] -> SimpleXML -> [SimpleXML]
appendXML a b = a ++ [b]

{-
    This is the Mapping function that is specific to the Play.hs input
    We handle the individual tags that appear in Play.hs on a case by case basis
    For each input XML mapping that is given in the input we try to find a corresponding
    output HTML mapping.
-}
handleTag :: Int -> SimpleXML -> [SimpleXML]
handleTag lvl (PCDATA entry) = [PCDATA entry]
handleTag lvl (Element ename xml)
    | ename == "TITLE"
        = changeElementName ("h" ++ show lvl) (Element ename xml)
    | ename == "PERSONAE"
        = prependXML (Element "h2" [PCDATA "Dramatis Personae"]) (concatmap (handleTag lvl) (deleteParentXML(Element ename xml)))
    | (ename == "PERSONA") || (ename == "LINE")
        = appendXML (deleteParentXML(Element ename xml)) (Element "br" [])
    | (ename == "ACT") || (ename == "SCENE") || (ename == "SPEECH")
        = concatmap (handleTag (lvl + 1)) xml
    | ename == "SPEAKER"
        = appendXML [Element "b" xml] (Element "br" [])

-- The helper function that calls handleTag on each SimpleXML present in PLAY
formatPlayHelper :: [SimpleXML] -> [SimpleXML]
formatPlayHelper [] = []
formatPlayHelper xs = foldr ((++) handleTag 1) [] xs

-- The actual formatPlay function
formatPlay :: SimpleXML -> SimpleXML
formatPlay (Element ename xml)
    = Element "html" [Element "body" (formatPlayHelper xml)]

-- The main action that we've provided below will use your function to
-- generate a ﬁle `dream.html` from the sample play. The contents of this
-- ﬁle after your program runs must be character-for-character identical
-- to `sample.html`.

mainXML = do writeFile "dream.html" $ xml2string $ formatPlay play
             testResults "dream.html" "sample.html"
-- >
firstDiff :: Eq a => [a] -> [a] -> Maybe ([a],[a])
firstDiff [] [] = Nothing
firstDiff (c:cs) (d:ds)
     | c==d = firstDiff cs ds
     | otherwise = Just (c:cs, d:ds)
firstDiff cs ds = Just (cs,ds)
-- >
testResults :: String -> String -> IO ()
testResults file1 file2 = do
  f1 <- readFile file1
  f2 <- readFile file2
  case firstDiff f1 f2 of
    Nothing -> do
      putStr "Success!\n"
    Just (cs,ds) -> do
      putStr "Results differ: '"
      putStr (take 20 cs)
      putStr "' vs '"
      putStr (take 20 ds)
      putStr "'\n"

-- Important: The purpose of this assignment is not just to "get the job
-- done" --- i.e., to produce the right HTML. A more important goal is to
-- think about what is a good way to do this job, and jobs like it. To
-- this end, your solution should be organized into two parts:

-- 1. a collection of generic functions for transforming XML structures
--    that have nothing to do with plays, plus

-- 2. a short piece of code (a single deﬁnition or a collection of short
--    deﬁnitions) that uses the generic functions to do the particular
--    job of transforming a play into HTML.

-- Obviously, there are many ways to do the ﬁrst part. The main challenge
-- of the assignment is to ﬁnd a clean design that matches the needs of
-- the second part.

-- You will be graded not only on correctness (producing the required
-- output), but also on the elegance of your solution and the clarity and
-- readability of your code and documentation.  Style counts.  It is
-- strongly recommended that you rewrite this part of the assignment a
-- couple of times: get something working, then step back and see if
-- there is anything you can abstract out or generalize, rewrite it, then
-- leave it alone for a few hours or overnight and rewrite it again. Try
-- to use some of the higher-order programming techniques we've been
-- discussing in class.

-- Submission Instructions
-- -----------------------

-- * If working with a partner, you should both submit your assignments
--   individually.

-- * Make sure your `Hw1.hs` is accepted by GHCi without errors or warnings.

-- * Attach your `hw1.hs` file in an email to `cse230@goto.ucsd.edu` with the
--   subject "HW1" (minus the quotes). *This address is unmonitored!*

-- Credits
-- -------

-- This homework is essentially Homeworks 1 & 2 from
-- <a href="http://www.cis.upenn.edu/~bcpierce/courses/552-2008/index.html">UPenn's CIS 552</a>.
