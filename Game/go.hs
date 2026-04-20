{-# Language TemplateHaskell #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language NoMonomorphismRestriction #-}

module Game.Go where

import qualified Data.IntMap as IntMap
import Data.Bits
import Data.Word
import Data.List
import Data.Maybe
import Data.Monoid
import qualified Set as S
import qualified Data.RTree.Base as RT
import Data.RTree.MBB hiding (mbb)
import Debug.Trace
import System.Random.Mersenne.Pure64
--import Control.Monad.Mersenne.Random
import Control.Lens
import Control.Arrow

------------------------------------------------------------------------
-- | monad mersenne random, updated for AMP

-- | The state of a random monad, optimized for performance.
data R a = R !a {-# UNPACK #-}!PureMT

------------------------------------------------------------------------

-- | A basic random monad, for generating random numbers from pure mersenne twisters.
newtype Rand a = Rand { runRand :: PureMT -> R a }

instance Functor Rand where
  {-# INLINE fmap #-}
  fmap f r = Rand $ \s -> case runRand r s of
                              R a s' -> R (f a) s'

instance Applicative Rand where
    {-# INLINE pure #-}
    pure a = Rand $ \s -> R a s

    rf <*> ra = Rand $ \s -> case runRand rf s of
                                 R f s' -> case runRand ra s' of
                                               R a s'' -> R (f a) s''

instance Monad Rand where
    {-# INLINE (>>=) #-}
    m >>= k  = Rand $ \s -> case runRand m s of
                                R a s' -> runRand (k a) s'

    {-# INLINE (>>) #-}
    m >>  k  = Rand $ \s -> case runRand m s of
                                R _ s' -> runRand k s'

-- | Run a random computation using the generator @g@, returning the result
-- and the updated generator.
runRandom  :: Rand a -> PureMT -> (a, PureMT)
runRandom  r g = case runRand r g of R x g -> (x, g)

-- | Evaluate a random computation using the mersenne generator @g@.  Note that the
-- generator @g@ is not returned, so there's no way to recover the
-- updated version of @g@.
evalRandom :: Rand a -> PureMT -> a
evalRandom r g = case runRand r g of R x _ -> x

 -- | Yield a new 'Int' value from the generator.
getInt      :: Rand Int
getInt      = Rand $ \s -> case randomInt s of (w,s') -> R w s'

--------------------------------------------------------------------------------------------------


intersectRTree mbb tree = isJust $ mbb `intersectMBB` (RT.getMBB tree)

-- | returns all keys and values, which intersect a given MBB.
-- | useful for finding everything adjacent to a point.
lookupIntersectionWithKey :: MBB -> RT.RTree BitBoard -> [MBBitBoard]
lookupIntersectionWithKey _ RT.Empty = []
lookupIntersectionWithKey mbb t@RT.Leaf{}
    | mbb `intersectRTree` t = [MBBitBoard (RT.getMBB t) (RT.getElem t)]
    | otherwise = []
lookupIntersectionWithKey mbb t = founds
    where
    matches = filter (intersectRTree mbb) $ RT.getChildren t
    founds = concatMap (lookupIntersectionWithKey mbb) matches


--------------------------------------------------------------------------------------------------

-- A bitboard is something like a bit array.  They're often used in Chess, because a chessboard is
-- 64 squares big.  We need a much bigger board, here.  Bitboards are nice because they make many
-- set operations very cheap: set intersection is .|. and set union is .&.
--
-- Additionally, finding the squares adjacent to a group is 4 simple bitshifts.
--
-- A go board is 361 = 5.6 * 64.  In 6 word 64s, we have enough space to store
-- an entire board with a gutter between each row.  The gutter is there so when you shift
-- by one square, the first/last square on a row isn't in the sq
data BitBoard = BB {-# UNPACK #-} !Word64 
                   {-# UNPACK #-} !Word64 
                   {-# UNPACK #-} !Word64 
                   {-# UNPACK #-} !Word64 
                   {-# UNPACK #-} !Word64 
                   {-# UNPACK #-} !Word64 

-- Problem: Can't have a non-existent MBB.  However, zero doesn't have a MBB.  So just represent that explicitly
data MBBitBoard = MBBitBoard MBB BitBoard | Zero

toBitBoard Zero = zero
toBitBoard (MBBitBoard _ bb) = bb

instance Eq BitBoard where
  (==) (BB a b c d e f) (BB a' b' c' d' e' f') = a == a' 
                                              && b == b'
                                              && c == c'
                                              && d == d'
                                              && e == e'
                                              && f == f'
-- BB are stored big-endian.
instance Ord BitBoard where
  compare (BB a b c d e f) (BB a' b' c' d' e' f') = 
    compare a a' <> compare b b' <> compare c c' <> compare d d' <> compare e e'<> compare f f'
          
{-
  instance Show BitBoard where
  show (BB a b c d e f g) = unwords [lg a, lg b, lg c, lg d, lg e, lg f]
    where lg 0 = "-"
          lg x = show x -- . floor . logBase (2 :: Float) . fromInteger . toInteger $ x
          -}

instance Show BitBoard where
    show bb = unlines [ [ if' 'X' '-'  . testBit bb . pointToBBIndex $ Point x y 
                          | x <- [1 .. boardSize ]] 
                        | y <- [1..boardSize]]
      where if' t f b = if b then t else f 

instance Monoid BitBoard where
   mappend = (.&.)
   mempty = zero

showBoard blacks whites = 
    unlines [ [ disp . pointToBBIndex $ Point x y 
              | x <- [1 .. boardSize ]] 
            | y <- [1..boardSize]]
      where disp i | testBit blacks i = 'B'
                   | testBit whites i = 'W'
                   | otherwise = '-'


instance Bits BitBoard where
 (BB a b c d e f) .&. (BB a' b' c' d' e' f') = BB (a .&. a') (b .&. b') (c .&. c') (d .&. d') (e .&. e') (f .&. f') 
 (BB a b c d e f) .|. (BB a' b' c' d' e' f') = BB (a .|. a') (b .|. b') (c .|. c') (d .|. d') (e .|. e') (f .|. f') 
 xor (BB a b c d e f) (BB a' b' c' d' e' f') = BB (a `xor` a') (b `xor` b') (c `xor` c') (d `xor` d') (e `xor` e') (f `xor` f') 
 shiftL (BB a b c d e f) x = BB (shiftL a x .|. shiftR b (64 - x))
                                (shiftL b x .|. shiftR c (64 - x))
                                (shiftL c x .|. shiftR d (64 - x))
                                (shiftL d x .|. shiftR e (64 - x))
                                (shiftL e x .|. shiftR f (64 - x))
                                (shiftL f x)
 shiftR (BB a b c d e f) x = BB (shiftR a x)
                                (shiftR b x .|. shiftL a (64 - x))
                                (shiftR c x .|. shiftL b (64 - x))
                                (shiftR d x .|. shiftL c (64 - x))
                                (shiftR e x .|. shiftL d (64 - x))
                                (shiftR f x .|. shiftL e (64 - x))
 rotateR (BB a b c d e f) x =  BB (shiftR a x .|. shiftL f (64 - x))
                                  (shiftR b x .|. shiftL a (64 - x))
                                  (shiftR c x .|. shiftL b (64 - x))
                                  (shiftR d x .|. shiftL c (64 - x))
                                  (shiftR e x .|. shiftL d (64 - x))
                                  (shiftR f x .|. shiftL e (64 - x))

 rotateL (BB a b c d e f) x = BB (shiftL a x .|. shiftR b (64 - x))
                                 (shiftL b x .|. shiftR c (64 - x))
                                 (shiftL c x .|. shiftR d (64 - x))
                                 (shiftL d x .|. shiftR e (64 - x))
                                 (shiftL e x .|. shiftR f (64 - x))
                                 (shiftL f x .|. shiftR a (64 - x))
 bitSize x = 448
 bitSizeMaybe x = Just 448
 isSigned x = isSigned (0 :: Word64)
 testBit (BB a b c d e f) x 
  | x < 64  = testBit f x
  | x < 128 = testBit e (x - 64)
  | x < 192 = testBit d (x - 128)
  | x < 256 = testBit c (x - 192)
  | x < 320 = testBit b (x - 256)
  | x < 384 = testBit a (x - 320)
  | otherwise = error $ show x ++ " is too large to fit in a bitboard"

 bit x 
  | x < 64  = BB 0 0 0 0 0 (bit $ x)         
  | x < 128 = BB 0 0 0 0 (bit $ x - 64)   0 
  | x < 192 = BB 0 0 0 (bit $ x - 128)  0 0 
  | x < 256 = BB 0 0 (bit $ x - 192)  0 0 0 
  | x < 320 = BB 0 (bit $ x - 256)  0 0 0 0 
  | x < 384 = BB (bit $ x - 320)  0 0 0 0 0 
  | otherwise = error $ show x ++ " is too large to fit in a bitboard"
 popCount (BB a b c d e f) = popCount a + popCount b + popCount c + popCount d +  popCount e +  popCount f
 complement (BB a b c d e f) = BB (complement a) (complement b) (complement c) (complement d)  (complement e)  (complement f)

select bb@(BB a b c d e f) x = if (go .&. bb) == zero then error $ "selected point out of bitboard.  bb was " ++ show bb ++ " selected " ++ show go else go where
  popf = popCount f
  pope = popCount e + popf
  popd = popCount d + pope
  popc = popCount c + popd
  popb = popCount b + popc
  popa = popCount a + popb
  go | x <= popf = BB 0 0 0 0 0 (select' f x)          
     | x <= pope = BB 0 0 0 0 (select' e $ x - popf) 0 
     | x <= popd = BB 0 0 0 (select' d $ x - pope) 0 0  
     | x <= popc = BB 0 0 (select' c $ x - popd) 0 0 0 
     | x <= popb = BB 0 (select' b $ x - popc) 0 0 0 0 
     | x <= popa = BB (select' a $ x - popb) 0 0 0 0 0 
     | otherwise = error $ "can't select " ++ show x ++ " from " ++ show bb
  
isSelectWrong = take 20 $ filter (not . isIn) lst''
  where lst = map (id &&& popCount) [1..] 
        lst' = filter ( (== 1) . snd) lst
        lst'' = lst' >>= \(x, pc) -> [(x,p)| p <- [1.. pc - 1]]
        isIn (x,k) = select' x k .&. x /= 0 

select' x k = if ans .&. x == 0 then error "impossibru" else ans
  where ans = bit (slowSelect x k)

bbsToBB = foldl (.|.) zero
-- Following functions stolen from EKmett's succinct package,
-- which currently only exists on github.  Replace when he pushes to hackage.

-- | For every byte in the input test whether it's non-zero, setting
-- the corresponding byte of the result to 0x01 or 0x00 accordingly
-- 
-- @'nonzero8' x = 'uleq8' 0x0101010101010101 x@
nonzero8 :: Word64 -> Word64
nonzero8 x = shiftR ((x .|. ((x .|. msbs) - lsbs)) .&. msbs) 7 where
  msbs = 0x8080808080808080
  lsbs = 0x0101010101010101

byteCounts :: Word64 -> Word64
byteCounts a = d .&. lsns where
  threes = 0x3333333333333333
  as = 0xAAAAAAAAAAAAAAAA
  lsbs = 0x0101010101010101
  lsns = 0x0f0f * lsbs
  b = a - shiftR (a .&. as) 1
  c = (b .&. threes) + (shiftR b 2 .&. threes)
  d = c + shiftR c 4
-- | signed compare byte by byte, returning whether or not the result is less than or equal to
-- the corresponding byte in the other word as the least significant bit of each byte
leq8 :: Word64 -> Word64 -> Word64
leq8 x y = shiftR (w .&. msbs) 7 where
  msbs = 0x8080808080808080
  z = (y .|. msbs) - (x .&. complement msbs)
  w = x `xor` y `xor` z
  
selectWord64 :: Word64 -> Int -> Int
selectWord64 x k 
  | popcnt == k = hack
  | popcnt < k = error $ "error: tried to select " ++ show k ++ "th bit from " ++ show x ++ " but popcount was " ++ show popcnt
  | otherwise = place + offset
  where
  wk = fromIntegral k
  lsbs = 0x0101010101010101
  hi = 0xFFFFFFFFFFFFFFF8
  inc = 0x8040201008040201
  sums = byteCounts x * lsbs
  steps = wk * lsbs
  place = fromIntegral $ shiftR (leq8 sums steps * lsbs) 53 .&. hi
  br = wk - (shiftR (shiftL sums 8) place .&. 0xFF)
  spread = (shiftR x place .&. 0xFF) * lsbs
  bitSums = nonzero8 (spread .&. inc) * lsbs
  offset = fromIntegral $ shiftR (leq8 bitSums (br * lsbs) * lsbs) 56
  popcnt = popCount x
  -- The rest of this only works in the case where popcount x > k
  -- If popcount x == k, then either we want the least significant bit,
  -- or we can artificially increase the popcount by setting the lsb
  hack = if x .&. 1 == 1 then 0 else selectWord64 (x .|. 1) k

slowSelect :: Word64 -> Int -> Int
slowSelect x k = go x k 63
  where go x 1 n = if testBit x n then n else go x 1 (n - 1)
        go x c 0 = error $  "bitcount of "++ show x ++ " must be greater than " ++ show k
        go x k n = if testBit x n then go x (k - 1) (n -1)
                                  else go x k (n - 1)

-- End of functions stolen from Ed.

zero = BB 0 0 0 0 0 0 

-- boardsize + 1 is because we want a gutter on either side of a row
-- This is so that when we shift it over 1, the edge stones aren't on the next line.
-- Then, we can just use a constant mask to remove them.
pointToBBIndex :: Point -> Int
pointToBBIndex (Point x y) =  (y-1) * (boardSize + 1) + (x-1)

pointToBB :: Point -> BitBoard
pointToBB = bit . pointToBBIndex

gutter :: BitBoard
gutter = complement $ foldl (.|.) zero gutterBits
  where gutterStride = boardSize + 1 
        gutterBits = map bit [gutterStride, gutterStride * 2 .. gutterStride * (boardSize - 1)]

adjacents bb = left .|. right .|. up .|. down
  where left = gutter .&. shiftL bb 1 
        right = gutter .&. shiftR bb 1
        up = shiftL bb $ boardSize + 1
        down = shiftR bb $ boardSize + 1

diagonals bb = upRight .|. downRight .|. upLeft .|. downLeft
  where upRight   = gutter .&. shiftL bb (boardSize + 2)
        upLeft    = gutter .&. shiftL bb boardSize
        downRight = gutter .&. shiftR bb (boardSize + 2)
        downLeft  = gutter .&. shiftR bb boardSize

touches bb1 bb2 = bb1 .&. bb2 /= zero

collapseSet = S.fold (.|.) zero
collapseTree = foldl' (.|.) zero . map snd . RT.toList


--------------------------------------------------------------------------------------------------

boardSize :: Num a => a
boardSize = 19 -- 19, 13 or 9.  Don't make it larger than 32.  It will break PointToInt

-- Note: derive Ord so IntMap (Maybe Color) has an Ord instance, so we can make a Set
-- of them.  The Ord instance doesn't really have any semantic meaning, though.
data Color = Black | White deriving (Show, Eq, Ord) -- use Just Color to include empty points, i.e. Nothing
data Point = Point Int Int deriving (Show, Eq, Ord)

getX (Point x _) = x
getY (Point _ y) = y

pointToInt (Point x y)  = (shiftL x 5) .|. y 
intToPoint num  = Point (shiftR num 5) (num .&. 31)

-- a board, a list of empty spots, i.e. candidate moves, and some information for implementin the ko rule
data Board = Board { _prevPos:: S.Set (BitBoard, BitBoard, BitBoard)
                   , _emptyPoints :: BitBoard
                   , _whiteChains :: RT.RTree BitBoard
                   , _white :: BitBoard
                   , _blackChains :: RT.RTree BitBoard
                   , _black :: BitBoard
                   } deriving (Eq)


allPoints = [Point x y | x <- [1..19], y <- [1..19]]
allPoints' = foldr (.|.) zero $ map pointToBB allPoints

selectWorks =  and $ map sel allPoints
  where sel p = let b = pointToBB p in b == select b 1 

makeLenses ''Board

removeFromEmpties board loc = over emptyPoints (.&. complement loc)  board
addToEmpties board point = over emptyPoints (.|. point)  board

instance Show Board where
    show board = showBoard b w
      where w = collapseTree (board ^. whiteChains)
            b = collapseTree (board ^. blackChains)

genBoard = Board{ _emptyPoints = foldr (.|.) zero $ map pointToBB points
                , _whiteChains = RT.empty 
                , _white = zero
                , _blackChains = RT.empty
                , _black = zero
                , _prevPos = S.empty
                }
        where points = [(Point x y) | x <-[1..boardSize], y <-[1..boardSize]] 

otherPlayer Black = White
otherPlayer White = Black

otherChains Black = whiteChains
otherChains White = blackChains

chains Black = blackChains
chains White = whiteChains

otherStones Black = white
otherStones White = black

stones Black = black
stones White = white

chainsIfTouchesStones bb player board res = if ((board ^. stones player) .|. bb) == zero 
                                            then S.singleton zero 
                                            else res

coalesceChains :: [MBBitBoard] -> MBBitBoard
coalesceChains [] = Zero
coalesceChains mbbs = foldl1 combine mbbs
  where
  combine (MBBitBoard mbb1 b1) (MBBitBoard mbb2 b2) = MBBitBoard (unionMBB mbb1 mbb2) (b1 .|. b2)
  combine Zero (MBBitBoard mbb b) = MBBitBoard mbb b
  combine (MBBitBoard mbb b) Zero = MBBitBoard mbb b

getChain :: MBBitBoard -> Color -> Board -> MBBitBoard
getChain (MBBitBoard mbb point) player board = coalesceChains filtered
  where 
    allChains = board ^. chains player
    filtered = lookupIntersectionWithKey mbb allChains

--testBoard = playMove (pointToBB $ Point 1 2) Black . playMove (pointToBB $ Point 2 1) Black . playMove (pointToBB $Point 1 1) White $ genBoard

unset p bb = bb .&. complement p

placeStone :: MBBitBoard -> Color -> Board -> Board
placeStone point player board = undefined -- set (chains player) newGroups . over (stones player) ((.|.) point) $ board
{-  where
    ownGroups = board ^. chains player
    adjacentGroups =  (touches $ adjacents point) ownGroups
    newGroup = S.fold (.|.) point adjacentGroups
    newGroups = S.insert newGroup $ ownGroups `S.difference` adjacentGroups
-}

removeCaptures :: MBBitBoard -> Color -> Board -> Board
removeCaptures point player board = undefined {- newEnemySet . newEmpties . newEnemyStones $ board
  where
    enemyGroups = board ^. otherChains player
    enemyAdjacents = S.filter (touches $ adjacents point) enemyGroups
    captures = S.filter (isCaptured board) enemyAdjacents
    capturedBB = S.fold (.|.) zero captures
    newEnemySet = set (otherChains player) $ enemyGroups `S.difference` captures
    newEnemyStones = over (otherStones player) (unset capturedBB)
    newEmpties = over emptyPoints ((.|.) capturedBB) -}


-- check moves validity before calling this method; it assumes move is valid
playMove :: MBBitBoard -> Color -> Board -> Board
playMove point player board = over prevPos (S.insert (board ^. emptyPoints, board ^. white, board ^. black))
                            . removeCaptures point player
                            . over emptyPoints (unset $ toBitBoard point)
                            . placeStone point player $ board

-- offer both a group and point version of this function.
isCaptured :: Board -> MBBitBoard -> Bool
isCaptured board chain = (board^.emptyPoints .&. adjacents (toBitBoard chain)) == zero

-- positional superko forbids a move iff it repeats a board position
isKo point color board played = S.member (played^.emptyPoints, played ^. white, played ^. black) (board^.prevPos)

-- we need to play the move to check if it's suicide
-- So just keep it around 
isSuicide :: MBBitBoard -> Color -> Board -> Board -> Bool
isSuicide point player board played = not (touches pointBB (board ^. emptyPoints))  || isCaptured played chain
  where
  pointBB = toBitBoard point
  chain = getChain point player played 

isOccupied point board = ((board ^. emptyPoints) .&. point) == zero

traceIf s b = if b then trace s b else b

isValidMove :: MBBitBoard -> Color -> Board -> (Bool, Board)
isValidMove point color board = (not (isOwnEye point color board || isKo point color board played || isSuicide point color board played), played)
  where
   played = playMove point color board

-- True means that it's an eye for the player.  False means that it might not be an eye
-- this is because we use an easy to test for but stupid means of figuring out if it's an eye
-- Used by AI to rule out some really stupid moves.
isOwnEye :: MBBitBoard -> Color -> Board -> Bool
isOwnEye pointmbb player board = (adj .&. st == adj)
                            && (popCount (diag .&. st) >= 3)
  where point = toBitBoard pointmbb
        adj = adjacents point
        diag = diagonals point
        st = board ^. stones player

----------------------------
--- Random Playouts --------
----------------------------

playMove' :: Color -> Board -> BitBoard -> Rand Board
playMove' color board illegals = -- {-# SCC playMove' #-}
                        do x <- getInt
                           let points = board^.emptyPoints .&. complement illegals
                           let count = popCount points
                           if {- trace ("count was " ++ show count ++ " x is " ++ show x ++ " points were" ++ show points ) $ -} count > 0
                           then let point = select points $ (x `mod` count) + 1
                            in if point .&. points == zero then error (show point ++ " isn't in " ++ show points)
                               else case isValidMove point color board of
                                     (True, played) -> return $ played
                                     (False, _) -> playMove' color board $ illegals .|. point 
                                       
                           else return board

playGame color = if color == Black then black 0 else white 0
    where playIfNotOver :: Board -> (Board -> Rand Board) -> Int -> Board -> Rand Board
          playIfNotOver board f n board2 = {- trace ("\n\n move: " ++ show n ++ "\n" ++ show board ) -} (if board == board2 then return board else f board2)
          black n board = playMove' Black board zero >>= playIfNotOver board (white $ n+1) n
          white n board = playMove' White board zero >>= playIfNotOver board (black $ n+1) n

playMoves = foldr p genBoard
  where
    p (x,y,color) = playMove (pointToBB $ Point x y) color 

-----------------------------
-- some test functions
----------------------------

--main = print $ evalRandom (playGame Black genBoard) (pureMT 5)
finishedGame = evalRandom (playGame Black genBoard) (pureMT 5)
finishedGame' i = evalRandom (playGame Black genBoard) (pureMT i)
