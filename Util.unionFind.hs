module Util.UnionFind

-- A modified Union-Find structure.
-- Used to track points adjacent to connectied components

-- e.g. UnionFind Point (Set EmptyPoint)
data UnionFind a b = UnionFind

union :: (b -> b) -> a -> a -> UnionFind a b -> UnionFind a b
find :: UnionFind a b -> a -> b
remove :: a -> UnionFind a b -> UnionFind a b
mapCanonicals (b -> b) -> UnionFind a b -> UnionFind a b

-- Problem: now, I can't get the actual group associated with a point
-- solution: just use the old getGroup depth first search.  It will be called very infrequently.
