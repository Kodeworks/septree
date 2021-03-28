# SepTree

A 2D index or something.

### Description

A _SepTree_ is an index much like a QuadTree over 2D points. _Sep_ points to the number 7, because there are 7 smaller
tiles (hexagonals/hexes) for each bigger tile. Unlike a QuadTree it does not tesselate perfectly, as the smaller hexes needs to be rotated
a small angle in order to _fit_ into a bigger tile.

Each tiling of 7 hexes is indexed as follows:
```
  1   2 
3   4   5
  6   7
```
Note that `0` is not used.

### Operations
(I) means impemented

- Create index from keys (I)
- Get keys from index (I)
- Index point in space
- Get point from index