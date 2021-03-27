# SepTree

A 2D index or something.

### Description

A _SepTree_ is an index much like a QuadTree over 2D points. _Sep_ points to the number 7, because there are 7 smaller
tiles for each bigger tile. Unlike a QuadTree it does not tesselate perfectly, as the smaller tiles needs to be rotated
a small angle in order to _fit_ into a bigger tile.

### Operations
(I) means impemented

- Create index from keys (I)
- Get keys from index (I)
- Index point in space
- Get point from index