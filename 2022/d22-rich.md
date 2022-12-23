The test case has format

```
 X X A X     . . 7 .
 B C D X     2 5 8 .
 X X E F     . . 9 12
```

We can show the links this way:

```
    +-------+
    |       |
    |       A------+
    |       |      |
 +--B---C---D---+  |
 |  |   |   |   |  |
 |  |   +---E---F--+
 |  |       |   |
 |  +-------+   |
 +--------------+
```

So

* heading down (d) from A leads to D heading d
* heading left (l) from A leads to F heading r

For my real case I have

```
  A E  . 5 8
  D    . 6 .
B F    3 7 .
C      4 . .
```

with links:

```
 +-------+   +----+
 |       |   |    |
 | +-----A---E-+  |
 | |     |   | |  |
 | | +---D---+ |  |
 | | |   |     |  |
 | +-B---F-----+  |
 |   |   |        |
 +---C---+        |
     |            |
     +------------+
```

For the test case the table is

```
from direction to rotate
   A         r  F      l
             d  D      d
             l  C      d
             u  B      d

   B         r  C      r
             d  E      u
             l  F      u
             u  A      d

   C         r  D      r
             d  E      r
             l  B      l
             u  A      r

   D         r  F      d
             d  E      d
             l  C      l
             u  A      u

   E         r  F      r
             d  B      u
             l  C      u
             u  D      u

   F         r  A      l
             d  B      r
             l  E      l
             u  D      l
```

and for my real case

```
from direction to rotate
   A         r  E      r
             d  D      d
             l  B      r
             u  C      r

   B         r  F      r
             d  C      d
             l  A      r
             u  D      r

   C         r  F      u
             d  E      d
             l  A      d
             u  B      u

   D         r  E      u
             d  F      d
             l  B      d
             u  A      u

   E         r  F      l
             d  D      l
             l  A      l
             u  C      u

   F         r  E      l
             d  C      l
             l  B      l
             u  D      u
```

These form basis of the lists that are fed to the `info` functions.  The transforms for part 1 are much simpler.
