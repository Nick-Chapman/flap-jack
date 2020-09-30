# flap-jack

Exploring strategies for flap-jack. See [code](src/Top.hs).

    twist-N : Twist a fixed number of cards (a really dumb strategy).
    reach-N : Twist until we reach an accumulated goal: 16..25 (an ok simple strategy).
    god-score: Best play when future cards are known.


```
$ stack run
*flap-jack*
("twist-7",9.3643)
("reach-25",8.76)
("reach-24",7.7819)
("reach-23",7.0239)
("reach-16",6.7176)
("reach-22",6.5041)
("reach-17",6.3068)
("reach-21",6.1576)
("reach-18",6.0283)
("reach-20",5.8901)
("reach-19",5.8716)
("god-score",4.8543)
```
