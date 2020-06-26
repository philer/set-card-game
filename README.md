SET!
====

Browser version of the [SET card game](https://en.wikipedia.org/wiki/Set_(card_game)).

You can play it [**here**](https://philer.org/set)!

![](teaser.png)


Math
----

There is some math involved in optimizing this. It's hardly necessary but was fun to do.

Cards are distinguished by four separate features (color, shape, fill pattern and number of symbols), each of which has three values. That means a total of 3<sup>4</sup> = 81 cards, each of which can be represented as a number. To identify which number belongs to which card, we first assign each features's values to the digits  0, 1 and 2. For example red is 0, green is 1, blue is 2.

|       | 0         | 1       | 2       |
| ----- | --------- | ------- | ------- |
| color | red       | green   | blue    |
| shape | rectangle | tilde   | ellipse |
| fill  | empty     | hatched | full    |
| count | 1         | 2       | 3       |

With four features we get four digits of a number. For instance the card with two full green rectangles would be represented as 1021.

Note: If we interpret the number as a [ternary number](https://en.wikipedia.org/wiki/Ternary_numeral_system) it can be converted to base 10: (1021)<sub>3</sub> = 34. Thus we get exactly the numbers from (0000)<sub>3</sub> = 0 to (2222)<sub>3</sub> = 80.

Next we want to be able to validate card triples, i.e. check if three cards are either identical or mutually different in all features. For each feature we ca simply compare the relevant digit in each card's number, so for example (0, 0, 0) would be valid, (1, 2, 1) would be invalid. This is a bit tedious as it requires a lot of comparisons, so let's find an easier way to compute the same result. If we add the values, we get a number between 0 and 6:

* 0+0+0 = 0 (valid)
* 0+0+1 = 1 (invalid)
* 0+1+1 = 2 (invalid)
* 0+0+2 = 2 (invalid)
* 0+1+2 = 3 (valid)
* 1+1+1 = 3 (valid)
* 1+1+2 = 4 (invalid)
* 1+2+2 = 5 (invalid)
* 2+2+2 = 6 (valid)

Note that exactly the valid combinations result in a sum of 0, 3 or 6.
That means we can simple calculate the sum of the relevant digits and check if it is a member of the set {0, 3, 6}.

Going one step further, if the entire card's number was represented as a 4 digit number we can simply add up the entire cards (in decimal not ternary):
```
  1001
  1222
+ 1112
------
  3335
```
In this example we see, that the last feature invalidates the triple, since (1,2,2) are neither all the same nor all different and the sum is 5.
Incidently, there are exactly 81 valid combinations, namely all four digit numbers with digits 0, 3 and 6. We can generate this set and use it as a lookup.
