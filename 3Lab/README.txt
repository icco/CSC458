Nathaniel Welch
Jesse Tyler
Lab 3

1. An event is a sequence of heads and tails.

2. An equity has an initial price and a multiplier 1 < m < 2 to use on a
head, and (1 / m) on a tail.  (m is a world fixed value)

3. Develop the function makeEquity : double {a} -> event -> double that
accepts a value 'a' and returns a randomVariable.  (randomVariable =
(event -> double)) 

randomVariable is a mapping from events to values.

4. Develop the function Expected : int -> double -> randomVariable -> double
that computes the expected value over a given number of time steps of the
given random variable by exhaustive search.

5. Develop the function Variance : int -> double -> randomVariable -> double
that computes the variance over the given number of timesteps of the given
random Variable



