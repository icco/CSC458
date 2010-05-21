Nathaniel Welch
Jesse Tyler
Assignment 3
CSC458 Spring 2010

Our program is fully functional and well commented. To run tests uncomment the
last line of nr.fs.




Spec from Clements:

Newton Raphson:

   Here’s a data definition for single-variable polynomials,and the signature
   for a findZeros method.

   type term = (double * double) // coefficient and exponent
   type poly = term list
   //val findZeros : poly * double -> double

   The findZeros function should find double values for which the polynomial
   returns values that are very close to zero. How close? Your function should
   give up after 1000 iterations, or when the polynomial returns a value that is
   indistinguishable from zero.

   Also, it’s fine for your function to signal an error when the derivative at a
   tested point is zero (e.g. y = 4) or undefined (e.g. y = 1/x with x = 0).

Expectation:

   Using our existing data definitions for events and random variables, develop
   the expectedVal function that accepts an arbitrary random variable and a
   probability of flipping a head on each toss and produces the random
   variable’s expected value.

   type event1 = bool
   type event = int -> event1
   type rv = event -> double // a random variable
   //val expectedVal : rv -> double -> double

   For this problem, random variables must be actual functions: that is, they
   must be deterministic. Also, if the function representing a random variable
   fails to converge (a.k.a. loops forever) on a particular event, then it’s
   fine for your expectedVal function to do the same thing.

   Also, unsurprisingly, the probability of flipping a head must be in the
   range [0,1].
