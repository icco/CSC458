Nathaniel Welch
Assignment 3
CSC458 Spring 2010

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

