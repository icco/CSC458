1) Pick your favorite high-performance language other than F\#.  C or C++ would
be the standard choices here, but you could also go with FORTRAN, Go, or
another native-code compiled language.

2) Flip a coin. On Heads, language A = compiled language, language B = F\#.

3) Make a note of the time.

4) Implement multi-period option valuation for options, using the framework we
established before. That is:

optionValue : int [periods] -> double [S_0] -> double [u] -> double [d] -> option -> double [resulting value]

5) We'll make up some benchmark test case in lab. Use this benchmark to see
what size of problem your implementation can handle in 5 seconds.

6) Make a note of the time again.

7) Do it again in language B.

8) Make a note of the time.

How did they compare?
