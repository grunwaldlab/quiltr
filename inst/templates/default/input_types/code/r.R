# This example is from rosettacode.org
# Source: http://rosettacode.org/wiki/Fibonacci_sequence#R

funcfibo <- function(n) {
  if (n < 2) 
    n
  else {
    generator <- function(f, ...) {
      c(f[2], sum(f))
    }
    Reduce(generator, 2:n, c(0,1))[2]
  }
}

print.table(lapply(0:20, funcfibo))