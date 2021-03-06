// This example is from rosettacode.org
// Source: http://rosettacode.org/wiki/Fibonacci_sequence#C.2B.2B

#include <iostream>
 
int main()
{
        unsigned int a = 1, b = 1;
        unsigned int target = 48;
        for(unsigned int n = 3; n <= target; ++n)
        {
                unsigned int fib = a + b;
                std::cout << "F("<< n << ") = " << fib << std::endl;
                a = b;
                b = fib;
        }
 
        return 0;
}