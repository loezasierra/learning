// \n means new line

// printf patterns:
// %% for literal %
// %c for character
// %s for string
// %d or %i for integer
// %o for octal (base-8)
// %x or %X for hexadecimal (base-16)
// %u for unsigned integer
// %f or %F for float
// %e or %E for exponent
// %a or %A for hexadecimal exponent
// %g or %G either %f or %a depending on the value and precision
// %n number of characters written so far
// %p pointer

// printf incorrect inputs
// %c prints ascii character given an int,
//  wanrs when given a float but prints ascii character after truncation
//  prints a weird character when given a string
// %s seg fault when given a char, int, or float
//  (trying to dereference the incorrect input as a pointer?)
// %i prints the ascii value of a char
//  prints a seemingly random number when given a string (maybe pointer?)
//  prints a seemingly random negative number given a float
// %o prints the octave of ascii value of a char
//  prints octave of seemingly random number given string(maybe pointer?)
//  prints seemingly random number given a float
// %x prints the hexadecmal of ascii value of a char
//  prints hex of seemingly random number given string (maybe pointer?)
//  prints seemingly random number given a float
// %u prints seemingly random number given a negative int
//  prints ascii value of a char
//  prints seemingly random number given a string (maybe pointer?)
//  prints seemingly random number given a float
// %f prints 0.000000 given an int, char, or string
// %e prints a seemingly random exponent number given an int
//  prints a seemingly random exponent number given a char
//  prints a seemingly random exponent number given a string (pointer?)
// %a prints seeminly random exponent number given an int
//  prints a seemingly random exponent number given a char
//  prints a seemingly random exponent number given a string (pointer?)
// %g prints a seemingly random exponent number given an int
//  prints a seemingly random exponent number given a char
//  prints a seemingly random exponend number given a string (pointer?)

// #ifndef = if not defined 

// #define = creates a macro, substituting all matching things
//  in your source code with your definition (except in comments, strings, etc.)

// Defined for Mac and Linux
// __APPLE__
// __linux 
