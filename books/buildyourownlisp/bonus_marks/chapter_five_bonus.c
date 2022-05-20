// Doge string examples:
// such c so language wow build many book
// many language so c such lisp wow build so c

// Back slashes in front of quote marks:
// To escape the quote marks, so that they don't end
//  the long string of the grammar declaration

// Back slashes at end of lines:
// Allows you to have a multi-line string

// Decimal number grammar:
// Integer is number between 0 - 9
// <more than one Integer> . <more than one Integer>

// Web URL grammar:
// Protocal is one of: http, https
// Site is: <more than one letter or number>
// Domain is one of: .com, .net, .org, .edu, .gov
// URL is: <Protocal>://www.<Site>.<Domain>

// Formal Decimal:
/* 
    integer : "0" | "1" | "2" | "3" | "4" | "5" |
              "6" | "7" | "8" | "9" | "0"
    decimal : <integer>+ . <integer>+
*/

// Formal URL:
/* 
    protocal : "http" | "https"
    alphanum : "a" - "z" | "0" - "9"
    site     : <alphanum>+
    domain   : "com" | "net" | "org" | "edu" | "gov"
    url      : <protocal> ://www. <site> . <domain>
*/
