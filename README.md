# Suffix-Trees
This folders contains set of LISP programs for implementing suffix trees and suffix arrays with some standard functions.  
  
1. Create-Suffix-Tree.lisp main function which creates suffix tree using one or multiple strings. Need to setup global constant +delimeter+ which will be used for separating input strings e.g. (defconstant +delimeter+ "#").
Examples:   
(make-suffix-tree '("xabx")) generates list:  
&nbsp; (((4) ("x" ("abx" (NIL (0)))  
&emsp; &emsp; &emsp; &ensp; ("" (NIL (3))))  
&ensp; &ensp; &ensp; &ensp;  ("abx" (NIL (1)))  
&ensp; &ensp; &ensp; &ensp;  ("bx" (NIL (2)))))

  
(make-suffix-tree '("xabx" "cbxab")) generates list:    
&nbsp; (((5 6) ("x" ("ab" ("x#" (NIL (0))) ("#" (NIL (7))))  
&emsp; &emsp; &emsp; &emsp;  ("#" (NIL (3))))  
&emsp; &emsp; &ensp; ("ab" ("x#" (NIL (1))) ("#" (NIL (8))))  
&emsp; &emsp; &ensp; ("b" ("x" ("#" (NIL (2))) ("ab#" (NIL (6)))) ("#" (NIL (9))))  
&emsp; &emsp; &ensp; ("cbxab#" (NIL (5)))))  

    
2. Patterns.lisp contains functions for retrieving patterns.  
&emsp;  2.1. search-pattern function counts how many times pattern appears in the string.  
&emsp; &emsp; (search-pattern (make-sf-tree "aabaabaaabaa") "baa") returns 3  
&emsp; &emsp; (search-pattern (make-sf-tree "aabaabaaabaa") "baaa") returns 1  
&emsp; 2.2. list-of-pattern function returns list with all positions where pattern appears in the string.  
&emsp; &emsp; (list-of-pattern (make-sf-tree "aabaabaaabaa") "baa") returns list (2 5 9)
&emsp; &emsp; (list-of-pattern (make-sf-tree "aabaabaaabaa") "baaa") returns list (5)

3. Repeated String.lisp contains function longest-rep-str which retrieves the longest repeated substring.  
&emsp; (LONGEST-REPEATED-STR (make-sf-tree "banana"))  returns "ana"  
&emsp; (LONGEST-REPEATED-STR (make-sf-tree "aaaaa"))  returns "aaaa"  
  
4. Common Substring.lisp contains function longest-common-substr which retrieve the longest common substring in a multiple strings.  
&emsp; (LONGEST-COMMON-SUBSTR (make-gen-sf-tree '("xabxa" "babxba")))  returns "abx"  
&emsp; (LONGEST-COMMON-SUBSTR (make-gen-sf-tree '("xabxa" "babxba" "cab")))  returns "ab"
  
Suffix Array.lisp contains the following functions for working with suffix arrays:  
  
5. make-suffix-array generates suffix array for an input string as a  list of integers
&ensp; (make-suffix-array "banana")	returns list (5 3 1 0 4 2).  
6. longest-common-prefix retrieves the longest prefix for a two suffixes of a given string. It returns list with three integers:
First two values are indexes for the two suffixes, last number is a length of common prefix for these two suffixes.
&ensp; (longest-common-prefix "banana")	returns (3 1 3)   
7. number-of-diff-substr cakculates number of different substrings for a given string.  
&ensp; (NUMBER-OF-DIFF-SUBSTR "banana") returns 15   
8. k-mers find all k-mers of a given length in the input string. Output is a list of pairs, where the first element is a substring of the specified length and second element is a frequency for that substring.    
&ensp; (k-mers "banana" 2)  returnx  (("na" 2) ("ba" 1) ("an" 2))  
