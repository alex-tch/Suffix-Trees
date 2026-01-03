# Suffix-Trees
This folders contains set of LISP programs for implementing suffix trees and some standard functions with them.
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

    
3. Patterns.lisp contains functions for retrieving patterns.  
&emsp;  2.1. search-pattern function counts how many times pattern appears in the string.  
&emsp; &emsp; (search-pattern (make-sf-tree "aabaabaaabaa") "baa") returns 3  
&emsp; &emsp; (search-pattern (make-sf-tree "aabaabaaabaa") "baaa") returns 1  
&emsp; 2.2. list-of-pattern function returns list with all positions where pattern appears in the string.  
&emsp; &emsp; (list-of-pattern (make-sf-tree "aabaabaaabaa") "baa") returns list (2 5 9)
&emsp; &emsp; (list-of-pattern (make-sf-tree "aabaabaaabaa") "baaa") returns list (5)

4. Repeated String.lisp contains function longest-rep-str which retrieves the longest repeated substring.  
&emsp; (LONGEST-REP-STR (make-sf-tree "banana"))  returns "ana"  
&emsp; (LONGEST-REP-STR (make-sf-tree "aaaaa"))  returns "aaaa"  
  
5. Common Substring.lisp contains function longest-common-substr which retrieve the longest common substring in a multiple strings.  
&emsp; (LONGEST-COMMON-SUBSTR (make-gen-sf-tree '("xabxa" "babxba")))  returns "abx"  
&emsp; (LONGEST-COMMON-SUBSTR (make-gen-sf-tree '("xabxa" "babxba" "cab")))  returns "ab"

  
# Suffix-Arrays  
