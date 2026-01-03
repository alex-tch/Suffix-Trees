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
(((5 6) ("x" ("ab" ("x#" (NIL (0))) ("#" (NIL (7)))) ("#" (NIL (3))))  
        ("ab" ("x#" (NIL (1))) ("#" (NIL (8))))  
        ("b" ("x" ("#" (NIL (2))) ("ab#" (NIL (6)))) ("#" (NIL (9))))  
        ("cbxab#" (NIL (5)))))  

    
2. Patterns.lisp contains functions for retrieving patterns.
   2.1. search-pattern function counts how many times pattern appears in the string.
        (search-pattern (make-sf-tree "aabaabaaabaa") "baa") returns 3
        (search-pattern (make-sf-tree "aabaabaaabaa") "baaa") returns 1
   2.2. list-of-pattern function returns list with all positions where pattern appears in the string.
        (list-of-pattern (make-sf-tree "aabaabaaabaa") "baa") returns list (2 5 9)
        (list-of-pattern (make-sf-tree "aabaabaaabaa") "baaa") returns list (5)

3. Repeated String.lisp contains function longest-rep-str which retrieves the longest repeated substring.
   (LONGEST-REP-STR (make-sf-tree "banana"))  returns "ana"
   (LONGEST-REP-STR (make-sf-tree "aaaaa"))  returns "aaaa"

4. Common Substring.lisp contains function longest-common-substr which retrieve the longest common substring in a multiple strings.
   (LONGEST-COMMON-SUBSTR (make-gen-sf-tree '("xabxa" "babxba")))  returns "abx"
   (LONGEST-COMMON-SUBSTR (make-gen-sf-tree '("xabxa" "babxba" "cab")))  returns "ab"
