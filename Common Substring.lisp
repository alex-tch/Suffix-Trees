;;; find the longest common substring in a multiple strings
(defun longest-common-substr (tree)
  (find-longest-string 
    (remove-duplicates 
      (mapcar #'concat-strings 
        (list-rep-str-detailed tree)) :test #'string=)))

;;; creates list of all repeated strings with leaf's numbers under the string
;;;  variables description:
;;;  list-of-suf-str - accumulates all common substrings
;;;  str-indexes  -  is a list with integers which shows range for leaves numbers
;;;  for every string. For example if root has list (n1 n2), where n1 is length
;;;  of the first string including delimeter and n2 - Length of the second string,
;;;  then all leaves of the first string would have index values <= n1,
;;;  all leaves of the second string would have index values > n1 and <= (n1 + n2) etc.."
;;;  num-of-trees  -  is a number of trees presented in the tree. 
;;;  It's equal to number of integers in the root list
(defun list-rep-str-detailed (tree)
  (let ((list-of-suf-str nil)  
        (num-of-trees (length (caar tree)))
        (str-indexes 
          (reverse (maplist #'(lambda (x) (apply '+ x)) (reverse (caar tree))))))

  (labels ((traverse-tree (tree new-suf-str)		; helper function
    ;; new-suf-str is a list with all node's values during the traversing tree
    (when tree
        ;; get index of each leaf under the current node (function list-of-leaves)
        ;; replace index with a tree number to which index does belong (determine-tree)
        ;; remove duplications and check the length of the resulting list
        ;; if length equal to number of trees, then need such suffix, otherwise reject.
        ;; (it means that such node does not appear at all trees)
        (when (= (length (remove-duplicates 
                 (mapcar #'(lambda (x) (determine-tree x str-indexes))
                    (list-of-leaves (list (car tree))))))
                  num-of-trees)
          (push (data tree) new-suf-str)
		  (traverse-tree (first-child tree) new-suf-str))

        ;; reached end of the branch  
        ;; store the list for this path and move to the next branch
        ;; add current list if it's not already in list 
        (unless (member (reverse new-suf-str) list-of-suf-str :test #'equal)
	      (push (reverse new-suf-str) list-of-suf-str))
        (pop new-suf-str)			; remove very last element

        (traverse-tree (next-sibling tree) new-suf-str)))) ; move to the next branch

      (traverse-tree tree ()))
    list-of-suf-str))

;;; determine to which tree node belongs
(defun determine-tree (node-num lst)
  ;;lst is a list wich shows numbers of nodes in each tree
  ;;  nodes from first tree have indexes less than first element of lst, 
  ;;  nodes from second tree have indexes greater than first element
  ;;    and less than second element,  etc...
  ;; need to determine to which tree belongs node with number node-num
  (dotimes (i (length lst))
    (when (< node-num (nth i lst))
	  (return-from determine-tree i))))
