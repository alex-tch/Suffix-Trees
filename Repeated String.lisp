;;; find the longest repeated string
(defun longest-repeated-str (tree)
  (find-longest-string 
    (remove-duplicates 
      (mapcar #'concat-strings 
        (list-rep-str tree)) :test #'string=)))


;;; creates list of all repeated strings
(defun list-rep-str (tree)
  (let ((list-of-suf-str '("")))  ; accumulates all repeated substrings
  (labels ((trv-tree (tree new-suf-str)		; helper function
    (when tree
      ;; new-suf-str is a list with all nodes values during the traversing tree
      (push (data tree) new-suf-str)
      
      ;; when two or more nodes underneath still traverse down 
      (when (> (count-leaves (list (car tree))) 1)
        (trv-tree (first-child tree) new-suf-str))
	  
      ;; reached end of the branch  
      ;; (one node underneath, so the last node is not repeated))
      ;; store the list for this path and move to the next branch
      (pop new-suf-str)			; remove very last element
      ;; add current list if it's not already in list 
	  (unless (member (reverse new-suf-str) list-of-suf-str :test #'equal)
	     (push (reverse new-suf-str) list-of-suf-str))  
	  (trv-tree (next-sibling tree) new-suf-str)))) ; move to the next branch

    (trv-tree tree ()))
  list-of-suf-str))

