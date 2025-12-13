;;; Create suffix tree.
;;; input parameter str-list is a list with strings.
;;; When there are more than one input string, delimeter character
;;; would added to the end of each string, all such extended string
;;; concatenated and suffix tree created 
;;; If input list has only one string, then suffix tree will be created
;;; immediately, without adding delimeter character.
;;; root of the created tree contains list with lengths of each input string. 
;;; Length of this list is equal to number of input strings. 
;;; For multiple input strings length includes delimeter symbol.
;;; 
(defun make-suffix-tree (str-list)
  ;; str-list contains N input strings without delimeter symbol.
  (let* ((curr-index 0)
         (delimeter-ind (if (= 1 (length str-list)) 0 1))
         (str-1-list (if (= 1 (length str-list)) str-list ; original string 
                        (mapcar #'(lambda (x) ; add delimeter to the end of each string
                         (concatenate 'string x +delimeter+)) str-list)))
         (num-of-str (length str-list))
         (s-tree (make-tree (mapcar 'length str-1-list))))

    ;; loop through list of strings. add each string separately
    (dotimes (n num-of-str)
      (dotimes (i (- (length (nth n str-1-list)) delimeter-ind))
        (let ((suf-str (subseq (nth n str-1-list) i)))
          (update-child s-tree suf-str (+ curr-index i))))
	  (incf curr-index (length (nth n str-1-list))))
   s-tree))

(defun update-child (tree new-suff ind)
  (let ((child (search-subtree (first-child tree) new-suff)))
    (if child
      ; node for new suffix was found. analyze current level and go down if necessary
      (let* ((data-child (data child))
             (n (str-compr data-child new-suff)))
        (cond
          ;; new-suffix already exist on some level; create empty terminal leaf
          ((string= data-child new-suff)		
            (nconc (cdar child) (list (cons "" (list (cons nil (list (list ind))))))))
          ;; new-suffix is a part of current node
          ;; e.g. data-child is "abc", new-suff is "ab"
          ;; split current node and add empty terminal leaf
          ((= n (length new-suff))	
            (setf (caar child) (subseq data-child 0 n))
            (setf (car child) (cons (caar  child)
            (list (cons (subseq data-child n) (cdar child))
                  (cons (subseq new-suff n) (list (cons nil (list (list ind)))))))))
          ;; current node is a part of new-suffix. add new branch
          ;; e.g. data-child is "ab", new-suff is "abc"
          ((= n (length data-child))
            (update-child child (subseq new-suff n) ind))
          (T
            (setf (caar child) (subseq data-child 0 n))
            (setf (car child) (cons (caar  child) 
                                    (list (cons (subseq data-child n) (cdar child)))))
            (update-child child (subseq new-suff n) ind))))
      ; node for new suffix not found. add new branch/leaf
      (add-child tree
                (add-child (make-tree new-suff) (add-child (make-tree nil) (make-tree ind))))))
    tree)
