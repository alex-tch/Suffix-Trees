;;; Returns number of all occurences of pattern within given suffix tree
(defun search-pattern (tree pattern)
  (let ((child (search-subtree (first-child tree) pattern)))
    (if child
      ; first subtree was found, analyze and continue search
      (let* ((data-child (data child))
            (n (str-compr data-child pattern)))
        (COND
          ((= (length pattern) n) (count-leaves (list (car child))))
          ((> (length data-child) n) 0)
          (T (search-pattern child (subseq pattern n)))))
      ; first subtree was not found, hence zero occurences
      0)))

;;; creates list of all displacements where  pattern appeares in the text
(defun list-of-pattern (tree pattern)
  (let ((child (search-subtree (first-child tree) pattern)))
    (if child
      ; first subtree was found, analyze and continue search
      (let* ((data-child (data child))
            (n (str-compr data-child pattern)))
        (COND
          ((= (length pattern) n) (list-of-leaves (list (car child))))
          ((> (length data-child) n) 0)
          (T (list-of-pattern child (subseq pattern n)))))
      nil)))
