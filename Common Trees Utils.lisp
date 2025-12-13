; define a function to create a tree node with an item
(defun make-tree (item)
   "it creates a new node with item."
   (cons (cons item nil) nil))

(defun add-child (tree child)
  "Takes two nodes created with 'make-tree' and adds the
  second node as a child of the first. Returns the first node,
  which will be modified."
  (nconc (car tree) child)
  tree)

(defun first-child (tree)
  (when (listp tree)
    (cdr (car tree))))

; define a function to get sibling of the tree
(defun next-sibling (tree)
  (cdr tree))

; define a function to get the data of a tree node
(defun data (tree)
  (car (car tree)))

(defun traverse (tree &optional (padding 0))
  (when tree
    (format t "~&~v@TData: ~A" padding (data tree))
    (when (first-child tree)
      (format t "  Children: ~A"
              (maplist #'(lambda (x) (data x))
                       (first-child tree))))
    (traverse (first-child tree) (+ padding 3))
    (traverse (next-sibling tree) padding)))

;;; find node under the root which starts from the first letter of the str parameter 
(defun search-subtree (tree str)
  (cond
    ((null tree) nil)
    ((string= "" (caar tree)) nil)
    ((null (caar tree)) nil)
    ((string= (subseq (data tree) 0 1) (subseq str 0 1)) tree)
    (T (search-subtree (cdr tree) str))))

;;; calculate number of leaves in the tree
(defun count-leaves (tree)
  (cond ((null tree) 0)
        ((null (caar tree)) 1)
        (T (+ (count-leaves (first-child tree)) (count-leaves (next-sibling tree))))))

;;; creates list with leave's numbers (suffix indices)
(defun list-of-leaves (tree)
  (cond ((null tree) nil)
        ((null (caar tree)) (cadar tree))
        (T (sort (append (list-of-leaves (first-child tree)) (list-of-leaves (next-sibling tree))) #'<))))
