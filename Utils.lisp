;;; Find the longest string within given list of strings
(defun find-longest-string (str-list)
  (let ((longest ""))
    (dolist (str str-list)
      (when (> (length str) (length longest))
        (setq longest str)))
    longest))

;;; concatenate all string elements from a given list 
(defun concat-strings (list)
  (if (> (length (remove-if-not #'stringp list)) 0)
    (apply #'concatenate 'string
           (remove-if-not #'stringp list))
    ""))

;;; find number of common characters in the prefixes of two strings	
(defun str-compr (str1 str2)
  (cond 
    ((= 0 (length str1))  0)
    ((= 0 (length str2))  0)
    ((char/= (aref str1 0) (aref str2 0))  0)
    (T 	(+ 1 (str-compr (subseq str1 1) (subseq str2 1))))))
