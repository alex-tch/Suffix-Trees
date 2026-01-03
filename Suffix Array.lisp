(defun make-suffix-array (str)
  ;; returns list of suffix array for a given string
  (let ((data-lst nil)
         (str-lth (length str)))
    ;; loop through list of strings. add each string separately
    (dotimes (n str-lth)
      (push (list n (subseq str n)) data-lst))
    (mapcar 'car (sort data-lst #'string< :key #'cadr))))

(defun	longest-common-prefix (str)
  ;; Returns list with three integers. 
  ;; First two values are indexes for the two suffixes
  ;; last number is a length of common prefix for these two suffixes
  (let* ((curr-lcp (list 0 0 0))     ; initial value for the LCP list
         (suf-arr (make-suffix-array str))
         (n (- (length suf-arr) 1)))
    (dotimes (i n)
      ; find LCP for adjacent indices
      (let* ((ind1 (nth i suf-arr))
             (ind2 (nth (+ i 1) suf-arr))
             (prefix-lth         ; length of common prefix for adjacent indices
	           (str-compr (subseq str ind1) (subseq str ind2))))
        (when (> prefix-lth (third curr-lcp))
		  ; length of new common prefix is greater than current value
          (setf (first curr-lcp) ind1)          ; index for the first suffix
          (setf (second curr-lcp) ind2)         ; index for the second suffix
          (setf (third curr-lcp) prefix-lth)))) ; length of the common prefix
	curr-lcp))

(defun number-of-diff-substr (str)
  ;; returns number of different substring of the given string
  ;; any substring is a prefix of some suffix.
  ;; iterate the suffix array. 
  ;; for any suffix the total number of prefixes is equal to the length of the suffix
  ;; In order to find out which of them have already occurred in the previous suffixes,
  ;; subtract the LCP of this suffix with the previous one.
  
  ;; loop through all adjacent indices
  ;; sum up lengths of LCP for the corresponding suffixes
  (let* ((total-lcp 0)
         (k (length str))
         (suf-arr (make-suffix-array str))
         (n (- (length suf-arr) 1)))
    (dotimes (i n)
      (incf total-lcp (str-compr (subseq str (nth i suf-arr))
                                 (subseq str (nth (+ i 1) suf-arr)))))

  ;; sum of length of all suffixes is (1+2+...+k)=k*(k+1)/2,
  ;; where k is a length of the given string
  (- (/ (* k (+ k 1)) 2) total-lcp)))

(defun k-mers (str k)
  ;; returns list of pairs (substring with length k, frequency of that substring)
  (let ((suf-arr (make-suffix-array str))
        (k-mers-lst nil)
        (curr-k-mer "")
        (curr-freq 0))
    (dotimes (i (length suf-arr))
      (when (>= (length (subseq str (nth i suf-arr))) k)      ; skip all elements with length <k
        (let ((new-k-mer (subseq str (nth i suf-arr) (+ (nth i suf-arr) k))))
          (cond
            ((string= curr-k-mer "")       ; begin new k-mer
              (setf curr-k-mer new-k-mer)
              (incf curr-freq))
            ((string= curr-k-mer new-k-mer)  
              ; new element has the same k-mer, increase frequency
              (incf curr-freq))
            (T  ; new k-mer, store the previous one in the list and start new cycle
              (push (list curr-k-mer curr-freq) k-mers-lst)
              (setf curr-k-mer new-k-mer)
              (setf curr-freq 1))))))
    (push (list curr-k-mer curr-freq) k-mers-lst)
    k-mers-lst))
