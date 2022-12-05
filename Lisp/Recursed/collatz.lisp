#!/usr/bin/sbcl --script


;;; Read and return command line arguments list
(defun argv ()
  (or
     #+clisp (ext:argv)
     #+sbcl sb-ext:*posix-argv*
     nil))
                          
;;; Function that returns the collatz sequence length of a number
(defun find-sequence-length (num)
  (cond ((= num 1) 0)
        ((= (mod num 2) 0)
          (+ 1 (find-sequence-length (/ num 2))))
        (t (+ 1 (find-sequence-length (+ (* num 3) 1))))))



;;; Function that prints the arrays in the required fashion
(defun print-numbers-with-lengths (num-arr seqlen-arr)
  (let ((num-elems 0))
    (loop for x across seqlen-arr do
      (incf num-elems 1))
    (loop for i  from 0 to (- num-elems 1) do
      (format t "~21@a~21@a~%" (aref num-arr i) (aref seqlen-arr i))))) 

;;; Funtion to add elements to two size-10 arrays which are managed parallely
(defun add-to-arrays (num seqlen num-arr seqlen-arr)
  (let ((samelen 0)       ; samelen will be used as a boolean with 0(F) and 1(T) 
        (num-elems 0))
    (loop for x across seqlen-arr do
       (incf num-elems 1))
    (cond ((= num-elems 1)
            (vector-push num num-arr) 
            (vector-push seqlen seqlen-arr))
          ((< num-elems 10)
            (loop for x across seqlen-arr do
              (when (= x seqlen)
                (setf samelen 1)
                (return)))
            (when (= samelen 0)
              (vector-push num num-arr)
              (vector-push seqlen seqlen-arr))
            (when (= num-elems 10)
              (sort-by-seqlen num-arr seqlen-arr)))
          (t (loop for x across seqlen-arr do
              (when (= x seqlen)
                (setf samelen 1)
                (return)))
             (when (and (= samelen 0) (> seqlen (aref seqlen-arr 9)))
               (vector-pop num-arr)
               (vector-pop seqlen-arr)
               (vector-push num num-arr)
               (vector-push seqlen seqlen-arr)
               (sort-by-seqlen num-arr seqlen-arr))))))

;;; Funtion to sort array of sequence lengths and adjust corresponding integer
;;; accordingly. 
;;; Sorting algorithm used is bubble sort which should be efficient enough since
;;; arrays can only be a maximum of size 10.
(defun sort-by-seqlen (num-arr seqlen-arr)
  (let ((num-elems 0) (tmp))
     (loop for x across seqlen-arr do
       (incf num-elems 1))
     (loop for i from 0 to (- num-elems 1) do 
       (loop for j from 0 to (- num-elems 2) do 
         (when (< (aref seqlen-arr j) (aref seqlen-arr (+ j 1)))
            (setf tmp (aref seqlen-arr j))
            (setf (aref seqlen-arr j) (aref seqlen-arr (+ j 1)))
            (setf (aref seqlen-arr (+ j 1)) tmp)
            (setf tmp (aref num-arr j))
            (setf (aref num-arr j) (aref num-arr (+ j 1)))
            (setf (aref num-arr (+ j 1)) tmp))))))

;;; Function to sort array of the numbers themselves and adjust their corresponing
;;; collatz sequence lengths accordingly.
;;; This is similar to the previous function.     
(defun sort-by-num (num-arr seqlen-arr)
  (let ((num-elems 0) (tmp))
     (loop for x across seqlen-arr do
       (incf num-elems 1))
     (loop for i from 0 to (- num-elems 1) do
       (loop for j from 0 to (- num-elems 2) do
         (when (< (aref num-arr j) (aref num-arr (+ j 1)))
            (setf tmp (aref seqlen-arr j))
            (setf (aref seqlen-arr j) (aref seqlen-arr (+ j 1)))
            (setf (aref seqlen-arr (+ j 1)) tmp)
            (setf tmp (aref num-arr j))
            (setf (aref num-arr j) (aref num-arr (+ j 1)))
            (setf (aref num-arr (+ j 1)) tmp))))))

         
;;;; Main starts here.
(defvar a1 (parse-integer (nth 1 sb-ext:*posix-argv*)))
(defvar a2 (parse-integer (nth 2 sb-ext:*posix-argv*)))


  (let ((num-arr) (seqlen-arr))
    (setf num-arr (make-array 10 :fill-pointer 0))
    (setf seqlen-arr (make-array 10 :fill-pointer 0))
    (loop for num from a1 to a2 do
      (add-to-arrays num (find-sequence-length num) num-arr seqlen-arr))
    (format t "Sorted based on sequence length~%")
    (sort-by-seqlen num-arr seqlen-arr)
    (print-numbers-with-lengths num-arr seqlen-arr)
    (format t "Sorted based on integer size~%")
    (sort-by-num num-arr seqlen-arr)
    (print-numbers-with-lengths num-arr seqlen-arr)))

 
