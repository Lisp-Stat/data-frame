;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: DATA-FRAME-TESTS -*-
;;; Copyright (c) 2020 by Symbolics Pte. Ltd. All rights reserved.

(cl:defpackage #:data-frame-tests
  (:use
   #:cl
   #:alexandria
   #:anaphora
   #:clunit
   #:let-plus
   #:cl-slice
   #:data-frame)
  (:import-from #:clnu #:as-alist #:as-plist)
  (:export #:run))

(cl:in-package #:data-frame-tests)

(defsuite data-frame ())

(defun run (&optional interactive?)
  (run-suite 'data-frame :use-debugger interactive?))

(defsuite data-vector (data-frame))

(deftest data-vector-basics (data-vector)
  (let ((dv (dv :a 1 :b 2 :c 3)))
    (assert-equalp '(:a 1 :b 2 :c 3) (as-plist dv))
    (assert-equalp #(1 2 3) (columns dv))
    (assert-equalp #(:a :b :c) (keys dv))
    (assert-equalp '((:a . 1) (:b . 2) (:c . 3)) (as-alist dv))
    (assert-equalp '(:a 1 :b 2) (as-plist (slice dv #(:a :b))))
    (assert-equalp 3 (slice dv :c))
    (let ((dv2 (map-columns dv #'1+)))
      (assert-equalp '(:a 2 :b 3 :c 4) (as-plist dv2))
      (assert-true (typep dv2 'data-vector)))))

(defsuite data-frame-basics (data-frame))

(deffixture data-frame-basics (@body)
  (let ((v #(1 2 3 4))
        (b #*0110)
        (s #(a b c d)))
    @body))

(deftest data-frame-creation (data-frame-basics)
  (let* ((plist `(:vector ,v :symbols ,s))
         (df (apply #'df plist))
         (df-plist (plist-df plist))
         (df-alist (alist-df (plist-alist plist))))
    (assert-equalp #(:vector :symbols) (keys df))
    (assert-equalp (vector v s) (columns df))
    (assert-equalp (vector v s) (columns df t))
    (assert-equalp (vector v) (columns df #(:vector)))
    (assert-equalp v (columns df :vector))
    (assert-equalp v (columns df -2))
    (assert-equalp `(:vector ,v :symbols ,s) (as-plist df))
    (assert-equalp `((:vector . ,v) (:symbols . ,s)) (as-alist df))
    (assert-equalp (as-alist df) (as-alist df-plist))
    (assert-equalp (as-alist df) (as-alist df-alist))))

(deftest data-frame-slice (data-frame-basics)
  (let ((df (df :vector v :symbols s)))
    (assert-equalp `(:vector ,v) (as-plist (slice df t #(:vector))))
    (assert-equalp `(:vector ,(slice v b)) (as-plist (slice df b #(0))))
    (assert-equalp (slice v b) (slice df b :vector))
    (assert-equalp '(:vector 3 :symbols c) (as-plist (slice df 2 t)))
    (assert-equalp `(:vector #(2 4)) (as-plist
                                      (slice df
                                             (mask-rows df :vector #'evenp)
                                             #(:vector))))
    (assert-equalp #(2 4) (slice df (mask-rows df :vector #'evenp) :vector))))

(deftest data-frame-map (data-frame-basics)
  (let+ ((df (df :a #(2 3 5)
                 :b #(7 11 13)))
         (product #(14 33 65))
         ((&flet predicate (a b) (<= 30 (* a b))))
         ((&flet predicate-bit (a b) (if (predicate a b) 1 0)))
         (mask #*011))
    (assert-equalp product
        (map-rows df '(:a :b) #'*))
    (assert-equalp `(:p ,product :m ,mask)
        (as-plist (map-df df '(:a :b)
                          (lambda (a b)
                            (vector (* a b) (predicate-bit a b)))
                          '((:p fixnum) (:m bit)))))
    (let ((mask-rows (mask-rows df '(:a :b) #'predicate)))
      (assert-equal mask mask-rows)
      (assert-eq 'bit (array-element-type mask-rows)))
    (assert-equalp (count 1 mask)
        (count-rows df '(:a :b) #'predicate))))

(deftest print-object (data-frame-basics)
  (let ((df (df :a v :b b :c s)))
    (assert-true (with-output-to-string (stream)
                   (print-object df stream)))))
;;;

(defsuite data-frame-add (data-frame))

(deffixture data-frame-add (@body)
  (let* ((plist1 '(:a #(1 2 3)))
         (plist2 '(:b #(4 5 6)))
         (plist12 (append plist1 plist2)))
    @body))

(defmacro test-add (add-function plist1 plist2 append?)
  "Macro for generating the following test:

  1. create a data frame using plist1,

  2. add plist2 using add-function to get a second data frame,

  3. test that the first data frame is uncorrupted if append? is nil, or
     equivalent the concatenated plist otherwise,

  4. test that the second data frame is equivalent to the concatenated plist.

This is a pretty comprehensive test of the add-column family of functions,
destructive or non-destructive."
  (with-unique-names (df df2 plist12)
    (once-only (plist1 plist2)
      `(let* ((,df (plist-df ,plist1))
              (,df2 (apply ,add-function ,df ,plist2))
              (,plist12 (append ,plist1 ,plist2)))
         (assert-equal (if ,append?
                           ,plist12
                           ,plist1)
             (as-plist ,df))
         (assert-equal ,plist12
             (as-plist ,df2))))))

(deftest add-column (data-frame-add)
    (test-add #'add-columns plist1 plist2 nil)
    (test-add #'add-column! plist1 plist2 t)
    (test-add #'add-columns! plist1 plist2 t))

(deftest add-map (data-frame-add)
  (let* ((plist3 '(:c #(4 10 18)))
         (plist123 (append plist12 plist3)))
    ;; non-destructive
    (let* ((df (plist-df plist12))
           (df2 (add-columns df :c (map-rows df '(:a :b) #'*))))
      (assert-equalp plist12 (as-plist df))
      (assert-equalp plist123 (as-plist df2)))
    ;; destructive, function
    (let* ((df (plist-df plist12))
           (df2 (add-column! df :c (map-rows df '(:a :b) #'*))))
      (assert-equalp plist123 (as-plist df))
      (assert-equalp plist123 (as-plist df2)))))

;;; replace-column

(defsuite replace-column (data-frame))

(deftest replace-column1 (replace-column)
  (let* ((plist '(:a #(1 2 3) :b #(5 7 11)))
         (df (plist-df plist))
         (df-copy (copy df))
         (df1 (replace-column df :a #'1+))
         (df2 (replace-column df :a #(2 3 4)))
         (expected-plist '(:a #(2 3 4) :b #(5 7 11))))
    (assert-equalp expected-plist (as-plist df1))
    (assert-equalp expected-plist (as-plist df2))
    (assert-equalp plist (as-plist df))
    ;; modify destructively
    (replace-column! df :a #'1+)
    (assert-false (equalp plist (as-plist df)))
    (assert-equalp expected-plist (as-plist df))))
