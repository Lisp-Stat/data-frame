;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: DATA-FRAME -*-
;;; Copyright (c) 2026 by Symbolics Pte. Ltd. All rights reserved.
;;; SPDX-License-identifier: MS-PL
(in-package #:data-frame)

(defvar *default-df-search-packages* '(:ls-user :cl-user)
  "Package designators searched by default when looking up data frames.

The current *PACKAGE* is always appended and all NIL entries (missing
packages) are silently dropped.  Extend this list to expose data frames
from additional packages without modifying source code.

Example: (pushnew :my-data-pkg df:*default-df-search-packages*)")

(defun default-search-packages ()
  "Build the default package search list: *default-df-search-packages* plus *package*, deduplicated and with missing packages dropped."
  (remove-duplicates
   (remove nil
           (mapcar #'find-package
                   (append *default-df-search-packages*
                           (list *package*))))
   :from-end t :test #'eq))

(defun data-frame-symbols (&optional (packages (default-search-packages)))
  "Return a deduplicated list of symbols bound to a data-frame

Sorted by name, whose values are data frames, searching PACKAGES (a list of package designators or package objects).

This is the core primitive for all data-frame discovery.  Pass an explicit PACKAGES argument to widen or narrow the search:

  ;; Search only the current package
  (df:data-frame-symbols (list *package*))

  ;; Search an additional project package
  (df:data-frame-symbols
    (cons (find-package :my-project) (df:default-search-packages)))"
  (let (result)
    (dolist (pkg (mapcar #'find-package packages))
      (when pkg
        (do-symbols (sym pkg)
          ;; Only consider symbols whose home package is PKG to avoid
          ;; re-checking symbols inherited via :use (e.g. the entire
          ;; COMMON-LISP package).
          (when (and (eq (symbol-package sym) pkg)
                     (boundp sym)
                     (typep (symbol-value sym) 'data-frame))
            (pushnew sym result :test #'eq)))))
    (sort result #'string< :key #'symbol-name)))

(defun find-data-frame (name &optional (packages (default-search-packages)))
  "Return the symbol named NAME (case-insensitive) whose value is a
data frame, or NIL.

PACKAGES defaults to *default-df-search-packages* plus *package*."
  (find (string-upcase name)
        (data-frame-symbols packages)
        :key  #'symbol-name
        :test #'string=))
