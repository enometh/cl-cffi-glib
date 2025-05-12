;;; ----------------------------------------------------------------------------
;;; cl-cffi-glib-sys.lisp
;;;
;;; Copyright (C) 2024 Dieter Kaiser
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the "Software"),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.
;;; ----------------------------------------------------------------------------

(defpackage :glib-sys
  (:use :iterate :common-lisp)
  (:export #:mklist
           #:get-current-package
           #:sys-path
           #:check-and-create-resources
           #:flatten
           #:with-foreign-string-array))

(in-package :glib-sys)

;; Ensure OBJ is a list
(defun mklist (obj)
  (if (listp obj)
      obj
      (list obj)))

;;; ----------------------------------------------------------------------------

#+mk-defsystem
(defun mk::search-for-component (system &optional (name system))
  (setq system (mk::find-system system :error))
  (block nil
    (or (mk::%mk-traverse
	 system
	 (labels ((rec (component)
		    (unless (find (mk::component-type component)
				  '(:defsystem :system :subsystem))
		      (if (equal (mk::canonicalize-system-name
				  (mk::component-name  component))
				 (mk::canonicalize-system-name name))
			  (return component)))))
	   #'rec)
	 nil
	 :never)
	system)))

#+mk-defsystem
(defun mk::component-source-dir (c)
  (if (find (mk::component-type c) '(:defsystem))
      (mk::component-source-root-dir c)
      (let ((f (mk::component-full-pathname c :source)))
	(if (find (mk::component-type c) '(:system :subsystem :module))
	    f
	    (directory-namestring f)))))
#||
(get-current-package)
(mk::component-source-dir (mk::search-for-component (get-current-package)))
(mk::component-source-pathname (mk::search-for-component (get-current-package)))
(mk::component-full-pathname  (mk::search-for-component (get-current-package))
			      :source)
(sys-path "foo" "cl-cffi-glib")
||#

(let ((current-package nil))

  ;; Get pathname for FILENAME in PACKAGE
  (defun sys-path (filename &optional (package current-package))
    #+(and asdf (not mk-defsystem))
    (asdf:system-relative-pathname package filename)
    #+mk-defsystem
    (progn
      (assert (mk::find-system package :load-or-nil))
      (let* ((c (mk::search-for-component package))
	     (d (mk::component-source-dir c))
	     (r (merge-pathnames (or filename "") d)))
	#+sbcl (sb-ext:native-namestring r)
	#-sbcl r)))

  (defun get-current-package ()
    current-package)

  (defun (setf get-current-package) (value)
    (setf current-package value))

  ;; Check for RESOURCE in PACKAGE, create or update it when neccessary and
  ;; return the pathname of TARGET.
  (defun check-and-create-resources (resource &key (package current-package)
                                                   (sourcedir nil)
                                                   (verbose nil))
    (let* ((source (truename (sys-path resource package)))
           (sourcedir (directory-namestring (truename (sys-path sourcedir package))))
           (name1 (pathname-name resource))
           (name (if (string= "gresource" (pathname-type name1))
                     (pathname-name name1)
                     name1))
           (target (sys-path (make-pathname :name name
                                            :directory
                                            (pathname-directory resource)
                                            :type "gresource")
                             package)))

      (unless (and (probe-file target)
                   (> (file-write-date target)
                      (file-write-date source)))
        (when verbose
          (format t "Run GLIB-COMPILE-RESOUCRES~%")
          (format t "    source : ~a~%" source)
          (format t " sourcedir : ~a~%" sourcedir)
          (format t "      name : ~a~%" name)
          (format t "    target : ~a~%" target))
        (uiop:run-program (list "glib-compile-resources"
                                "--sourcedir"
                                sourcedir
                                (namestring source))
			  :output t
			  :error-output :output
			  ))
      (truename target)))
)

;;; ----------------------------------------------------------------------------

(defun flatten (tree)
  (let (lst)
    (labels ((traverse (subtree)
               (when subtree
                 (if (consp subtree)
                     (progn
                       (traverse (car subtree))
                       (traverse (cdr subtree)))
                     (push subtree lst)))))
      (traverse tree))
    (nreverse lst)))

;;; ----------------------------------------------------------------------------

(defmacro with-foreign-string-array ((strptr strs) &body body)
  (let ((i (gensym))
        (str (gensym)))
    `(let ((,strptr (cffi:foreign-alloc :pointer :count (1+ (length ,strs)))))
       (iter (for ,i from 0)
             (for ,str in ,strs)
             (setf (cffi:mem-aref ,strptr :pointer ,i)
                   (cffi:foreign-string-alloc ,str)))
       (setf (cffi:mem-aref ,strptr :pointer (length ,strs))
             (cffi:null-pointer))
       (unwind-protect
         (progn ,@body)
         (progn
           (iter (for ,i from 0)
                 (repeat (1- (length ,strs)))
                 (cffi:foreign-string-free (cffi:mem-aref ,strptr
                                                          :pointer ,i)))
           (cffi:foreign-string-free ,strptr))))))

;;; --- End of file c-cffi-glib-sys.lisp ---------------------------------------
