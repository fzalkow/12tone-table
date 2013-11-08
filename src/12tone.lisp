;;; -*- mode: Common-Lisp; Base: 10 ; Syntax: ANSI-Common-Lisp ; coding: utf-8 -*-
;;; make 12 tone row table pdf files with LilyPond and LaTeX
;;; This work is licensed under a CC BY 3.0 license.
;;; http://creativecommons.org/licenses/by/3.0/
;;; Frank Zalkow, 2010-2013

(defun my-getenv (name &optional default)
    #+CMU
    (let ((x (assoc name ext:*environment-list*
                    :test #'string=)))
      (if x (cdr x) default))
    #-CMU
    (or
     #+Allegro (sys:getenv name)
     #+CLISP (ext:getenv name)
     #+ECL (si:getenv name)
     #+SBCL (sb-unix::posix-getenv name)
     #+LISPWORKS (lispworks:environment-variable name)
     default))

(defun temporary-directory ()
  (let ((system-tmpdir (coerce (my-getenv "TMPDIR") 'string)))
    (if (string= "" system-tmpdir) ; null or empty
        (make-pathname :directory '(:absolute "tmp"))
      (pathname (concatenate 'string system-tmpdir "/")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ASDF
(require 'asdf)
(unless (find-package 'asdf)
  (error "ASDF is not installed!"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ARBITRARY NOTE NAMES TO NOTE NUMBERS
(defun name2number (note)
  (let* ((diatonic '(("c" 0) ("d" 2) ("e" 4) ("f" 5) ("g" 7) ("a" 9) ("b" 11)))
         (notestr (symbol-name note))
         (nr (cadr (nth
                    (position (subseq notestr 0 1) diatonic :test #'string-equal :key #'car)
                    diatonic))))
    (dotimes (i (/ (- (length notestr) 1) 2) nr)
      (let ((substr (subseq notestr (+ 1 (* 2 i)) (+ 3 (* 2 i)))))
        (setq nr (+ nr (cond ((string-equal substr "is") 1)
                             ((string-equal substr "es") -1)
                             (T (error (concatenate 'string note " is not a valid note name!"))))))))))

(defun number2name (number)
  (nth number '("c" "cis" "d" "dis" "e" "f" "fis" "g" "aes" "a" "b" "bes")))

;; ROW MANIPULATIONS: transpositions, retrograde, inversion, retrograde-inversion
(defun transposition (row interval)
  (mapcar #'(lambda (n) (mod (+ n interval) 12)) row))

(defun retrograde (row)
  (reverse row))

(defun inversion (row &aux (first (car row)))
  (mapcar
   #'(lambda (n)
       (mod (+ (- first n) first) 12))
   row))

(defun retrograde-inversion (row)
  (retrograde (inversion row)))

(defun generate-code (row-name &optional (stream T) (paper 'c4))
  (let ((row-nr (mapcar #'name2number row-name)))
    ; converting a list of note numbers into a lilypond string
    (flet ((row2str (row)
             (let ((notes (mapcar #'(lambda (n)
                                      (let ((try (position n row-nr :test #'equalp)))
                                        (if try (nth try row-name) (number2name n))))
                                  row)))
               (string-downcase
                (format nil "~(~a~)'4 ~{~(~a~)'~^ ~}" (car notes) (cdr notes))))))
      (format stream "\\version \"2.12.3\"~%~%#(set-default-paper-size \"~(~a~)\" 'landscape)~%\\header {tagline = \"\"}~%~%\\markup {~%" paper)
      (dolist (func '(append retrograde inversion retrograde-inversion))
        (format stream "  \\left-column { ~s "
                (if (eq func 'append) "prime" (string-downcase (symbol-name func))))
        (dotimes (transp 12)
          (format stream "\\score{ \\new Staff { #(set-accidental-style 'dodecaphonic) \\override Staff.TimeSignature #'stencil = ##f \\override Stem #'transparent = ##t \\cadenzaOn \\time 12/4 ~a } \\layout{~a} } "
                  (row2str (transposition (funcall func row-nr) transp))
                  (if (eq paper 'a4) "#(layout-set-staff-size 16)" "")))
        (format stream " }~%"))
      (format stream " }~%"))))

(defun make-pdf (row outfile lilypond &key (paper 'c4))
  (labels ((make-temp-file ()
             (let ((tmp-file (merge-pathnames (temporary-directory)
                                              (format nil "12-tone-row-table~a.ly"
                                                      (random 1000000)))))
               (if (probe-file tmp-file) (make-temp-file) tmp-file))))
    (let ((ly-file (make-temp-file)))
      (with-open-file (filestream
                       ly-file
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
        (generate-code row filestream paper))
      (unless (probe-file ly-file)
        (error (concatenate 'string (namestring ly-file) " could not be created!~%")))
      (asdf:run-shell-command
       (format nil "~s -o ~s ~s" lilypond outfile (namestring ly-file)))
      (if (or (probe-file outfile) (probe-file (concatenate 'string outfile ".pdf")))
          (format t "PDF file has been created!~%")
        (format t "Could not create PDF file!~%"))
      (delete-file ly-file))))

(defun my-command-line ()
  (or 
   #+SBCL *posix-argv*  
   #+LISPWORKS system:*line-arguments-list*
   #+CMU extensions:*command-line-words*
   nil))

(defun argument-value (key argv &optional default)
  (let ((m (member key argv :test #'equalp)))
    (cond ((and m (second m))
           (second m))
          (m (format t "The ~a option requires a value~%." key))
          (T default))))

(defun string-row-to-symbol-list (string)
  (loop for i = 0 then (1+ j)
        as j = (position #\Space string :start i)
        collect (intern (string-upcase (subseq string i j)))
        while j))

(defun main (argv &aux ly row paper out)
  (flet ((help () (format t "12tone-table [--ly PATH] [--row \"ROW\"] [--paper FORMAT] [--help] --out PATH~%Creating PDF files with a table of all 48 variants of a twelve tone row. lilypond is required.~%~%Arguments~%   --ly     path to lilypond (default: /Applications/LilyPond.app/Contents/Resources/bin/lilypond)~%   --row    twelve tone row (default: \"bes e ges ees f a d cis g gis b c\")~%   --paper  A4 or C4 (default: C4)~%   --help   displays this help~%   --out    path to pdf file to be created~%")))
    (setf sb-ext:*invoke-debugger-hook* (lambda (condition previous-hook) (declare (ignore condition previous-hook))  (help) (exit)))
    (let ((row-default "bes e ges ees f a d cis g gis b c")
          (ly-default "/Applications/LilyPond.app/Contents/Resources/bin/lilypond")
          (paper-default "C4"))
      (when (member "--help" argv :test #'equalp)
        (help)
        (exit))
      (setq ly (argument-value "--ly" argv ly-default))
      (setq row (argument-value "--row" argv row-default))
      (setq paper (argument-value "--paper" argv paper-default))
      (unless (or (equalp paper "A4") (equalp paper "C4")) (help) (exit))
      (setf out (argument-value "--out" argv))
      (unless out (help) (exit))
      (unless (probe-file (directory-namestring out))
        (format t "The directory ~a doesn't exist.~%" (pathname-directory out)))
      (make-pdf (string-row-to-symbol-list row) out ly :paper (intern (string-upcase paper))))))

