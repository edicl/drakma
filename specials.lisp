;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: DRAKMA; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/drakma/specials.lisp,v 1.19 2008/01/14 01:57:02 edi Exp $

;;; Copyright (c) 2006-2009, Dr. Edmund Weitz.  All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :drakma)

(defmacro define-constant (name value &optional doc)
  "A version of DEFCONSTANT for, cough, /strict/ CL implementations."
  ;; See <http://www.sbcl.org/manual/Defining-Constants.html>
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))

(define-constant +latin-1+ (make-external-format :latin-1 :eol-style :lf)
  "Default external format when reading headers.")

(define-constant +redirect-codes+ '(301 302 303 307)
  "A list of all HTTP return codes that redirect us to another URI.")

(define-constant +known-methods+ '(:copy
                                   :delete
                                   :delete
                                   :get
                                   :head
                                   :lock
                                   :mkcol
                                   :move
                                   :options
                                   :options*
                                   :post
                                   :propfind
                                   :proppatch
                                   :put
                                   :trace
                                   :unlock)
  "The HTTP methods \(including WebDAV methods) Drakma knows.")

(defconstant +buffer-size+ 8192)

(defvar *drakma-default-external-format* ':latin-1
  "The default value for the external format keyword arguments of
HTTP-REQUEST.")

(defvar *header-stream* nil
  "If this variable is not NIL, it should be bound to a stream to
which incoming and outgoing headers will be written for debugging
purposes.")

(defvar *allow-dotless-cookie-domains-p* nil
  "When this variable is not NIL, cookie domains containing no dots
are considered valid.  The default is NIL, meaning to disallow such
domains except for \"localhost\".")

(defvar *ignore-unparseable-cookie-dates-p* nil
  "Whether Drakma is allowed to treat `Expires' dates in cookie
headers as non-existent if it can't parse them.  If the value of this
variable is NIL \(which is the default), an error will be signalled
instead.")

(defvar *text-content-types* '(("text" . nil))
  "A list of conses which are used by DETERMINE-BODY-FORMAT to decide
whether a `Content-Type' header denotes text content.  The car and cdr
of each cons should each be a string or NIL.  A content type matches
one of these entries \(and thus denotes text) if the type part is
STRING-EQUAL to the car or if the car is NIL and if the subtype part
is STRING-EQUAL to the cdr or if the cdr is NIL.")

(defvar *body-format-function* 'determine-body-format
  "A function which determines whether the content body returned by
the server is text and should be treated as such or not.  The function
is called after the request headers have been read and it must accept
two arguments, HEADERS and EXTERNAL-FORMAT-IN where HEADERS is like
the third return value of HTTP-REQUEST while EXTERNAL-FORMAT-IN is the
HTTP-REQUEST argument of the same name.  It should return NIL if the
body should be regarded as binary content, or a FLEXI-STREAMS external
format \(which will be used to read the body) otherwise.

This function will only be called if the FORCE-BINARY argument to
HTTP-REQUEST was NIL.")

(defvar *time-zone-map*
  ;; list taken from
  ;; <http://www.timeanddate.com/library/abbreviations/timezones/>
  '(("A" . -1)
    ("ACDT" . -10.5)
    ("ACST" . -9.5)
    ("ADT" . 3)
    ("AEDT" . -11)
    ("AEST" . -10)
    ("AKDT" . 8)
    ("AKST" . 9)
    ("AST" . 4)
    ("AWDT" . -9)
    ("AWST" . -8)
    ("B" . -2)
    ("BST" . -1)
    ("C" . -3)
    ("CDT" . 5)
    ("CEDT" . -2)
    ("CEST" . -2)
    ("CET" . -1)
    ("CST" . -10.5)
    ("CST" . -9.5)
    ("CST" . 6)
    ("CXT" . -7)
    ("D" . -4)
    ("E" . -5)
    ("EDT" . 4)
    ("EEDT" . -3)
    ("EEST" . -3)
    ("EET" . -2)
    ("EST" . -11)
    ("EST" . -10)
    ("EST" . 5)
    ("F" . -6)
    ("G" . -7)
    ("GMT" . 0)
    ("H" . -8)
    ("HAA" . 3)
    ("HAC" . 5)
    ("HADT" . 9)
    ("HAE" . 4)
    ("HAP" . 7)
    ("HAR" . 6)
    ("HAST" . 10)
    ("HAT" . 2.5)
    ("HAY" . 8)
    ("HNA" . 4)
    ("HNC" . 6)
    ("HNE" . 5)
    ("HNP" . 8)
    ("HNR" . 7)
    ("HNT" . 3.5)
    ("HNY" . 9)
    ("I" . -9)
    ("IST" . -1)
    ("K" . -10)
    ("L" . -11)
    ("M" . -12)
    ("MDT" . 6)
    ("MESZ" . -2)
    ("MEZ" . -1)
    ("MST" . 7)
    ("N" . 1)
    ("NDT" . 2.5)
    ("NFT" . -11.5)
    ("NST" . 3.5)
    ("O" . 2)
    ("P" . 3)
    ("PDT" . 7)
    ("PST" . 8)
    ("Q" . 4)
    ("R" . 5)
    ("S" . 6)
    ("T" . 7)
    ("U" . 8)
    ("UTC" . 0)
    ("V" . 9)
    ("W" . 10)
    ("WEDT" . -1)
    ("WEST" . -1)
    ("WET" . 0)
    ("WST" . -9)
    ("WST" . -8)
    ("X" . 11)
    ("Y" . 12)
    ("Z" . 0))
  "An alist which maps time zone abbreviations to Common Lisp
timezones.")

;; stuff for Nikodemus Siivola's HYPERDOC
;; see <http://common-lisp.net/project/hyperdoc/>
;; and <http://www.cliki.net/hyperdoc>
;; also used by LW-ADD-ONS

(defvar *hyperdoc-base-uri* "http://weitz.de/drakma/")

(let ((exported-symbols-alist
       (loop for symbol being the external-symbols of :drakma
             collect (cons symbol
                           (concatenate 'string
                                        "#"
                                        (string-downcase symbol))))))
  (defun hyperdoc-lookup (symbol type)
    (declare (ignore type))
    (cdr (assoc symbol
                exported-symbols-alist
                :test #'eq))))
               
