;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: DRAKMA; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/drakma/cookies.lisp,v 1.15 2008/01/14 01:57:01 edi Exp $

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

(defclass cookie ()
  ((name :initarg :name
         :initform (cookie-error nil "A cookie must have a name.")
         :accessor cookie-name
         :documentation "The name of the cookie.")
   (value :initarg :value
          :initform ""
          :accessor cookie-value
          :documentation "The cookie's value.")
   (domain :initarg :domain
           :initform (cookie-error nil "A cookie must have a domain.")
           :accessor cookie-domain
           :documentation "The domain the cookie is valid for.")
   (path :initarg :path
         :initform "/"
         :accessor cookie-path
         :documentation "The path prefix the cookie is valid for.")
   (expires :initarg :expires
            :initform nil
            :accessor cookie-expires
            :documentation "When the cookie expires.  A Lisp
universal time or NIL.")
   (securep :initarg :securep
            :initform nil
            :accessor cookie-securep
            :documentation "Whether the cookie must only be
transmitted over secure connections.")
   (http-only-p :initarg :http-only-p
                :initform nil
                :accessor cookie-http-only-p
                :documentation "Whether the cookie should not be
accessible from Javascript.

This is a Microsoft extension that has been implemented in Firefox as
well. See <http://msdn2.microsoft.com/en-us/library/ms533046.aspx>."))
  (:documentation "Elements of this class represent HTTP cookies."))

(defun render-cookie-date (time)
  "Returns a string representation of the universal time TIME
which can be used for cookie headers."
  (multiple-value-bind (second minute hour date month year weekday)
      (decode-universal-time time 0)
    (format nil "~A, ~2,'0d-~2,'0d-~4,'0d ~2,'0d:~2,'0d:~2,'0d GMT"
            (aref #("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun") weekday)
            date month year hour minute second)))

(defmethod print-object ((cookie cookie) stream)
  "Prints a representation of COOKIE similar to a `Set-Cookie' header."
  (print-unreadable-object (cookie stream :type t)
    (with-slots (name value expires path domain securep http-only-p)
        cookie
      (format stream "~A~@[=~A~]~@[; expires=~A~]~@[; path=~A~]~@[; domain=~A~]~@[; secure~]~@[; HttpOnly~]"
              name (and (plusp (length value)) value)
              (and expires (render-cookie-date expires))
              path domain securep http-only-p))))
              
(defun normalize-cookie-domain (domain)
  "Adds a dot at the beginning of the string DOMAIN unless there
is already one."
  (cond ((starts-with-p domain ".") domain)
        (t (format nil ".~A" domain))))

(defun valid-cookie-domain-p (domain)
  "Checks if the string DOMAIN contains enough dots to be
acceptable.  If *ALLOW-DOTLESS-COOKIE-DOMAINS-P* is non-NIL,
every domain name is considered acceptable."
  (or *allow-dotless-cookie-domains-p*
      (string-equal domain "localhost")
      (> (count #\. (normalize-cookie-domain domain) :test #'char=) 1)))

(defun cookie-domain-matches (domain uri)
  "Checks if the domain DOMAIN \(a string) matches the \(PURI) URI URI."
  (ends-with-p (normalize-cookie-domain (uri-host uri))
               (normalize-cookie-domain domain)))

(defun send-cookie-p (cookie uri force-ssl)
  "Checks if the cookie COOKIE should be sent to the server
depending on the \(PURI) URI URI and the value of FORCE-SSL \(as
in HTTP-REQUEST)."
  (and ;; check domain
       (cookie-domain-matches (cookie-domain cookie) uri)
       ;; check path
       (starts-with-p (uri-path uri) (cookie-path cookie))
       ;; check expiry date
       (let ((expires (cookie-expires cookie)))
         (or (null expires)
             (> expires (get-universal-time))))
       ;; check if connection must be secure       
       (or (null (cookie-securep cookie))
           force-ssl
           (eq (uri-scheme uri) :https))))

(defun check-cookie (cookie)
  "Checks if the slots of the COOKIE object COOKIE have valid values
and raises a corresponding error of type COOKIE-ERROR otherwise."
  (with-slots (name value domain path expires)
      cookie
    (unless (and (stringp name) (plusp (length name)))
      (cookie-error cookie "Cookie name ~S must be a non-empty string." name))
    (unless (stringp value)
      (cookie-error cookie "Cookie value ~S must be a non-empty string." value))
    (unless (valid-cookie-domain-p domain)
      (cookie-error cookie "Invalid cookie domain ~S." domain))
    (unless (and (stringp path) (plusp (length path)))
      (cookie-error cookie "Cookie path ~S must be a non-empty string." path))
    (unless (or (null expires)
                (and (integerp expires)
                     (plusp expires)))
      (cookie-error cookie "Cookie expiry ~S should have been NIL or a universal time." expires))))

(defmethod initialize-instance :after ((cookie cookie) &rest initargs)
  "Check cookie validity after creation."
  (declare (ignore initargs))
  (check-cookie cookie))

(defmethod (setf cookie-name) :after (new-value (cookie cookie))
  "Check cookie validity after name change."
  (declare (ignore new-value))
  (check-cookie cookie))

(defmethod (setf cookie-value) :after (new-value (cookie cookie))
  "Check cookie validity after value change."
  (declare (ignore new-value))
  (check-cookie cookie))

(defmethod (setf cookie-domain) :after (new-value (cookie cookie))
  "Check cookie validity after domain change."
  (declare (ignore new-value))
  (check-cookie cookie))

(defmethod (setf cookie-path) :after (new-value (cookie cookie))
  "Check cookie validity after path change."
  (declare (ignore new-value))
  (check-cookie cookie))

(defmethod (setf cookie-expires) :after (new-value (cookie cookie))
  "Check cookie validity after expiry change."
  (declare (ignore new-value))
  (check-cookie cookie))

(defun cookie= (cookie1 cookie2)
  "Returns true if the cookies COOKIE1 and COOKIE2 are equal.
Two cookies are considered to be equal if name and path are
equal."
  (and (string= (cookie-name cookie1) (cookie-name cookie2))
       (string= (cookie-path cookie1) (cookie-path cookie2))))

(defclass cookie-jar ()
  ((cookies :initarg :cookies
            :initform nil
            :accessor cookie-jar-cookies
            :documentation "A list of the cookies in this cookie jar."))
  (:documentation "A COOKIE-JAR object is a collection of cookies."))

(defmethod print-object ((cookie-jar cookie-jar) stream)
  "Print a cookie jar, showing the number of cookies it contains."
  (print-unreadable-object (cookie-jar stream :type t :identity t)
    (format stream "(with ~A cookie~:P)" (length (cookie-jar-cookies cookie-jar)))))

(defun parse-cookie-date (string)
  "Parses a cookie expiry date and returns it as a Lisp universal
time.  Currently understands the following formats:

  \"Wed, 06-Feb-2008 21:01:38 GMT\"
  \"Wed, 06-Feb-08 21:01:38 GMT\"
  \"Tue Feb 13 08:00:00 2007 GMT\"
  \"Wednesday, 07-February-2027 08:55:23 GMT\"
  \"Wed, 07-02-2017 10:34:45 GMT\"

Instead of \"GMT\" time zone abbreviations like \"CEST\" and UTC
offsets like \"GMT-01:30\" are also allowed."
  ;; it seems like everybody and their sister invents their own format
  ;; for this, so (as there's no real standard for it) we'll have to
  ;; make this function more flexible once we come across something
  ;; new; as an alternative we could use net-telent-date, but it also
  ;; fails to parse some of the stuff you encounter in the wild; or we
  ;; could try to employ CL-PPCRE, but that'd add a new dependency
  ;; without making this code much cleaner
  (handler-case 
      (let* ((last-space-pos
              (or (position #\Space string :test #'char= :from-end t)
                  (cookie-date-parse-error "Can't parse cookie date ~S, no space found." string)))
             (time-zone-string (subseq string (1+ last-space-pos)))
             (time-zone (interpret-as-time-zone time-zone-string))
             second minute hour day month year)
        (dolist (part (rest (split-string (subseq string 0 last-space-pos))))
          (when (and day month)
            (cond ((every #'digit-char-p part)
                   (when year
                     (cookie-date-parse-error "Can't parse cookie date ~S, confused by ~S part."
                                              string part))
                   (setq year (parse-integer part)))
                  ((= (count #\: part :test #'char=) 2)
                   (let ((h-m-s (mapcar #'safe-parse-integer (split-string part ":"))))
                     (setq hour (first h-m-s)
                           minute (second h-m-s)
                           second (third h-m-s))))
                  (t (cookie-date-parse-error "Can't parse cookie date ~S, confused by ~S part."
                                              string part))))
          (cond ((null day)
                 (unless (setq day (safe-parse-integer part))               
                   (setq month (interpret-as-month part))))
                ((null month)
                 (setq month (interpret-as-month part)))))
        (unless (and second minute hour day month year)
          (cookie-date-parse-error "Can't parse cookie date ~S, component missing." string))
        (when (< year 100)
          (setq year (+ year 2000)))
        (encode-universal-time second minute hour day month year time-zone))
    (cookie-date-parse-error (condition)
      (cond (*ignore-unparseable-cookie-dates-p*
             (drakma-warn "~A" condition)
             nil)
            (t (error condition))))))

(defun parse-set-cookie (string)
  "Parses the `Set-Cookie' header line STRING and returns a list
of three-element lists where each one contains the name of the
cookie, the value of the cookie, and an attribute/value list for
the optional cookie parameters."
  (with-sequence-from-string (stream string)
    (loop with *current-error-message* = (format nil "While parsing cookie header ~S:" string)
          for first = t then nil
          for next = (and (skip-whitespace stream)
                          (or first (assert-char stream #\,))
                          (skip-whitespace stream)
                          (skip-more-commas stream))
          for name/value = (and next (read-name-value-pair stream
                                                           :cookie-syntax t))
          for parameters = (and name/value (read-name-value-pairs stream
                                                                  :value-required-p nil
                                                                  :cookie-syntax t))
          while name/value
          collect (list (car name/value) (cdr name/value) parameters))))

(defun get-cookies (headers uri)
  "Returns a list of COOKIE objects corresponding to the
`Set-Cookie' header as found in HEADERS \(an alist as returned by
HTTP-REQUEST).  Collects only cookies which match the domain of
the \(PURI) URI URI."
  (loop with set-cookie-header = (header-value :set-cookie headers)
        with parsed-cookies = (and set-cookie-header (parse-set-cookie set-cookie-header))
        for (name value parameters) in parsed-cookies
        for expires = (parameter-value "expires" parameters)
        for domain = (or (parameter-value "domain" parameters) (uri-host uri))
        when (and (valid-cookie-domain-p domain)
                  (cookie-domain-matches domain uri))
        collect (make-instance 'cookie
                               :name name
                               :value value
                               :path (or (parameter-value "path" parameters)
                                         (uri-path uri)
                                         "/")
                               :expires (and expires
                                             (plusp (length expires))
                                             (parse-cookie-date expires))
                               :domain domain
                               :securep (not (not (parameter-present-p "secure" parameters)))
                               :http-only-p (not (not (parameter-present-p "HttpOnly" parameters))))))

(defun update-cookies (new-cookies cookie-jar)
  "Updates the cookies in COOKIE-JAR by replacing those which are
equal to a cookie in \(the list) NEW-COOKIES with the corresponding
`new' cookie and adding those which are really new."
  (setf (cookie-jar-cookies cookie-jar)
        (let ((updated-cookies
               (loop for old-cookie in (cookie-jar-cookies cookie-jar)
                     collect (or (find old-cookie new-cookies :test #'cookie=)
                                 old-cookie))))
          (union updated-cookies
                 (set-difference new-cookies updated-cookies))))                               
  cookie-jar)

(defun delete-old-cookies (cookie-jar)
  "Removes all cookies from COOKIE-JAR which have either expired
or which don't have an expiry date."
  (setf (cookie-jar-cookies cookie-jar)
        (loop with now = (get-universal-time)
              for cookie in (cookie-jar-cookies cookie-jar)
              for expires = (cookie-expires cookie)
              unless (or (null expires) (< expires now))
              collect cookie))
  cookie-jar)
