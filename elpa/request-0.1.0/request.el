;;; request.el --- Compatible layer for URL request in Emacs

;; Copyright (C) 2012 Takafumi Arakaki
;; Copyright (C) 1985-1986, 1992, 1994-1995, 1999-2012
;;   Free Software Foundation, Inc.

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>
;; Version: 0.1.0
;; Package-Version: 0.1.0

;; This file is NOT part of GNU Emacs.

;; request.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; request.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with request.el.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Request.el is a HTTP request library with multiple backends.  It
;; supports url.el which is shipped with Emacs and curl command line
;; program.  User can use curl when s/he has it, as curl is more reliable
;; than url.el.  Library author can use request.el to avoid imposing
;; external dependencies such as curl to users while giving richer
;; experience for users who have curl.

;; Following functions are adapted from GNU Emacs source code.
;; Free Software Foundation holds the copyright of them.
;; * `request--process-live-p'
;; * `request--url-default-expander'

;;; Code:

(eval-when-compile (require 'cl))
(require 'url)
(require 'mail-utils)

(defgroup request nil
  "Compatible layer for URL request in Emacs."
  :group 'comm
  :prefix "request-")

(defconst request-version "0.1.0")


;;; Customize variables

(defcustom request-storage-directory
  (concat (file-name-as-directory user-emacs-directory) "request")
  "Directory to store data related to request.el."
  :group 'request)

(defcustom request-curl "curl"
  "Executable for curl command."
  :group 'request)

(defcustom request-backend (if (executable-find request-curl)
                               'curl
                             'url-retrieve)
  "Backend to be used for HTTP request.
Automatically set to `curl' if curl command is found."
  :group 'request)

(defcustom request-timeout nil
  "Default request timeout in second.
`nil' means no timeout."
  :group 'request)

(defcustom request-log-level -1
  "Logging level for request.
One of `error'/`warn'/`info'/`verbose'/`debug'.
-1 means no logging."
  :group 'request)

(defcustom request-message-level 'warn
  "Logging level for request.
See `request-log-level'."
  :group 'request)


;;; Utilities

(defun request--safe-apply (function &rest arguments)
  (condition-case err
      (apply #'apply function arguments)
    ((debug error))))

(defun request--safe-call (function &rest arguments)
  (request--safe-apply function arguments))

;; (defun request--url-no-cache (url)
;;   "Imitate `cache=false' of `jQuery.ajax'.
;; See: http://api.jquery.com/jQuery.ajax/"
;;   ;; FIXME: parse URL before adding ?_=TIME.
;;   (concat url (format-time-string "?_=%s")))

(defmacro request--document-function (function docstring)
  "Document FUNCTION with DOCSTRING.  Use this for defstruct accessor etc."
  (declare (indent defun)
           (doc-string 2))
  `(put ',function 'function-documentation ,docstring))

(defun request--process-live-p (process)
  "Copied from `process-live-p' for backward compatibility (Emacs < 24).
Adapted from lisp/subr.el.
FSF holds the copyright of this function:
  Copyright (C) 1985-1986, 1992, 1994-1995, 1999-2012
    Free Software Foundation, Inc."
  (memq (process-status process) '(run open listen connect stop)))


;;; Logging

(defconst request--log-level-def
  '(;; debugging
    (blather . 60) (trace . 50) (debug . 40)
    ;; information
    (verbose . 30) (info . 20)
    ;; errors
    (warn . 10) (error . 0))
  "Named logging levels.")

(defun request--log-level-as-int (level)
  (if (integerp level)
      level
    (or (cdr (assq level request--log-level-def))
        0)))

(defvar request-log-buffer-name " *request-log*")

(defun request--log-buffer ()
  (get-buffer-create request-log-buffer-name))

(defmacro request-log (level fmt &rest args)
  (declare (indent 1))
  `(let ((level (request--log-level-as-int ,level))
         (log-level (request--log-level-as-int request-log-level))
         (msg-level (request--log-level-as-int request-message-level)))
     (when (<= level (max log-level msg-level))
       (let ((msg (format "[%s] %s" ,level
                          (condition-case err
                              (format ,fmt ,@args)
                            (error (format "
!!! Logging error while executing:
%S
!!! Error:
%S"
                                           ',args err))))))
         (when (<= level log-level)
           (with-current-buffer (request--log-buffer)
             (setq buffer-read-only t)
             (let ((inhibit-read-only t))
               (goto-char (point-max))
               (insert msg "\n"))))
         (when (<= level msg-level)
           (message "REQUEST %s" msg))))))


;;; HTTP specific utilities

(defconst request--url-unreserved-chars
  '(?a ?b ?c ?d ?e ?f ?g ?h ?i ?j ?k ?l ?m ?n ?o ?p ?q ?r ?s ?t ?u ?v ?w ?x ?y ?z
    ?A ?B ?C ?D ?E ?F ?G ?H ?I ?J ?K ?L ?M ?N ?O ?P ?Q ?R ?S ?T ?U ?V ?W ?X ?Y ?Z
    ?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9
    ?- ?_ ?. ?~)
  "`url-unreserved-chars' copied from Emacs 24.3 release candidate.
This is used for making `request--urlencode-alist' RFC 3986 compliant
for older Emacs versions.")

(defun request--urlencode-alist (alist)
  ;; FIXME: make monkey patching `url-unreserved-chars' optional
  (let ((url-unreserved-chars request--url-unreserved-chars))
    (loop for sep = "" then "&"
          for (k . v) in alist
          concat sep
          concat (url-hexify-string (format "%s" k))
          concat "="
          concat (url-hexify-string v))))


;;; Header parser

(defun request--parse-response-at-point ()
  "Parse the first header line such as \"HTTP/1.1 200 OK\"."
  (re-search-forward "\\=[ \t\n]*HTTP/\\([0-9\\.]+\\) +\\([0-9]+\\)")
  (list :version (match-string 1)
        :code (string-to-number (match-string 2))))

(defun request--goto-next-body ()
  (re-search-forward "^\r\n"))


;;; Response object

(defstruct request-response
  "A structure holding all relevant information of a request."
  status-code redirects data error-thrown symbol-status url
  done-p settings
  ;; internal variables
  -buffer -timer -backend -tempfiles)

(defmacro request--document-response (function docstring)
  (declare (indent defun)
           (doc-string 2))
  `(request--document-function ,function ,(concat docstring "

.. This is an accessor for `request-response' object.

\(fn RESPONSE)")))

(request--document-response request-response-status-code
  "Integer HTTP response code (e.g., 200).")

(request--document-response request-response-redirects
  "Redirection history (a list of URLs).
The first element is the oldest redirection.")

(request--document-response request-response-data
  "Response parsed by the given parser.")

(request--document-response request-response-error-thrown
  "Error thrown during request.
It takes the form of ``(ERROR-SYMBOL . DATA)``, which can be
re-raised (`signal'ed) by ``(signal ERROR-SYMBOL DATA)``.")

(request--document-response request-response-symbol-status
  "A symbol representing the status of request (not HTTP response code).
One of success/error/timeout/abort/parse-error.")

(request--document-response request-response-url
  "Final URL location of response.")

(request--document-response request-response-done-p
  "Return t when the request is finished or aborted.")

(request--document-response request-response-settings
  "Keyword arguments passed to `request' function.
Some arguments such as HEADERS is changed to the one actually
passed to the backend.  Also, it has additional keywords such
as URL which is the requested URL.")


;;; Backend dispatcher

(defconst request--backend-alist
  '((url-retrieve
     . ((request             . request--url-retrieve)
        (request-sync        . request--url-retrieve-sync)
        (terminate-process   . delete-process)
        (get-cookies         . request--url-retrieve-get-cookies)))
    (curl
     . ((request             . request--curl)
        (request-sync        . request--curl-sync)
        (terminate-process   . interrupt-process)
        (get-cookies         . request--curl-get-cookies))))
  "Map backend and method name to actual method (symbol).

It's alist of alist, of the following form::

    ((BACKEND . ((METHOD . FUNCTION) ...)) ...)

It would be nicer if I can use EIEIO.  But as CEDET is included
in Emacs by 23.2, using EIEIO means abandon older Emacs versions.
It is probably necessary if I need to support more backends.  But
let's stick to manual dispatch for now.")
;; See: (view-emacs-news "23.2")

(defun request--choose-backend (method)
  "Return `fucall'able object for METHOD of current `request-backend'."
  (assoc-default
   method
   (or (assoc-default request-backend request--backend-alist)
       (error "%S is not valid `request-backend'." request-backend))))


;;; Cookie

(defun request-cookie-string (host &optional localpart secure)
  "Return cookie string (like `document.cookie').

Example::

 (request-cookie-string \"127.0.0.1\" \"/\")  ; => \"key=value; key2=value2\"
"
  (mapconcat (lambda (nv) (concat (car nv) "=" (cdr nv)))
             (request-cookie-alist host localpart secure)
             "; "))

(defun request-cookie-alist (host &optional localpart secure)
  "Return cookies as an alist.

Example::

 (request-cookie-alist \"127.0.0.1\" \"/\")  ; => ((\"key\" . \"value\") ...)
"
  (funcall (request--choose-backend 'get-cookies) host localpart secure))


;;; Main

(defun* request-default-error-callback (url &key symbol-status
                                            &allow-other-keys)
  (request-log 'error
    "Error (%s) while connecting to %s." symbol-status url))

(defun* request (url &rest settings
                     &key
                     (type "GET")
                     (params nil)
                     (data nil)
                     (files nil)
                     (parser nil)
                     (headers nil)
                     (success nil)
                     (error nil)
                     (complete nil)
                     (timeout request-timeout)
                     (status-code nil)
                     (sync nil)
                     (response (make-request-response)))
  "Send request to URL.

Request.el has a single entry point.  It is `request'.

==================== ========================================================
Keyword argument      Explanation
==================== ========================================================
TYPE       (string)   type of request to make: POST/GET/PUT/DELETE
PARAMS      (alist)   set \"?key=val\" part in URL
DATA (string/alist)   data to be sent to the server
FILES       (alist)   files to be sent to the server (see below)
PARSER     (symbol)   a function that reads current buffer and return data
HEADERS     (alist)   additional headers to send with the request
SUCCESS  (function)   called on success
ERROR    (function)   called on error
COMPLETE (function)   called on both success and error
TIMEOUT    (number)   timeout in second
STATUS-CODE (alist)   map status code (int) to callback
SYNC         (bool)   If `t', wait until request is done.  Default is `nil'.
==================== ========================================================


* Callback functions

Callback functions STATUS, ERROR, COMPLETE and `cdr's in element of
the alist STATUS-CODE take same keyword arguments listed below.  For
forward compatibility, these functions must ignore unused keyword
arguments (i.e., it's better to use `&allow-other-keys').::

    (CALLBACK                      ; SUCCESS/ERROR/COMPLETE/STATUS-CODE
     :data          data           ; whatever PARSER function returns, or nil
     :error-thrown  error-thrown   ; (ERROR-SYMBOL . DATA), or nil
     :symbol-status symbol-status  ; success/error/timeout/abort/parse-error
     :response      response       ; request-response object
     ...)

Arguments data, error-thrown, symbol-status can be accessed by
`request-response-data', `request-response-error-thrown',
`request-response-symbol-status' accessors, i.e.::

    (request-response-data RESPONSE)  ; same as data

Response object holds other information which can be accessed by
the following accessors:
`request-response-status-code',
`request-response-redirects',
`request-response-url' and
`request-response-settings'

* STATUS-CODE callback

STATUS-CODE is an alist of the following format::

    ((N-1 . CALLBACK-1)
     (N-2 . CALLBACK-2)
     ...)

Here, N-1, N-2,... are integer status codes such as 200.


* FILES

FILES is an alist of the following format::

    ((NAME-1 . FILE-1)
     (NAME-2 . FILE-2)
     ...)

where FILE-N is a list of the form::

    (FILENAME &key PATH BUFFER STRING MIME-TYPE)

FILE-N can also be a string (path to the file) or a buffer object.
In that case, FILENAME is set to the file name or buffer name.

Example FILES argument::

    `((\"passwd\"   . \"/etc/passwd\")                ; filename = passwd
      (\"scratch\"  . ,(get-buffer \"*scratch*\"))    ; filename = *scratch*
      (\"passwd2\"  . (\"password.txt\" :file \"/etc/passwd\"))
      (\"scratch2\" . (\"scratch.txt\"  :buffer ,(get-buffer \"*scratch*\")))
      (\"data\"     . (\"data.csv\"     :data \"1,2,3\\n4,5,6\\n\")))

.. note:: FILES is implemented only for curl backend for now.
   As furl.el_ supports multipart POST, it should be possible to
   support FILES in pure elisp by making furl.el_ another backend.
   Contributions are welcome.

   .. _furl.el: http://code.google.com/p/furl-el/


* PARSER function

PARSER function takes no argument and it is executed in the
buffer with HTTP response.  The current position in the
HTTP response buffer is at the beginning of the response
body.  So, for example, you can pass `json-read' to parse
JSON object in the buffer.  To fetch whole buffer as a string,
pass `buffer-string'.  If you want just the response part
without header, pass::

    (lambda () (buffer-substring (point) (point-max)))

This is analogous to the `dataType' argument of jQuery.ajax_.
Only this function can access to the process buffer, which
is killed immediately after the execution of this function.

* SYNC

Synchronous request is functional, but *please* don't use it
other than testing or debugging.  Emacs users have better things
to do rather than waiting for HTTP request.  If you want a better
way to write callback chains, use `request-deferred'.

If you can't avoid using it (e.g., you are inside of some hook
which must return some value), make sure to set TIMEOUT to
relatively small value.

Due to limitation of `url-retrieve-synchronously', response slots
`request-response-error-thrown', `request-response-redirects' and
`request-response-url' are unknown (always `nil') when using
synchronous request with `url-retrieve' backend.

* Note

API of `request' is somewhat mixture of jQuery.ajax_ (Javascript)
and requests.request_ (Python).

.. _jQuery.ajax: http://api.jquery.com/jQuery.ajax/
.. _requests.request: http://docs.python-requests.org
"
  (request-log 'debug "REQUEST")
  ;; FIXME: support CACHE argument (if possible)
  ;; (unless cache
  ;;   (setq url (request--url-no-cache url)))
  (unless error
    (setq error (apply-partially #'request-default-error-callback url))
    (setq settings (plist-put settings :error error)))
  (unless (or (stringp data)
              (null data)
              (assoc-string headers "Content-Type" t))
    (setq data (request--urlencode-alist data))
    (setq settings (plist-put settings :data data)))
  (when params
    (setq url (concat url (if (string-match-p "\\?" url) "&" "?")
                      (request--urlencode-alist params))))
  (setq settings (plist-put settings :url url))
  (setq settings (plist-put settings :response response))
  (setf (request-response-settings response) settings)
  (setf (request-response-url      response) url)
  (setf (request-response--backend response) request-backend)
  ;; Call `request--url-retrieve'(`-sync') or `request--curl'(`-sync').
  (apply (if sync
             (request--choose-backend 'request-sync)
           (request--choose-backend 'request))
         url settings)
  (when timeout
    (request-log 'debug "Start timer: timeout=%s sec" timeout)
    (setf (request-response--timer response)
          (run-at-time timeout nil
                       #'request-response--timeout-callback response)))
  response)

(defun request--parse-data (buffer parser error-thrown backend)
  "Run PARSER in current buffer if ERROR-THROWN is nil,
then kill the current buffer."
  (request-log 'debug "-PARSE-DATA")
  (request-log 'debug "parser = %s" parser)
  (request-log 'debug "error-thrown = %S" error-thrown)
  (request-log 'debug "backend = %S" backend)
  (let (noerror)
    (unwind-protect
        (prog1
            (when (and (buffer-live-p buffer) parser (not error-thrown))
              (with-current-buffer buffer
                (goto-char (point-min))
                ;; Should be no \r.
                ;; See `url-http-clean-headers' and `request--curl-preprocess'.
                (if (eq backend 'url-retrieve)
                    ;; FIXME: make this workaround optional.
                    ;; But it looks like sometimes `url-http-clean-headers'
                    ;; fails to cleanup.  So, let's be bit permissive here...
                    (re-search-forward "^\r?$")
                  (re-search-forward "^$"))
                ;; `forward-char' will fail when there is no body.
                (ignore-errors (forward-char))
                (funcall parser)))
          (setq noerror t))
      (unless noerror
        (request-log 'error "REQUEST--PARSE-DATA: error from parser %S"
                     parser))
      (kill-buffer buffer))))

(defun* request--callback (buffer &key parser success error complete
                                  timeout status-code response
                                  &allow-other-keys)
  (request-log 'debug "REQUEST--CALLBACK")
  (request-log 'debug "(buffer-string) =\n%s"
               (when (buffer-live-p buffer)
                 (with-current-buffer buffer (buffer-string))))

  (request-response--cancel-timer response)
  (symbol-macrolet
      ((error-thrown (request-response-error-thrown response))
       (symbol-status (request-response-symbol-status response))
       (data (request-response-data response))
       (done-p (request-response-done-p response)))

    ;; Parse response body
    (setq data (condition-case err
                   (request--parse-data buffer parser error-thrown
                                        (request-response--backend response))
                 (error
                  (setq symbol-status 'parse-error)
                  (setq error-thrown err))))
    (request-log 'debug "data = %s" data)

    ;; Determine `symbol-status'
    (unless symbol-status
      (setq symbol-status (if error-thrown 'error 'success)))
    (request-log 'debug "symbol-status = %s" symbol-status)

    ;; Call callbacks
    (let ((args (list :data data
                      :symbol-status symbol-status
                      :error-thrown error-thrown
                      :response response)))
      (let* ((success-p (eq symbol-status 'success))
             (cb (if success-p success error))
             (name (if success-p "success" "error")))
        (when cb
          (request-log 'debug "Executing %s callback." name)
          (request--safe-apply cb args)))

      (let ((cb (cdr (assq (request-response-status-code response)
                           status-code))))
        (when cb
          (request-log 'debug "Executing status-code callback.")
          (request--safe-apply cb args)))

      (when complete
        (request-log 'debug "Executing complete callback.")
        (request--safe-apply complete args)))

    (setq done-p t)

    ;; Remove temporary files
    ;; FIXME: Make tempfile cleanup more reliable.  It is possible
    ;;        callback is never called.
    (request--safe-delete-files (request-response--tempfiles response))))

(defun* request-response--timeout-callback (response)
  (request-log 'debug "-TIMEOUT-CALLBACK")
  (setf (request-response-symbol-status response) 'timeout)
  (setf (request-response-error-thrown response)  '(error . ("Timeout")))
  (let* ((buffer (request-response--buffer response))
         (proc (and (buffer-live-p buffer) (get-buffer-process buffer))))
    (when proc
      ;; This will call `request--callback':
      (funcall (request--choose-backend 'terminate-process) proc))

    (symbol-macrolet ((done-p (request-response-done-p response)))
      (unless done-p
        ;; This code should never be executed.  However, it occurs
        ;; sometimes with `url-retrieve' backend.
        (request-log 'error "Callback is not called when stopping process! \
Explicitly calling from timer.")
        (apply #'request--callback
               (with-temp-buffer (current-buffer)) ; #<killed buffer>
               (request-response-settings response))
        (setq done-p t)))))

(defun request-response--cancel-timer (response)
  (request-log 'debug "REQUEST-RESPONSE--CANCEL-TIMER")
  (symbol-macrolet ((timer (request-response--timer response)))
    (when timer
      (cancel-timer timer)
      (setq timer nil))))


(defun request-abort (response)
  "Abort request for RESPONSE (the object returned by `request').
Note that this function invoke ERROR and COMPLETE callbacks.
Callbacks may not be called immediately but called later when
associated process is exited."
  (symbol-macrolet ((buffer (request-response--buffer response))
                    (symbol-status (request-response-symbol-status response))
                    (done-p (request-response-done-p response)))
    (let ((process (get-buffer-process buffer)))
      (when (and (request--process-live-p process) (not symbol-status))
        (setq symbol-status 'abort)
        (setq done-p t)
        (funcall (request--choose-backend 'terminate-process) process)))))


;;; Backend: `url-retrieve'

(defun* request--url-retrieve-preprocess-settings
    (&rest settings &key type data files headers &allow-other-keys)
  (when files
    (error "`url-retrieve' backend does not support FILES."))
  (when (and (equal type "POST")
             data
             (not (assoc-string headers "Content-Type" t)))
    (push '("Content-Type" . "application/x-www-form-urlencoded") headers)
    (setq settings (plist-put settings :headers headers)))
  settings)

(defun* request--url-retrieve (url &rest settings
                                   &key type data timeout response
                                   &allow-other-keys
                                   &aux headers)
  (setq settings (apply #'request--url-retrieve-preprocess-settings settings))
  (setq headers (plist-get settings :headers))
  (let* ((url-request-extra-headers headers)
         (url-request-method type)
         (url-request-data data)
         (buffer (url-retrieve url #'request--url-retrieve-callback
                               (nconc (list :response response) settings)))
         (proc (get-buffer-process buffer)))
    (setf (request-response--buffer response) buffer)
    (process-put proc :request-response response)
    (request-log 'debug "Start querying: %s" url)
    (set-process-query-on-exit-flag proc nil)))

(defun* request--url-retrieve-callback (status &rest settings
                                               &key response
                                               &allow-other-keys)
  (declare (special url-http-method
                    url-http-response-status))
  (request-log 'debug "-URL-RETRIEVE-CALLBACK")
  (request-log 'debug "status = %S" status)
  (request-log 'debug "url-http-method = %s" url-http-method)
  (request-log 'debug "url-http-response-status = %s" url-http-response-status)

  (setf (request-response-status-code response) url-http-response-status)
  (let ((redirect (plist-get status :redirect)))
    (when redirect
      (setf (request-response-url response) redirect)
      (setf (request-response-redirects response)
            (loop with l = nil
                  for (k v) on redirect by 'cddr
                  when (eq k :redirect)
                  do (push v l)
                  finally return l))))

  (symbol-macrolet ((error-thrown (request-response-error-thrown response))
                    (status-error (plist-get status :error)))
    (when (and error-thrown status-error)
      (request-log 'warn
        "Error %S thrown already but got another error %S from \
`url-retrieve'.  Ignoring it..." error-thrown status-error))
    (unless error-thrown
      (setq error-thrown status-error)))

  (apply #'request--callback (current-buffer) settings))

(defun* request--url-retrieve-sync (url &rest settings
                                        &key type data timeout response
                                        &allow-other-keys
                                        &aux headers)
  (setq settings (apply #'request--url-retrieve-preprocess-settings settings))
  (setq headers (plist-get settings :headers))
  (let* ((url-request-extra-headers headers)
         (url-request-method type)
         (url-request-data data)
         (buffer (if timeout
                     (with-timeout
                         (timeout
                          (setf (request-response-symbol-status response)
                                'timeout)
                          (setf (request-response-done-p response) t)
                          nil)
                       (url-retrieve-synchronously url))
                   (url-retrieve-synchronously url))))
    (setf (request-response--buffer response) buffer)
    ;; It seems there is no way to get redirects and URL here...
    (when buffer
      ;; Fetch HTTP response code
      (with-current-buffer buffer
        (goto-char (point-min))
        (destructuring-bind (&key version code)
            (request--parse-response-at-point)
          (setf (request-response-status-code response) code)))
      ;; Parse response body, etc.
      (apply #'request--callback buffer settings)))
  response)

(defun request--url-retrieve-get-cookies (host localpart secure)
  (mapcar
   (lambda (c) (cons (url-cookie-name c) (url-cookie-value c)))
   (url-cookie-retrieve host localpart secure)))


;;; Backend: curl

(defvar request--curl-cookie-jar nil
  "Override what the function `request--curl-cookie-jar' returns.
Currently it is used only for testing.")

(defun request--curl-cookie-jar ()
  "Cookie storage for curl backend."
  (or request--curl-cookie-jar
      (expand-file-name "curl-cookie-jar" request-storage-directory)))

(defconst request--curl-write-out-template
  "\\n(:num-redirects %{num_redirects} :url-effective \"%{url_effective}\")")
;; FIXME: should % be escaped for Windows?

(defun request--curl-mkdir-for-cookie-jar ()
  (ignore-errors
    (make-directory (file-name-directory (request--curl-cookie-jar)) t)))

(defun* request--curl-command
    (url &key type data headers timeout files*
         &allow-other-keys
         &aux
         (cookie-jar (convert-standard-filename
                      (expand-file-name (request--curl-cookie-jar)))))
  (append
   (list request-curl "--silent" "--include"
         "--location"
         ;; FIMXE: this way of using cookie might be problem when
         ;;        running multiple requests.
         "--cookie" cookie-jar "--cookie-jar" cookie-jar
         "--write-out" request--curl-write-out-template)
   (loop for (name filename path mime-type) in files*
         collect "--form"
         collect (format "%s=@%s;filename=%s%s" name path filename
                         (if mime-type
                             (format ";type=%s" mime-type)
                           "")))
   (when data (list "--data-binary" "@-"))
   (when type (list "--request" type))
   (loop for (k . v) in headers
         collect "--header"
         collect (format "%s: %s" k v))
   (list url)))

(defun request--curl-normalize-files-1 (files get-temp-file)
  (loop for (name . item) in files
        collect
        (destructuring-bind (filename &key file buffer data mime-type)
            (cond
             ((stringp item) (list (file-name-nondirectory item) :file item))
             ((bufferp item) (list (buffer-name item) :buffer item))
             (t item))
          (unless (= (loop for v in (list file buffer data) if v sum 1) 1)
            (error "Only one of :file/:buffer/:data must be given.  Got: %S"
                   (cons name item)))
          (cond
           (file
            (list name filename file mime-type))
           (buffer
            (let ((tf (funcall get-temp-file)))
              (with-current-buffer buffer
                (write-region (point-min) (point-max) tf nil 'silent))
              (list name filename tf mime-type)))
           (data
            (let ((tf (funcall get-temp-file)))
              (with-temp-buffer
                (erase-buffer)
                (insert data)
                (write-region (point-min) (point-max) tf nil 'silent))
              (list name filename tf mime-type)))))))

(defun request--curl-normalize-files (files)
  "Change FILES into a list of (NAME FILENAME PATH MIME-TYPE).
This is to make `request--curl-command' cleaner by converting
FILES to a homogeneous list.  It returns a list (FILES* TEMPFILES)
where FILES* is a converted FILES and TEMPFILES is a list of
temporary file paths."
  (let (tempfiles noerror)
    (unwind-protect
        (let* ((get-temp-file (lambda ()
                                (let ((tf (make-temp-file "emacs-request-")))
                                  (push tf tempfiles)
                                  tf)))
               (files* (request--curl-normalize-files-1 files get-temp-file)))
          (setq noerror t)
          (list files* tempfiles))
      (unless noerror
        ;; Remove temporary files only when an error occurs
        (request--safe-delete-files tempfiles)))))

(defun request--safe-delete-files (files)
  "Remove FILES but do not raise error when failed to do so."
  (mapc (lambda (f) (condition-case err
                        (delete-file f)
                      (error (request-log 'error
                               "Failed delete file %s. Got: %S" f err))))
        files))

(defun* request--curl (url &rest settings
                           &key type data files headers timeout response
                           &allow-other-keys)
  "cURL-based request backend.

Redirection handling strategy
-----------------------------

curl follows redirection when --location is given.  However,
all headers are printed when it is used with --include option.
Number of redirects is printed out sexp-based message using
--write-out option (see `request--curl-write-out-template').
This number is used for removing extra headers and parse
location header from the last redirection header.

Sexp at the end of buffer and extra headers for redirects are
removed from the buffer before it is shown to the parser function.
"
  (request--curl-mkdir-for-cookie-jar)
  (let* (;; Use pipe instead of pty.  Otherwise, curl process hangs.
         (process-connection-type nil)
         (buffer (generate-new-buffer " *request curl*"))
         (command (destructuring-bind
                      (files* tempfiles)
                      (request--curl-normalize-files files)
                    (setf (request-response--tempfiles response) tempfiles)
                    (apply #'request--curl-command url :files* files*
                           settings)))
         (proc (apply #'start-process "request curl" buffer command)))
    (request-log 'debug "Run: %s" (mapconcat 'identity command " "))
    (setf (request-response--buffer response) buffer)
    (process-put proc :request-response response)
    (set-process-query-on-exit-flag proc nil)
    (set-process-sentinel proc #'request--curl-callback)
    (when data
      (process-send-string proc data)
      (process-send-eof proc))))

(defun request--curl-read-and-delete-tail-info ()
  "Read a sexp at the end of buffer and remove it and preceding character.
This function moves the point at the end of buffer by side effect.
See also `request--curl-write-out-template'."
  (let (forward-sexp-function)
    (goto-char (point-max))
    (forward-sexp -1)
    (let ((beg (1- (point))))
      (prog1
          (read (current-buffer))
        (delete-region beg (point-max))))))

(defconst request--cookie-reserved-re
  (mapconcat
   (lambda (x) (concat "\\(^" x "\\'\\)"))
   '("comment" "commenturl" "discard" "domain" "max-age" "path" "port"
     "secure" "version" "expires")
   "\\|")
  "Uninterested keys in cookie.
See \"set-cookie-av\" in http://www.ietf.org/rfc/rfc2965.txt")

(defun request--consume-100-continue ()
  "Remove \"HTTP/* 100 Continue\" header at the point."
  (destructuring-bind (&key code &allow-other-keys)
      (save-excursion (ignore-errors (request--parse-response-at-point)))
    (when (equal code 100)
      (delete-region (point) (progn (request--goto-next-body) (point)))
      ;; FIXME: Does this make sense?  Is it possible to have multiple 100?
      (request--consume-100-continue))))

(defun request--curl-preprocess ()
  "Pre-process current buffer before showing it to user."
  (let (redirects)
    (destructuring-bind (&key num-redirects url-effective)
        (request--curl-read-and-delete-tail-info)
      (goto-char (point-min))
      (request--consume-100-continue)
      (when (> num-redirects 0)
        (loop with case-fold-search = t
              repeat num-redirects
              do (request--consume-100-continue)
              for beg = (point)
              do (request--goto-next-body)
              for end = (point)
              ;; FIXME: use `mail-fetch-field'
              do (progn
                   (re-search-backward "^location: \\([^\r\n]+\\)\r\n" beg)
                   (push (match-string 1) redirects)
                   (goto-char end))
              ;; Remove headers for redirection.
              finally do (delete-region (point-min) end)))

      ;; Remove \r from header to use `mail-fetch-field'.
      ;; See: `url-http-clean-headers'
      (goto-char (point-min))
      (request--goto-next-body)
      (while (re-search-backward "\r$" (point-min) t)
        (replace-match ""))

      (goto-char (point-min))
      (nconc (list :num-redirects num-redirects :url-effective url-effective
                   :redirects redirects)
             (request--parse-response-at-point)))))

(defun request--curl-absolutify-redirects (start-url redirects)
  "Convert relative paths in REDIRECTS to absolute URLs.
START-URL is the URL requested."
  (loop for prev-url = start-url then url
        for url in redirects
        unless (string-match url-nonrelative-link url)
        do (setq url (url-expand-file-name url prev-url))
        collect url))

(defun request--curl-callback (proc event)
  (let* ((buffer (process-buffer proc))
         (response (process-get proc :request-response))
         (symbol-status (request-response-symbol-status response))
         (settings (request-response-settings response)))
    (request-log 'debug "REQUEST--CURL-CALLBACK event = %s" event)
    (request-log 'debug "REQUEST--CURL-CALLBACK proc = %S" proc)
    (request-log 'debug "REQUEST--CURL-CALLBACK buffer = %S" buffer)
    (request-log 'debug "REQUEST--CURL-CALLBACK symbol-status = %S"
                 symbol-status)
    (cond
     ((and (memq (process-status proc) '(exit signal))
           (/= (process-exit-status proc) 0))
      (setf (request-response-error-thrown response) (cons 'error event))
      (apply #'request--callback buffer settings))
     ((equal event "finished\n")
      (destructuring-bind (&key version code num-redirects redirects error
                                url-effective)
          (condition-case err
              (with-current-buffer buffer
                (request--curl-preprocess))
            ((debug error)
             (list :error err)))
        (setf (request-response-status-code  response) code)
        (setf (request-response-url          response) url-effective)
        (setf (request-response-redirects    response)
              (request--curl-absolutify-redirects (plist-get settings :url)
                                                  (nreverse redirects)))
        (setf (request-response-error-thrown response)
              (or error (when (>= code 400) `(error . (http ,code)))))
        (apply #'request--callback buffer settings))))))

(defun* request--curl-sync (url &rest settings &key response &allow-other-keys)
  ;; To make timeout work, use polling approach rather than using
  ;; `call-process'.
  (lexical-let (finished)
    (prog1 (apply #'request--curl url
                  :complete (lambda (&rest _) (setq finished t))
                  settings)
      (let ((proc (get-buffer-process (request-response--buffer response))))
        (while (and (not finished) (request--process-live-p proc))
          (accept-process-output proc))))))

(defun request--curl-get-cookies (host localpart secure)
  (request--netscape-get-cookies (request--curl-cookie-jar)
                                 host localpart secure))


;;; Netscape cookie.txt parser

(defun request--netscape-cookie-parse ()
  "Parse Netscape/Mozilla cookie format."
  (goto-char (point-min))
  (let ((tsv-re (concat "^\\="
                        (loop repeat 6 concat "\\([^\t\n]+\\)\t")
                        "\\(.*\\)"))
        cookies)
    (while
        (and
         (cond
          ((re-search-forward "^\\=#" nil t))
          ((re-search-forward "^\\=$" nil t))
          ((re-search-forward tsv-re)
           (push (loop for i from 1 to 7 collect (match-string i))
                 cookies)
           t))
         (= (forward-line 1) 0)
         (not (= (point) (point-max)))))
    (setq cookies (nreverse cookies))
    (loop for (domain flag path secure expiration name value) in cookies
          collect (list domain
                        (equal flag "TRUE")
                        path
                        (equal secure "TRUE")
                        (string-to-number expiration)
                        name
                        value))))

(defun request--netscape-filter-cookies (cookies host localpart secure)
  (loop for (domain flag path secure-1 expiration name value) in cookies
        when (and (equal domain host)
                  (equal path localpart)
                  (or secure (not secure-1)))
        collect (cons name value)))

(defun request--netscape-get-cookies (filename host localpart secure)
  (when (file-readable-p filename)
    (with-temp-buffer
      (erase-buffer)
      (insert-file-contents filename)
      (request--netscape-filter-cookies (request--netscape-cookie-parse)
                                        host localpart secure))))


;;; Monkey patches for url.el

(defun request--url-default-expander (urlobj defobj)
  "Adapted from lisp/url/url-expand.el.
FSF holds the copyright of this function:
  Copyright (C) 1999, 2004-2012  Free Software Foundation, Inc."
  ;; The default expansion routine - urlobj is modified by side effect!
  (if (url-type urlobj)
      ;; Well, they told us the scheme, let's just go with it.
      nil
    (setf (url-type urlobj) (or (url-type urlobj) (url-type defobj)))
    (setf (url-port urlobj) (or (url-portspec urlobj)
                                (and (string= (url-type urlobj)
                                              (url-type defobj))
				     (url-port defobj))))
    (if (not (string= "file" (url-type urlobj)))
	(setf (url-host urlobj) (or (url-host urlobj) (url-host defobj))))
    (if (string= "ftp"  (url-type urlobj))
	(setf (url-user urlobj) (or (url-user urlobj) (url-user defobj))))
    (if (string= (url-filename urlobj) "")
	(setf (url-filename urlobj) "/"))
    ;; If the object we're expanding from is full, then we are now
    ;; full.
    (unless (url-fullness urlobj)
      (setf (url-fullness urlobj) (url-fullness defobj)))
    (if (string-match "^/" (url-filename urlobj))
	nil
      (let ((query nil)
	    (file nil)
	    (sepchar nil))
	(if (string-match "[?#]" (url-filename urlobj))
	    (setq query (substring (url-filename urlobj) (match-end 0))
		  file (substring (url-filename urlobj) 0 (match-beginning 0))
		  sepchar (substring (url-filename urlobj) (match-beginning 0) (match-end 0)))
	  (setq file (url-filename urlobj)))
	;; We use concat rather than expand-file-name to combine
	;; directory and file name, since urls do not follow the same
	;; rules as local files on all platforms.
	(setq file (url-expander-remove-relative-links
		    (concat (url-file-directory (url-filename defobj)) file)))
	(setf (url-filename urlobj)
              (if query (concat file sepchar query) file))))))

(defadvice url-default-expander
  (around request-monkey-patch-url-default-expander (urlobj defobj))
  "Monkey patch `url-default-expander' to fix bug #12374.
Without this patch, port number is not treated when using
`url-expand-file-name'.
See: http://thread.gmane.org/gmane.emacs.devel/155698"
  (setq ad-return-value (request--url-default-expander urlobj defobj)))

(unless (equal (url-expand-file-name "/path" "http://127.0.0.1:8000")
               "http://127.0.0.1:8000/path")
  (ad-enable-advice 'url-default-expander
                    'around
                    'request-monkey-patch-url-default-expander)
  (ad-activate 'url-default-expander))


(eval-when-compile (require 'url-http)
                   (defvar url-http-no-retry)
                   (defvar url-http-extra-headers)
                   (defvar url-http-data)
                   (defvar url-callback-function)
                   (defvar url-callback-arguments))
(declare-function url-http-idle-sentinel "url-http")
(declare-function url-http-activate-callback "url-http")
(declare-function url-http "url-http")
(declare-function url-http-parse-headers "url-http")

(defun request--url-http-end-of-document-sentinel (proc why)
  "Adapted from lisp/url/url-http.el.
FSF holds the copyright of this function:
  Copyright (C) 1999, 2001, 2004-2012  Free Software Foundation, Inc."
  (url-http-debug "url-http-end-of-document-sentinel in buffer (%s)"
		  (process-buffer proc))
  (url-http-idle-sentinel proc why)
  (when (buffer-name (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (goto-char (point-min))
      (cond ((not (looking-at "HTTP/"))
	     (if url-http-no-retry
		 ;; HTTP/0.9 just gets passed back no matter what
		 (url-http-activate-callback)
	       ;; Call `url-http' again if our connection expired.
	       (erase-buffer)
               (let ((url-request-method url-http-method)
                     (url-request-extra-headers url-http-extra-headers)
                     (url-request-data url-http-data))
                 (url-http url-current-object url-callback-function
                           url-callback-arguments (current-buffer)))))
	    ((url-http-parse-headers)
	     (url-http-activate-callback))))))

(defadvice url-http-end-of-document-sentinel
  (around request-monkey-patch-url-http-end-of-document-sentinel (proc why))
  "Monkey patch `url-http-end-of-document-sentinel' to fix bug #11469.
Without this patch, PUT method fails every two times.
See: http://thread.gmane.org/gmane.emacs.devel/155697"
  (setq ad-return-value (request--url-http-end-of-document-sentinel proc why)))

(when (and (version< "24" emacs-version)
           (version< emacs-version "100"))
  ;; FIXME: change the version number after my patch is applied.
  (ad-enable-advice 'url-http-end-of-document-sentinel
                    'around
                    'request-monkey-patch-url-http-end-of-document-sentinel)
  (ad-activate 'url-http-end-of-document-sentinel))


(provide 'request)

;;; request.el ends here