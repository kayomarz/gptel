;;; -*- lexical-binding: t; -*-

;; This function and its invocations to be deleted in final code.
(defmacro dev-msg (str &rest args)
  `(message (concat "(DEV): " ,str) ,@args))

;;;; Query LLM APIs to count input tokens for the current gptel context.

;;; State machine for driving requests
(defvar gptel-get-token-count--transitions
  `((INIT . ((t                       . WAIT)))
    (WAIT . ((t                       . DONE))))
  "Alist specifying state transition table for token count requests.

For more, see documentation of `gptel-request--transitions' which
is gptel's default state transition table for requests.")

;;; State machine additions for `gptel-get-token-count'.
(defvar gptel-get-token-count--handlers
  `((WAIT ,#'gptel--url-get-response-token-count)
    (DONE ,#'gptel--fsm-last)))

;;; Send query to get token count, handle response.
;;;###autoload
(defun gptel-get-token-count (&optional arg)
  "Copied and edited `gptel-send'

By default, the contents of the buffer up to the cursor position
are sent.  If the region is active, its contents are sent
instead."
  (interactive "P")
  (if (and arg (require 'gptel-transient nil t))
      (call-interactively #'gptel-menu)
    (gptel--sanitize-model)
    (let ((fsm (gptel-make-fsm :handlers gptel-get-token-count--handlers
                               :table gptel-get-token-count--transitions))
;;; Dynamically bind `gptel-backend` to use the TOKEN COUNT API endpoint.  (This
;;; is the only hardcoded part for this proof of concept.  To avoid hardcoding
;;; we need to edit the core gptel code, which we try to avoid so that upgrading
;;; gptel does not wipe out the edits.
          (gptel-backend
           (gptel-make-anthropic "Claude" :key #'gptel-api-key
                                 :endpoint "/v1/messages/count_tokens")))
      (gptel-request nil
        :dry-run t
        :transforms gptel-prompt-transform-functions
        :callback (lambda (count info)
                    ;; TODO-1: Handle and test error cases

                    ;; TODO-2: the below update-status has not effect. (it maybe
                    ;; because on entering function `gptel--update-status',
                    ;; `header-line-format' is not a cons and the `(if consp
                    ;; ...)' is not true.
                    (let ((msg (format "Token count: %s"
                                       (if count
                                           (gptel--token-count-info count)
                                         "(nil)"))))
                      (gptel--update-status msg 'success)
                      (message msg)))
        :fsm fsm)

      (dev-msg "url: %s"
               (gptel-backend-url (plist-get (gptel-fsm-info fsm) :backend)))

;;; The above `gptel-request' is a `dry-run' so that the fsm does not transition
;;; to the next state which sends out the request.  But by now the fsm has
;;; gathered the relevant data including the current gptel context selected as
;;; part of the LLM Prompt.  This gives us an opportunity to alter the request
;;; orignally meant for the LLM MESSAGES API. The LLM TOKEN COUNT API needs less
;;; data which we now remove.
      (gptel--alter-request-for-token-count
       (plist-get (gptel-fsm-info fsm) :backend) fsm)

;;; Now that we have altered the request to suit the token count endpoint, let
;;; the fsm transition so that the actual request is sent out.

      (gptel--fsm-transition fsm)
      (message "Querying (token count) %s..."
               (thread-first (gptel-fsm-info fsm)
                             (plist-get :backend)
                             (or gptel-backend)
                             (gptel-backend-name))))
    (gptel--update-status " Getting token count..." 'warning)))

(cl-defgeneric gptel--alter-request-for-token-count (backend fsm)
  ;; This feature to querry the LLM API to count tokens tries to re-use existing
  ;; functionality to gather the selected gptel context. To make a request to
  ;; count tokens we start out with the existing functionality to make a request
  ;; to the LLM Prompt API.  This is because a request to the token counting API
  ;; is very similar.

  ;; This function is what trims down this request data.
  )

(cl-defmethod gptel--alter-request-for-token-count ((_backend gptel-anthropic)
                                                    fsm)
  (let ((data (plist-get (gptel-fsm-info fsm) :data)))
    ;; (dev-msg " *** orig-request-data ***: %s" data)
;;; Remove unwanted request data (not needed for token count api)
    (cl-remf data :max_tokens)
    (cl-remf data :metadata)
    (cl-remf data :service_tier)
    (cl-remf data :stop_sequences)
    (cl-remf data :stream)
    (cl-remf data :temperature)
    (cl-remf data :top_k)
    (cl-remf data :top_p)
    ;; TODO: Check if it is a good idea to put back this removed data after the
    ;; request is sent out?  Maybe not be needed because an fsm for a prompt
    ;; request would be a new fsm instance.
    ;; (dev-msg "*** altered-request-data ***: %s\n" data) ;; note removed fields
    ))

(defun gptel--url-get-response-token-count (fsm)
  "Copied from `gptel--url-get-response'

Fetch response to prompt in state FSM from the LLM.

FSM is the state machine driving this request.  Its INFO slot
contains the data required for setting up the request.  INFO is a
plist with the following keys, among others:
- :data     (the data being sent)
- :buffer   (the gptel buffer)
- :position (marker at which to insert the response).
- :callback (optional, the request callback)

Call CALLBACK with the response and INFO afterwards.  If omitted
the response is inserted into the current buffer after point."
  (let* (;; during dev, don't inhibit messages so that we can edebug.
         ;; after dev uncomment the next two lines.
         ;; (inhibit-message t)
         ;; (message-log-max nil)
         (url-request-method "POST")
         (info (gptel-fsm-info fsm))
         ;; We have to let-bind the following two since their dynamic
         ;; values are used for key lookup and url resolution
         (gptel-backend (plist-get info :backend))
         (gptel-model (plist-get info :model))
         (url-request-extra-headers
          (append '(("Content-Type" . "application/json"))
                  (when-let* ((header (gptel-backend-header gptel-backend)))
                    (if (functionp header)
                        (funcall header) header))))
         ;; (callback (or (plist-get info :callback) ;if not the first run
         ;;               #'gptel--insert-response)) ;default callback
         ;; NOTE: We don't need the decode-coding-string dance here since we
         ;; don't pass it to the OS environment and Curl.
         (url-request-data
          (encode-coding-string (gptel--json-encode (plist-get info :data)) 'utf-8)))
    ;; (when (with-current-buffer (plist-get info :buffer)
    ;;         (and (derived-mode-p 'org-mode)
    ;;              gptel-org-convert-response))
    ;;   (plist-put info :transformer #'gptel--convert-markdown->org))
    ;; (plist-put info :callback callback)
    (when gptel-log-level               ;logging
      (when (eq gptel-log-level 'debug)
        (gptel--log (gptel--json-encode
                     (mapcar (lambda (pair) (cons (intern (car pair)) (cdr pair)))
                             url-request-extra-headers))
                    "request headers"))
      (gptel--log url-request-data "request body"))
    (let ((proc-buf
           (url-retrieve (let ((backend-url (gptel-backend-url gptel-backend)))
                           (if (functionp backend-url)
                               (funcall backend-url) backend-url))
                         (lambda (_)
                           ;; (set-buffer-multibyte t)
                           ;; (set-buffer-file-coding-system 'utf-8-unix)
                           (pcase-let ((`(,response ,http-status ,http-msg ,error)
                                        (gptel--url-parse-response-token-count
                                         (plist-get info :backend) info))
                                       (buf (current-buffer)))
                             (plist-put info :http-status http-status)
                             (plist-put info :status http-msg)
                             (when-let* ((cb (plist-get info :callback))
                                         ((functionp cb)))
                               (funcall cb response info))
                             (gptel--fsm-transition fsm) ;WAIT -> TYPE
                             (when error (plist-put info :error error))
                             ;; (when response ;Look for a reasoning block
                             ;;   (if (string-match-p "^\\s-*<think>" response)
                             ;;       (when-let* ((idx (string-search "</think>" response)))
                             ;;         (with-demoted-errors "gptel callback error: %S"
                             ;;           (funcall callback
                             ;;                    (cons 'reasoning
                             ;;                          (substring response nil (+ idx 8)))
                             ;;                    info))
                             ;;         (setq response (string-trim-left
                             ;;                         (substring response (+ idx 8)))))
                             ;;     (when-let* ((reasoning (plist-get info :reasoning))
                             ;;                 ((stringp reasoning)))
                             ;;       (funcall callback (cons 'reasoning reasoning) info))))
                             ;; (when (or response (not (member http-status '("200" "100"))))
                             ;;   (with-demoted-errors "gptel callback error: %S"
                             ;;     (funcall callback response info)))
                             ;; (gptel--fsm-transition fsm) ;TYPE -> next
                             (setf (alist-get buf gptel--request-alist nil 'remove) nil)
                             (kill-buffer buf)))
                         nil t nil)))
      ;; TODO: Add transformer here.
      (setf (alist-get proc-buf gptel--request-alist)
            (cons fsm
                  #'(lambda ()
                      (plist-put info :callback #'ignore)
                      (let (kill-buffer-query-functions)
                        ;;Can't stop url-retrieve process
                        (kill-buffer proc-buf))))))))

(defun gptel--url-parse-response-token-count (backend proc-info)
  "Copied from gptel--url-parse-response.

Parse response from BACKEND with PROC-INFO."
  (when gptel-log-level                 ;logging
    (save-excursion
      (goto-char url-http-end-of-headers)
      (when (eq gptel-log-level 'debug)
        (gptel--log (gptel--json-encode (buffer-substring-no-properties (point-min) (point)))
                    "response headers"))
      (gptel--log (buffer-substring-no-properties (point) (point-max))
                  "response body")))
  (if-let* ((http-msg (string-trim (buffer-substring (line-beginning-position)
                                                     (line-end-position))))
            (http-status
             (save-match-data
               (and (string-match "HTTP/[.0-9]+ +\\([0-9]+\\)" http-msg)
                    (match-string 1 http-msg))))
            (response (progn (goto-char url-http-end-of-headers)
                             (condition-case nil
                                 (gptel--json-read)
                               (error 'json-read-error)))))
      (cond
       ;; FIXME Handle the case where HTTP 100 is followed by HTTP (not 200) BUG #194
       ((or (memq url-http-response-status '(200 100))
            (string-match-p "\\(?:1\\|2\\)00 OK" http-msg))
        (list (gptel--parse-response-token-count backend response proc-info)
              http-status http-msg))
       ((and-let* ((error-data
                    (cond ((plistp response) (plist-get response :error))
                          ((arrayp response)
                           (cl-some (lambda (el) (plist-get el :error)) response)))))
          (list nil http-status http-msg error-data)))
       ((eq response 'json-read-error)
        (list nil http-status (concat "(" http-msg ") Malformed JSON in response.") "json-read-error"))
       (t (list nil http-status (concat "(" http-msg ") Could not parse HTTP response.")
                "Could not parse HTTP response.")))
    (list nil (concat "(" http-msg ") Could not parse HTTP response.")
          "Could not parse HTTP response.")))

(cl-defgeneric gptel--parse-response-token-count (backend response proc-info)
  "Copied from cl-defgeneric gptel--parse-response

Response extractor for LLM requests.

BACKEND is the LLM backend in use.

RESPONSE is the parsed JSON of the response, as a plist.

PROC-INFO is a plist with process information and other context.
See `gptel-curl--get-response' for its contents.")

(cl-defmethod gptel--parse-response-token-count ((_backend gptel-anthropic) response info)
  "Copied from cl-defmethod gptel--parse-response

Parse an Anthropic (non-streaming) toekn count RESPONSE and return the count.

Mutate state INFO with response metadata."
  (let ((count (plist-get response :input_tokens)))
    (cl-ecase (type-of count)
      ('integer count)
      ('string (string-to-number count)))))

;;; utils

(defun gptel--token-count-info (count)
  "If a count is small (doesn't need to be humanized) then just return it.
Else return an approximate humanized form of the count along with
the accurate count.

Examples:

(gptel-token-count-to-string 500) => \"500\"
(gptel-token-count-to-string 999) => \"999\"
(gptel-token-count-to-string 1200) => \"1.2k (1,200)\"
(gptel-token-count-to-string 6050000) => \"6.0M (6,050,000)\"
"
  (let* ((str (number-to-string count))
         (humanized (humanize-count count)))
    (if (string= str humanized)
        str
      (format "%s (%s)" humanized (add-number-grouping count)))))

;; see github.com/kayomarz/snippets/blob/main/lisp/humanize.el
;; Started out with code by github.com/dustin and modified it. See
;; github.com/dustin/snippets/blob/8801dbba7dd167e6bb14da6a46230d5c25318ec5/lisp/humanize.el
(defun humanize-count (count)
  "Humanize COUNT (a positive integer).  Returns a string
representing the humanized form."
  (let ((base 1000)
        (labels '("" "k" "M" "G" "T" "P" "E" "Z" "Y" "R" "Q")))
    (if (< count base)
        ;; if count is less than the base, just convert it to a string
        (number-to-string count)
      (let ((e (floor (log count base))))
        (if (>= e (length labels))
            ;; if count is larger than known suffixes, just convert it to string
            (number-to-string count)
          (let* ((suffix (nth e labels))
                 (val (/ count (expt base e) 1.0)) ;; the 1.0 is to get a float
                 (f (if (< val 10) "%.1f%s" "%.0f%s")))
            (format f val suffix)))))))

;;; https://www.emacswiki.org/emacs/AddCommasToNumbers
(defun add-number-grouping (number &optional separator)
  "Add commas to NUMBER and return it as a string.
    Optional SEPARATOR is the string to use to separate groups.
    It defaults to a comma."
  (let ((num (number-to-string number))
        (op (or separator ",")))
    (while (string-match "\\(.*[0-9]\\)\\([0-9][0-9][0-9].*\\)" num)
      (setq num (concat
                 (match-string 1 num) op
                 (match-string 2 num))))
    num))

(provide 'gptel-token-count)
