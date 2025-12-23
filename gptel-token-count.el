;;; -*- lexical-binding: t; -*-

;;;; Query LLM APIs to count input tokens for the current gptel context.

;;; State machine for driving requests
(defvar gptel-get-token-count--transitions
  `((INIT . ((t                       . WAIT)))
    (WAIT . ((t                       . TYPE)))
    (TYPE . ((,#'gptel--error-p       . ERRS)
             (,#'gptel--tool-use-p    . TOOL)
             (t                       . DONE)))
    (TOOL . ((,#'gptel--error-p       . ERRS)
             (,#'gptel--tool-result-p . WAIT)
             (t                       . DONE))))
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
    (let ((fsm (gptel-make-fsm :handlers gptel-get-token-count--handlers)))
      (gptel-request nil
        :stream gptel-stream
        :transforms gptel-prompt-transform-functions
        :fsm fsm)
      (message "Querying %s..."
               (thread-first (gptel-fsm-info fsm)
                             (plist-get :backend)
                             (or gptel-backend)
                             (gptel-backend-name))))
    (gptel--update-status " Waiting..." 'warning)))

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
  (let* ((inhibit-message t)
         (message-log-max nil)
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
         (callback (or (plist-get info :callback) ;if not the first run
                       #'gptel--insert-response)) ;default callback
         ;; NOTE: We don't need the decode-coding-string dance here since we
         ;; don't pass it to the OS environment and Curl.
         (url-request-data
          (gptel--json-encode (plist-get info :data))))
    (when (with-current-buffer (plist-get info :buffer)
            (and (derived-mode-p 'org-mode)
                 gptel-org-convert-response))
      (plist-put info :transformer #'gptel--convert-markdown->org))
    (plist-put info :callback callback)
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
                           (set-buffer-multibyte t)
                           (set-buffer-file-coding-system 'utf-8-unix)
                           (pcase-let ((`(,response ,http-status ,http-msg ,error)
                                        (gptel--url-parse-response-token-count
                                         (plist-get info :backend) info))
                                       (buf (current-buffer)))
                             (plist-put info :http-status http-status)
                             (plist-put info :status http-msg)
                             (gptel--fsm-transition fsm) ;WAIT -> TYPE
                             (when error (plist-put info :error error))
                             (when response ;Look for a reasoning block
                               (if (string-match-p "^\\s-*<think>" response)
                                   (when-let* ((idx (string-search "</think>" response)))
                                     (with-demoted-errors "gptel callback error: %S"
                                       (funcall callback
                                                (cons 'reasoning
                                                      (substring response nil (+ idx 8)))
                                                info))
                                     (setq response (string-trim-left
                                                     (substring response (+ idx 8)))))
                                 (when-let* ((reasoning (plist-get info :reasoning))
                                             ((stringp reasoning)))
                                   (funcall callback (cons 'reasoning reasoning) info))))
                             (when (or response (not (member http-status '("200" "100"))))
                               (with-demoted-errors "gptel callback error: %S"
                                 (funcall callback response info)))
                             (gptel--fsm-transition fsm) ;TYPE -> next
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
        (list (and-let* ((resp (gptel--parse-response-token-count backend response proc-info))
                         ((not (string-blank-p resp))))
                (string-trim resp))
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

Parse an Anthropic (non-streaming) RESPONSE and return response text.

Mutate state INFO with response metadata."
  (plist-put info :stop-reason (plist-get response :stop_reason))
  (plist-put info :output-tokens
             (map-nested-elt response '(:usage :output_tokens)))
  (cl-loop
   with content = (plist-get response :content)
   for cblock across content
   for type = (plist-get cblock :type)
   if (equal type "text")
   ;; TODO(tool) can :text be :null?
   collect (plist-get cblock :text) into content-strs
   else if (equal type "tool_use")
   collect cblock into tool-use
   else if (equal type "thinking")
   do
   (plist-put
    info :reasoning
    (concat (plist-get info :reasoning)
            (plist-get cblock :thinking)))
   finally do
   (when tool-use
     ;; First, add the tool call to the prompts list
     (let* ((data (plist-get info :data))
            (prompts (plist-get data :messages)))
       (plist-put
        data :messages
        (vconcat prompts `((:role "assistant" :content ,content)))))
     ;; Then capture the tool call data for running the tool
     (cl-loop
      for call-raw in tool-use
      for call = (copy-sequence call-raw) do
      (plist-put call :args (plist-get call :input))
      (plist-put call :input nil)
      (plist-put call :id (plist-get call :id))
      collect call into calls
      finally do (plist-put info :tool-use calls)))
   finally return
   (and content-strs (apply #'concat content-strs))))

(provide 'gptel-token-count)
