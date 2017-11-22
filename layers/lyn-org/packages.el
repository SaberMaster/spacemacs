;;; packages.el --- lyn-org layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;

;; Author: Lyn <lyn1990828@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `lyn-org-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `lyn-org/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `lyn-org/pre-init-PACKAGE' and/or
;;   `lyn-org/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst lyn-org-packages
  '(
    (org :location built-in)
    (scimax-org-babel-ipython :location local)
    org-edit-latex
    org-download)
  "the list of Lisp packages required by the lyn-org layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the         [list(adfbdf)] + [None] + adfbdf.values.tolist()
 package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The follo         [list(adfbdf)] + [None] + adfbdf.values.tolist()
         [list(adfbdf)] + [None] + adfbdf.values.tolist()
wing values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")

(defun lyn-org/post-init-org ()
  (with-eval-after-load 'org
    (progn
      ;; C-c C-e h  output html add css style
      (defvar lyn-website-html-blog-head
        (concat "<link rel=\"stylesheet\" type=\"text/css\" href=\""
                (expand-file-name
                 "~/org-notes/css/worg.css")
                "\"/> \n")
        )
      (add-to-list 'company-backends-python-mode 'ob-ipython-company-backend)
      ;; (setq company-backends-python-mode 'ob-ipython-company-backend)
      (setq org-export-with-sub-superscripts nil)
      (setq org-html-head lyn-website-html-blog-head)
      ;; set pdf export style
      (lyn/init-org-export-latex-style))))

(defun lyn-org/post-init-org-download ()
  (with-eval-after-load 'org-download
    (progn
      ;; set org-download-screenshot-method for mac
      (setq org-download-screenshot-method "screencapture -i %s")
      ;; set ignore lvl
      (setq-default org-download-heading-lvl nil)
      ;; set download dir
      (setq-default org-download-image-dir "./_img/")

      (org-download-enable))))


(defun lyn-org/init-org-edit-latex ()
  (use-package org-edit-latex
    :defer t
    :init
    (add-hook 'org-mode-hook 'org-edit-latex-mode)
    ))

(defun lyn-org/init-scimax-org-babel-ipython ()
  (use-package  scimax-org-babel-ipython
    :config
    (progn
      ;; add auto indent
      (defun org-babel-src-block-put-property (property value)
        "Add a text property to the src-block"
        (save-excursion
          (goto-char (org-element-property :begin (org-element-context)))
          (put-text-property (line-beginning-position) (line-end-position) property value)
          ;; auto indent
          (org-indent-line)))

      ;; change result format
      (defun ob-ipython--async-callback (status &rest args)
        "Callback function for `ob-ipython--execute-request-asynchronously'.
It replaces the output in the results."
        (ob-ipython-log "Entering callback for %s" *org-babel-async-ipython-running-cell*)
        (let* ((ret (ob-ipython--eval (if (>= (url-http-parse-response) 400)
                                          (ob-ipython--dump-error (buffer-string))
                                        (goto-char url-http-end-of-headers)
                                        (ob-ipython-log "http request: %s"
                                                        (buffer-substring (point-min) (point-max)))
                                        (let* ((json-array-type 'list)
                                               (json (json-read)))
                                          ;; we will need this in the traceback buffer
                                          (setq *org-babel-ipython-exception-buffer* (car args))
                                          ;; This means there was an exception.
                                          (when (string= "error"
                                                         (cdr
                                                          (assoc 'msg_type (elt json 0))))
                                            (with-current-buffer (car args)
                                              (org-babel-goto-named-src-block
                                               (cdr (ob-ipython-get-running)))
                                              (org-babel-remove-result)))
                                          json))))
               (result (cdr (assoc :result ret)))
               (output (cdr (assoc :output ret)))
               params
               current-cell name
               (result-type))

          (with-current-buffer (car args)
            (setq current-cell (ob-ipython-get-running)
                  name (cdr current-cell))
            (save-excursion
              (org-babel-goto-named-src-block name)
              (setq result-type (org-babel-src-block-get-property 'org-babel-ipython-result-type))
              (org-babel-src-block-put-property 'org-babel-ipython-executed  t)
              (ob-ipython-log "Got a result-type of %s\n return from the kernel:  %S" result-type ret)
              (setq params (third (org-babel-get-src-block-info)))
              (org-babel-remove-result)
              (cond
               ((string= "output" result-type)
                (let ((res (concat
                            ;; change here to fromat code
                            (if (string-empty-p output)
                                output
                              (format
                               "#+BEGIN_EXAMPLE \n%s\n#+END_EXAMPLE\n"
                               output))
                            ;; output
                            (ob-ipython--format-result
                             result (cdr (assoc :ob-ipython-results params))))))
                  (when (not (string= "" (s-trim res)))
                    (org-babel-insert-result
                     (s-trim res)
                     (cdr (assoc :result-params (third (org-babel-get-src-block-info))))))))
               ((string= "value" result-type)
                (org-babel-insert-result
                 (cdr (assoc 'text/plain result))
                 (cdr (assoc :result-params (third (org-babel-get-src-block-info)))))))
              (org-redisplay-inline-images))
            (ob-ipython-set-running-cell nil)
            (setq header-line-format (format "The kernel is %s" (ob-ipython-get-kernel-name))))

          (let ((traceback (get-buffer "*ob-ipython-traceback*")))
            (when traceback (kill-buffer traceback)))
          ;; see if there is another thing in the queue.
          (org-babel-async-ipython-process-queue)))

      ;; change result format
      (defun org-babel-execute:ipython (body params)
        "Execute a block of IPython code with Babel.
This function is called by `org-babel-execute-src-block'."
        (let* ((file (cdr (assoc :file params)))
               (session (cdr (assoc :session params)))
               (async (cdr (assoc :async params)))
               (result-type (cdr (assoc :result-type params)))
               results)
          (org-babel-ipython-initiate-session session params)

          ;; Check the current results for inline images and delete the files.
          (let ((location (org-babel-where-is-src-block-result))
                current-results)
            (when location
              (save-excursion
                (goto-char location)
                (when (looking-at (concat org-babel-result-regexp ".*$"))
                  (setq results (buffer-substring-no-properties
                                 location
                                 (save-excursion
                                   (forward-line 1) (org-babel-result-end)))))))
            (with-temp-buffer
              (insert (or results ""))
              (goto-char (point-min))
              (while (re-search-forward
                      "\\[\\[file:\\(ipython-inline-images/ob-ipython-.*?\\)\\]\\]" nil t)
                (let ((f (match-string 1)))
                  (when (file-exists-p f)
                    (delete-file f))))))

          (-when-let (ret (ob-ipython--eval
                           (ob-ipython--execute-request
                            (org-babel-expand-body:generic
                             (encode-coding-string body 'utf-8)
                             params (org-babel-variable-assignments:python params))
                            (ob-ipython--normalize-session session))))
            (let ((result (cdr (assoc :result ret)))
                  (output (cdr (assoc :output ret))))
              (if (eq result-type 'output)
                  (concat
                   ;; change here to fromat code
                   (if (string-empty-p (s-trim output))
                       (s-trim output)
                     (format
                      "#+BEGIN_EXAMPLE \n%s\n#+END_EXAMPLE\n"
                      (s-trim output)))
                   ;; output
                   (ob-ipython--format-result
                    result
                    (cdr (assoc :ob-ipython-results params))))
                ;; The result here is a value. We should still get inline images though.
                (ob-ipython--create-stdout-buffer output)
                (ob-ipython--format-result
                 result (cdr (assoc :ob-ipython-results params))))))))

      ;; handle result
      (defun ob-ipython--format-result (result result-type)
        "Format a RESULT from an ipython cell.
Return RESULT-TYPE if specified. This comes from a header argument :ob-ipython-results"
        (cl-flet ((format-result (type value)
                                 (case type
                                   ;; change here
                                   ;; ('text/plain (concat value "\n"))
                                   ('text/plain (if (string-empty-p value)
                                                    value
                                                  (format
                                                   "#+BEGIN_EXAMPLE \n%s\n#+END_EXAMPLE\n"
                                                   value)))
                                   ('text/html (format
                                                "#+BEGIN_EXPORT HTML\n%s\n#+END_EXPORT\n"
                                                value))
                                   ('text/latex (format
                                                 "#+BEGIN_EXPORT latex\n%s\n#+END_EXPORT\n"
                                                 values))
                                   ('image/png (concat (ob-ipython-inline-image value) "\n"))))
                  (select-result-type (type result)
                                      (if type
                                          (--filter (eq (car it) (intern type)) result)
                                        result)))
          (->> result
               (select-result-type result-type)
               (--map (format-result (car it) (cdr it)))
               (apply #'concat "\n"))))
      )
    ;;** fixing ob-ipython-inspect cant open with `C-c ''
    ;; bet as i change it we cant assign :session for ipython code block, its session name is `ipython'
    (defun ob-ipython--inspect-request (code &optional pos detail)
      "This function is used to inspect code at a position.
This can provide information about the type, etc."
      (let ((url-request-data (json-encode `((code . ,code)
                                             (pos . ,(or pos (length code)))
                                             (detail . ,(or detail 0)))))
            (url-request-method "POST"))
        (with-current-buffer (url-retrieve-synchronously
                              (format "http://%s:%d/inspect/ipython"
                                      ob-ipython-driver-hostname
                                      ob-ipython-driver-port))
          (if (>= (url-http-parse-response) 400)
              (ob-ipython--dump-error (buffer-string))
            (goto-char url-http-end-of-headers)
            (let ((json-array-type 'list))
              (json-read))))))

    ;; This allows you to get completion from the ipython kernel.
    (defun ob-ipython--complete-request (code &optional pos)
      "Get completion candidates for the thing at POS from the kernel."
      (let ((url-request-data (json-encode `((code . ,code)
                                             (cursor_pos . ,(or pos (length code))))))
            (url-request-method "POST"))
        (with-current-buffer (url-retrieve-synchronously
                              (format "http://%s:%d/complete/ipython"
                                      ob-ipython-driver-hostname
                                      ob-ipython-driver-port))
          (if (>= (url-http-parse-response) 400)
              (ob-ipython--dump-error (buffer-string))
            (goto-char url-http-end-of-headers)
            (let ((json-array-type 'list))
              (json-read))))))

    ;; (defun ob-ipython-complete ()
    ;;   "Get completion candidates for the thing at point."
    ;;   (if (ob-ipython-get-running)
    ;;       (message "The kernel is busy running %s." (cdr (ob-ipython-get-running)))
    ;;     (save-restriction
    ;;       (if (org-in-src-block-p)
    ;;           (progn
    ;;             (when (org-in-src-block-p) (org-narrow-to-block))
    ;;             (-if-let (result (->> (ob-ipython--complete-request
    ;;                                    (buffer-substring-no-properties (point-min) (point-max))
    ;;                                    (- (point) (point-min)))
    ;;                                   car
    ;;                                   (assoc 'content)))
    ;;                 (list
    ;;                  (cdr (assoc 'matches result))
    ;;                  (cdr (assoc 'cursor_start result))
    ;;                  (cdr (assoc 'cursor_end result))))
    ;;             )
    ;;         (progn
    ;;           (-if-let (result (->> (ob-ipython--complete-request
    ;;                                  (buffer-substring-no-properties (point-min) (point-max))
    ;;                                  (- (point) (point-min)))
    ;;                                 car
    ;;                                 (assoc 'content)))
    ;;               (list
    ;;                (cdr (assoc 'matches result))
    ;;                (cdr (assoc 'cursor_start result))
    ;;                (cdr (assoc 'cursor_end result))))
    ;;           )
    ;;         )

    ;;       (message "abc")
    ;;       )))


    (defvar ob-ipython-syntax-table
      (make-syntax-table org-mode-syntax-table))

    (modify-syntax-entry ?. "_." ob-ipython-syntax-table)
    (modify-syntax-entry ?= ".=" ob-ipython-syntax-table)
    (modify-syntax-entry ?' "|'" ob-ipython-syntax-table)

    ;; This is a company backend to get completion while typing in org-mode.
    (defun ob-ipython-company-backend (command &optional arg &rest ignored)
      (interactive (list 'interactive))
      (if (and
           (not (ob-ipython-get-running))
           (or
            (and
             (org-in-src-block-p)
             (member (first (org-babel-get-src-block-info)) '("python" "ipython")))
            ob-ipython-mode
            ))
          (cl-case command
            (interactive (company-begin-backend 'ob-ipython-company-backend))
            (prefix (with-syntax-table ob-ipython-syntax-table
                      (when (looking-back "\\_<[a-zA-Z][a-zA-Z0-9._]*"
                                          (line-beginning-position))
                        (match-string 0))))
            (candidates (car (ob-ipython-complete)))
            ;; sorted => t if the list is already sorted
            (sorted t)
            ;; duplicates => t if there could be duplicates
            (duplicates nil)
            (require-match 'never))
        nil))

    ;; mode

    (define-minor-mode ob-ipython-mode
      ""
      nil
      " ipy"
      '())

    (defun org-babel-edit-prep:ipython (info)
      ;; TODO: based on kernel, should change the mode
      (ob-ipython-mode +1))
    ))


;;; packages.el ends here
