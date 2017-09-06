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
    org-download
    )
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
      (setq org-html-head lyn-website-html-blog-head))))

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

(defun lyn-org/init-scimax-org-babel-ipython ()
  (use-package  scimax-org-babel-ipython
    :config
    (progn
      (defun org-babel-src-block-put-property (property value)
        "Add a text property to the src-block"
        (save-excursion
          (goto-char (org-element-property :begin (org-element-context)))
          (put-text-property (line-beginning-position) (line-end-position) property value)
          ;; auto indent
          (org-indent-line)))

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
      )))


;;; packages.el ends here
