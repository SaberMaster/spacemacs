;;; packages.el --- lyn-programming layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
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
;; added to `lyn-programming-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `lyn-programming/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `lyn-programming/pre-init-PACKAGE' and/or
;;   `lyn-programming/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst lyn-programming-packages
  '(
    company
    python
    ))


;;; packages.el ends here


(defun lyn-programming/post-init-company ()
  (progn
    (when (configuration-layer/package-usedp 'company)
      (spacemacs|add-company-backends :modes sml-mode racket-mode)
      )
    ))


(defun lyn-programming/post-init-python ()
  (progn
    ;; I based most of this on the vs.code implementation:
    ;; https://github.com/Microsoft/vscode-python/blob/master/src/client/activation/languageServer/languageServer.ts#L219
    ;; (it still took quite a while to get right, but here we are!)
    ;;  active pyenv when open a python file, the pyenv env will loading from `.python-version`
    ;; the the backend will use this python path
    (add-hook 'python-mode-hook 'pyenv-mode)
    (defun lsp-python-ms--extra-init-params (&optional workspace)
      "Return form describing parameters for language server.

Old lsp will pass in a WORKSPACE, new lsp has a global
lsp-workspace-root function that finds the current buffer's
workspace root.  If nothing works, default to the current file's
directory"
      (let ((workspace-root (if workspace (lsp--workspace-root workspace) (lsp-python-ms--workspace-root))))
        (cl-destructuring-bind (pyver _pysyspath)
            (lsp-python-ms--get-python-ver-and-syspath workspace-root)
          `(:interpreter
            (:properties (:InterpreterPath
                          ;; change method of obtain python exec path, get from pyenv, then the backend will change python version according to the pyenv
                          ,(spacemacs/pyenv-executable-find lsp-python-executable-cmd)
                          ;; this database dir will be created if required
                          ;; :DatabasePath ,(expand-file-name (directory-file-name lsp-python-ms-cache-dir))
                          :Version ,pyver))
            ;; preferredFormat "markdown" or "plaintext"
            ;; experiment to find what works best -- over here mostly plaintext
            :displayOptions (
                             :preferredFormat "markdown"
                             :trimDocumentationLines :json-false
                             :maxDocumentationLineLength 0
                             :trimDocumentationText :json-false
                             :maxDocumentationTextLength 0)
            :searchPaths ,(if lsp-python-ms-extra-paths
                              (vconcat lsp-python-ms-extra-paths nil)
                            [])
            :analysisUpdates t
            :asyncStartup t
            :typeStubSearchPaths ,(vector (concat lsp-python-ms-dir "Typeshed"))))))
    )
  )
