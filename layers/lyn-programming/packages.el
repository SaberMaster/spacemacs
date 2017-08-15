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
    ))


;;; packages.el ends here

(defun lyn-programming/post-init-company ()
  (progn
    (when (configuration-layer/package-usedp 'company)
      (spacemacs|add-company-backends :modes sml-mode racket-mode)
      )
    ))
