;;; config.el --- lyn-org layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;

;; Author: Lyn <lyn1990828@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun lyn/org-ispell ()
  "Configure `ispell-skip-region-alist' for `org-mode'."
  (make-local-variable 'ispell-skip-region-alist)
  (add-to-list 'ispell-skip-region-alist '("^[\s\t]*#\\+BEGIN_SRC" . "^[\s\t]*#\\+END_SRC")))

;; fix zilongshanren org ispell bug
(add-hook 'org-mode-hook #'lyn/org-ispell)


(defun lyn/org-download-dir ()
  "Configure `org-download-image-dir` for every org buffer"
  ;; the org-download-image-dir is a local buffer variable
  (setq org-download-image-dir (lyn/org-default-image-path)))

;; format org-download image save path for every org buffer
(add-hook 'org-mode-hook #'lyn/org-download-dir)
