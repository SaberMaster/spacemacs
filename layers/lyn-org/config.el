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
  ;; check is is exist real buffer file name
  (if (buffer-file-name)
      (setq org-download-image-dir (lyn/org-default-image-path))))

;; format org-download image save path for every org buffer
(add-hook 'org-mode-hook #'lyn/org-download-dir)

;; org download html width
(setq org-download-image-html-width 500)

;; exec babel without confirm
(defun my-org-confirm-babel-evaluate (lang body)
  (not (member lang '("python" "octave" "ipython"))))

(setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)

;; when export never exec babel, use default result
(setq org-export-babel-evaluate nil)

;; dont use unqiue kernel, it may influence other org-babel
(setq ob-ipython-buffer-unique-kernel nil)
;; not not use async
(setq org-babel-async-ipython nil)
;; (add-to-list 'org-latex-minted-langs '(ipython "python"))
