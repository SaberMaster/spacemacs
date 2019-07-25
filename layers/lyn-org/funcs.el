
;;; funcs.el --- lyn-org layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;

;; Author: Lyn <lyn1990828@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(require 'cl)

(defun lyn/org-default-image-path ()
  "used for get different image save path"
  (setq image-floder-name "_imgs")
  (setq org-notes-path "~/org-notes")
  (if (my-project-name-contains-substring "org-notes")
      (progn
        (concat (expand-file-name org-notes-path)
                      "/source/"
                      image-floder-name
                      "/"
                      (file-name-directory (file-relative-name (buffer-file-name) org-notes-path))
                      (file-name-base (buffer-file-name))
                      "/"))
    (progn
      (concat (file-name-directory (buffer-file-name))
              image-floder-name
              "/"
              (file-name-base (buffer-file-name))
              "/"))))

(defun lyn/capture-screenshot-simple (basename)
  "Take a screenshot into a time stamped unique-named file in the
  same directory as the org-buffer/markdown-buffer and insert a link to this file."
  (interactive "sScreenshot name: ")
  (if (equal basename "")
      (setq basename (format-time-string "%Y%m%d_%H%M%S")))
  (setq image-save-path (lyn/org-default-image-path))
  (unless (file-directory-p image-save-path)
    (make-directory image-save-path 't))
  (setq final-image-full-path (concat image-save-path basename ".png"))
  (setq relativepath (concat "./" (file-relative-name final-image-full-path (file-name-directory (buffer-file-name)))))
  (call-process "screencapture" nil nil nil "-i" final-image-full-path)
  (if (executable-find "convert")
      (progn
        (setq resize-command-str (format "convert %s -resize 800x600 %s" final-image-full-path final-image-full-path))
        (shell-command-to-string resize-command-str)))
  (zilongshanren//insert-org-or-md-img-link "" relativepath)
  (insert "\n"))


(defun lyn/init-org-export-latex-style ()
  (require 'ox-publish)
  (setq org-latex-listings-options '(
                                     ("breaklines" "true")
                                     ("numbers" "left")
                                     ("framexleftmargin" "0mm")
                                     ))
  )
