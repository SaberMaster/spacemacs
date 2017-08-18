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

(defun lyn/capture-screenshot-simple (basename)
  "Take a screenshot into a time stamped unique-named file in the
  same directory as the org-buffer/markdown-buffer and insert a link to this file."
  (interactive "sScreenshot name: ")
  (if (equal basename "")
      (setq basename (format-time-string "%Y%m%d_%H%M%S")))
  (setq image-floder-name
        "_imgs")
  (if (my-project-name-contains-substring "org-notes")
      (progn
        (setq directory-path
              (concat (file-name-directory (buffer-file-name))
                      "../source/"
                      image-floder-name
                      "/"
                      (file-name-base (buffer-file-name))
                      "/"
                      ))
        (setq relativepath
              (concat "../source/"
                      image-floder-name
                      "/"
                      (file-name-base (buffer-file-name))
                      "/"
                      (file-name-base (buffer-file-name))
                      "_"
                      basename
                      ".png"))
        )
    (progn
      (setq directory-path
            (concat (file-name-directory (buffer-file-name))
                    image-floder-name
                    "/"
                    ))
      (setq relativepath
            (concat "./"
                    image-floder-name
                    "/"
                    (file-name-base (buffer-file-name))
                    "_"
                    basename
                    ".png"))
      )
    )

  (unless (file-directory-p directory-path)
    (make-directory directory-path 't))
  (setq fullpath
        (concat directory-path
                (file-name-base (buffer-file-name))
                "_"
                basename))
  (setq final-image-full-path (concat fullpath ".png"))
  (call-process "screencapture" nil nil nil "-i" final-image-full-path)
  (if (executable-find "convert")
      (progn
        (setq resize-command-str (format "convert %s -resize 800x600 %s" final-image-full-path final-image-full-path))
        (shell-command-to-string resize-command-str)))
  (zilongshanren//insert-org-or-md-img-link "" relativepath)
  (insert "\n"))
