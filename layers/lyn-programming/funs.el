;;; funs.el --- lyn-programming layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Lyn <lyn1990828@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; modify new pyenv-mode-set use lyn-programming/pyenv-mode-read-version
(defun lyn-programming/pyenv-mode-set (version)
  "Set python shell VERSION."
  (interactive (list (lyn-programming/pyenv-mode-read-version)))
  (pythonic-activate (pyenv-mode-full-path version))
  (setenv "PYENV_VERSION" version)
  (force-mode-line-update))

;; fix the pyenv use anaconda virtual env problem
(defun lyn-programming/pyenv-mode-read-version ()
  "Read virtual environment from user input."
  (completing-read "Pyenv: " (mapcar
                              (lambda (i) (replace-regexp-in-string
                                       "/usr/local/Cellar/pyenv/1.1.3/versions/"
                                       ""
                                       i))
                              (pyenv-mode-versions))))
