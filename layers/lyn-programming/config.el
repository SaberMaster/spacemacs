;;; config.el --- lyn-programming layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Lyn <lyn1990828@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; you can also set these environment variables in .zshenv or .profile
;; set pyenv workon_home for active virtual env on org code block
(setenv "WORKON_HOME" "/usr/local/opt/pyenv/versions")
;; fix pyenv can't get right dir error
(setenv "PYENV_ROOT" "/usr/local/opt/pyenv")
(add-hook 'sh-mode-hook 'flycheck-mode)
