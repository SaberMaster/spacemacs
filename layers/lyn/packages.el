;;; packages.el --- lyn layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Lyn <lyn1990828@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Code:

(defconst lyn-packages
  '())

(configuration-layer/declare-layers '(
                                      lyn-programming
                                      lyn-org
                                      lyn-ui
                                      ))
