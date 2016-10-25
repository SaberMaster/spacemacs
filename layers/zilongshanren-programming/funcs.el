; -*- lexical-binding: t -*-
;;; funcs.el --- zilongshanren Layer packages File for Spacemacs
;;
;; Copyright (c) 2015-2016 zilongshanren 
;;
;; Author: zilongshanren <guanghui8827@gmail.com>
;; URL: https://github.com/zilongshanren/spacemacs-private
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun zilongshanren/comment-box (b e)
  "Draw a box comment around the region but arrange for the region
to extend to at least the fill column. Place the point after the
comment box."
  (interactive "r")
  (let ((e (copy-marker e t)))
    (goto-char b)
    (end-of-line)
    (insert-char ?  (- fill-column (current-column)))
    (comment-box b e 1)
    (goto-char e)
    (set-marker e nil)))



;; "http://stackoverflow.com/questions/2242572/emacs-todo-indicator-at-left-side"
(defun zilongshanren/annotate-todo ()
  "put fringe marker on TODO: lines in the curent buffer"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "TODO:" nil t)
      (let ((overlay (make-overlay (- (point) 5) (point))))
        (overlay-put overlay 'before-string (propertize "A"
                                                        'display '(left-fringe right-triangle)))))))


;;js2-mode enhancement
(defun zilongshanren/js2-which-function ()
  ;; clean the imenu cache
  ;; @see http://stackoverflow.com/questions/13426564/how-to-force-a-rescan-in-imenu-by-a-function
  (setq imenu--index-alist nil)
  (which-function-mode t)
  (which-function))

(defun zilongshanren/run-current-file ()
  "Compile and/or Execute the current file."
  (interactive)
  ;; (call-interactively #'compile-dwim-compile)
  (call-interactively #'compile-dwim-run))



(defun my-web-mode-indent-setup ()
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

(defun my-toggle-web-indent ()
  (interactive)
  ;; web development
  (if (eq major-mode 'json-mode)
      (progn
        (setq js-indent-level (if (= js-indent-level 2) 4 2))))

  (if (or (eq major-mode 'js-mode) (eq major-mode 'js2-mode))
      (progn
        (setq js-indent-level (if (= js-indent-level 2) 4 2))))

  (if (eq major-mode 'web-mode)
      (progn (setq web-mode-markup-indent-offset (if (= web-mode-markup-indent-offset 2) 4 2))
             (setq web-mode-css-indent-offset (if (= web-mode-css-indent-offset 2) 4 2))
             (setq web-mode-code-indent-offset (if (= web-mode-code-indent-offset 2) 4 2))))
  (if (eq major-mode 'css-mode)
      (setq css-indent-offset (if (= css-indent-offset 2) 4 2)))

  (setq indent-tabs-mode nil))

(defun zilongshanren/load-yasnippet ()
  (interactive)
  (unless yas-global-mode
    (progn
      (yas-global-mode 1)
      (setq my-snippet-dir (expand-file-name "~/.spacemacs.d/snippets"))
      (setq yas-snippet-dirs '(my-snippet-dir))
      (yas-load-directory my-snippet-dir)
      (setq yas-wrap-around-region t)))
  (yas-minor-mode 1))

(defun conditionally-enable-lispy ()
  (when (eq this-command 'eval-expression)
    (lispy-mode 1)))

(defun cmake-rename-buffer ()
  "Renames a CMakeLists.txt buffer to cmake-<directory name>."
  (interactive)
  (when (and (buffer-file-name)
             (string-match "CMakeLists.txt" (buffer-name)))
    (setq parent-dir (file-name-nondirectory
                      (directory-file-name
                       (file-name-directory (buffer-file-name)))))
    (setq new-buffer-name (concat "cmake-" parent-dir))
    (rename-buffer new-buffer-name t)))

(defun my-ts-mode-hook ()
  (when (eq major-mode 'typescript-mode)
    (progn
      (setq imenu-create-index-function 'js2-imenu-make-index)

      (with-eval-after-load 'flycheck

        (add-to-list 'flycheck-disabled-checkers 'javascript-eslint)
        (flycheck-disable-checker 'javascript-eslint)))))

(defun my-js2-mode-hook ()
  (progn
    (define-key js2-mode-map "\C-ci" 'js-doc-insert-function-doc-snippet)
    (define-key js2-mode-map "@" 'js-doc-insert-tag)
    (modify-syntax-entry ?_ "w")
    (which-function-mode t)
    (setq imenu-create-index-function 'js2-imenu-make-index)

    (setq mode-name "JS2")

    ;; (spacemacs/toggle-syntax-checking-on)
    (setq forward-sexp-function nil)
    (set (make-local-variable 'semantic-mode) nil)))

(defun my-which-function ()
  ;; clean the imenu cache
  ;; @see http://stackoverflow.com/questions/13426564/how-to-force-a-rescan-in-imenu-by-a-function
  (setq imenu--index-alist nil)
  (which-function))

(defun js2-imenu-make-index ()
  (interactive)
  (save-excursion
    ;; this imenu generic expression aims to exclude for, while, if when aims to match functions in
    ;; es6 js, e.g. ComponentDidMount(), render() function in React
    ;; https://emacs-china.org/t/topic/4538/7
    (defun js-exception-imenu-generic-expression-regexp ()
      ;; (async)? xxx (e) { }
      (if (re-search-backward "^[ \t]*\\(async\\|get\\|set\\)?[ \t]*\\([A-Za-z_$][A-Za-z0-9_$]+\\)[ \t]*([\{a-zA-Z0-9, \\\)\(/'\"\*=\t\n\}]*) *\{ *" nil t)
          (progn
            (if (member (match-string 2) '("for" "if" "while" "switch"))
                (js-exception-imenu-generic-expression-regexp)
              t))
        nil))
    ;; (setq imenu-generic-expression '((nil "describe\\(\"\\(.+\\)\"" 1)))
    (imenu--generic-function '(("describe" "\\s-*describe\\s-*(\\s-*[\"']\\(.+\\)[\"']\\s-*,.*" 1)
                               ("it" "\\s-*it\\s-*(\\s-*[\"']\\(.+\\)[\"']\\s-*,.*" 1)
                               ("test" "\\s-*test\\s-*(\\s-*[\"']\\(.+\\)[\"']\\s-*,.*" 1)
                               ("before" "\\s-*before\\s-*(\\s-*[\"']\\(.+\\)[\"']\\s-*,.*" 1)
                               ("after" "\\s-*after\\s-*(\\s-*[\"']\\(.+\\)[\"']\\s-*,.*" 1)
                               ("Controller" "[. \t]controller([ \t]*['\"]\\([^'\"]+\\)" 1)
                               ("Controller" "[. \t]controllerAs:[ \t]*['\"]\\([^'\"]+\\)" 1)
                               ("Filter" "[. \t]filter([ \t]*['\"]\\([^'\"]+\\)" 1)
                               ("State" "[. \t]state([ \t]*['\"]\\([^'\"]+\\)" 1)
                               ("Factory" "[. \t]factory([ \t]*['\"]\\([^'\"]+\\)" 1)
                               ("Service" "[. \t]service([ \t]*['\"]\\([^'\"]+\\)" 1)
                               ("Module" "[. \t]module([ \t]*['\"]\\([a-zA-Z0-9_\.]+\\)" 1)
                               ("ngRoute" "[. \t]when(\\(['\"][a-zA-Z0-9_\/]+['\"]\\)" 1)
                               ("Directive" "[. \t]directive([ \t]*['\"]\\([^'\"]+\\)" 1)
                               ("Event" "[. \t]\$on([ \t]*['\"]\\([^'\"]+\\)" 1)
                               ("Config" "[. \t]config([ \t]*function *( *\\([^\)]+\\)" 1)
                               ("Config" "[. \t]config([ \t]*\\[ *['\"]\\([^'\"]+\\)" 1)
                               ("OnChange" "[ \t]*\$(['\"]\\([^'\"]*\\)['\"]).*\.change *( *function" 1)
                               ("OnClick" "[ \t]*\$([ \t]*['\"]\\([^'\"]*\\)['\"]).*\.click *( *function" 1)
                               ("Watch" "[. \t]\$watch( *['\"]\\([^'\"]+\\)" 1)


                               ("ES6 Function" js-exception-imenu-generic-expression-regexp 2) ;; (async)? xxx (e) { }
                               ("Function" "function[ \t]+\\([a-zA-Z0-9_$.]+\\)[ \t]*(" 1)
                               ("Function" "^[ \t]*\\([a-zA-Z0-9_$.]+\\)[ \t]*=[ \t]*function[ \t]*(" 1)
                               ("Function" "^var[ \t]*\\([a-zA-Z0-9_$.]+\\)[ \t]*=[ \t]*function[ \t]*(" 1)
                               ("Function" "^[ \t]*static[ \t]*\\([a-zA-Z0-9_$.]+\\)[ \t]*()[ \t]*{" 1)
                               ("Function" "^[ \t]*\\([a-zA-Z0-9_$.]+\\)[ \t]*:[ \t]*function[ \t]*(" 1)

                               ("Class" "^[ \t]*var[ \t]*\\([0-9a-zA-Z]+\\)[ \t]*=[ \t]*\\([a-zA-Z]*\\).extend" 1)
                               ("Class" "^[ \t]*cc\.\\(.+\\)[ \t]*=[ \t]*cc\.\\(.+\\)\.extend" 1)

                               ("Task" "[. \t]task([ \t]*['\"]\\([^'\"]+\\)" 1)))))

(defun my-doxymacs-font-lock-hook ()
  (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
      (doxymacs-font-lock)))

(defun my-project-name-contains-substring (REGEX)
  (let ((dir (if (buffer-file-name)
                 (file-name-directory (buffer-file-name))
               "")))
    (string-match-p REGEX dir)))


(defvar my-tags-updated-time nil)

(defun my-create-tags-if-needed (SRC-DIR &optional FORCE)
  "return the full path of tags file"
  (let ((dir (file-name-as-directory (file-truename SRC-DIR)))
        file)
    (setq file (concat dir "TAGS"))
    (when (spacemacs/system-is-mswindows)
      (setq dir (substring dir 0 -1)))
    (when (or FORCE (not (file-exists-p file)))
      (message "Creating TAGS in %s ..." dir)
      (shell-command
       (format "ctags -f %s -e -R %s" file dir)))
    file))

(defun my-update-tags ()
  (interactive)
  "check the tags in tags-table-list and re-create it"
  (dolist (tag tags-table-list)
    (my-create-tags-if-needed (file-name-directory tag) t)))


(defun my-auto-update-tags-when-save (prefix)
      (interactive "P")
      (cond
       ((not my-tags-updated-time)
        (setq my-tags-updated-time (current-time)))

       ((and (not prefix)
             (< (- (float-time (current-time)) (float-time my-tags-updated-time)) 300))
        ;; < 300 seconds
        (message "no need to update the tags")
        )
       (t
        (setq my-tags-updated-time (current-time))
        (my-update-tags)
        (message "updated tags after %d seconds." (- (float-time (current-time)) (float-time my-tags-updated-time))))))


(defun my-setup-develop-environment ()
  (interactive)
  (when (my-project-name-contains-substring "guanghui")
    (cond
     ((my-project-name-contains-substring "cocos2d-x")
      ;; C++ project don't need html tags
      (setq tags-table-list (list (my-create-tags-if-needed "~/cocos2d-x/cocos"))))
     ((my-project-name-contains-substring "Github/fireball")
      (message "load tags for fireball engine repo...")
      ;; html project donot need C++ tags
      (setq tags-table-list (list (my-create-tags-if-needed "~/Github/fireball/engine/cocos2d")))))))

(defun zilongshanren-refresh-imenu-index ()
  (interactive)
  (when (or (eq major-mode 'js2-mode)
            (eq major-mode 'typescript-mode))
    (progn
      (setq imenu-create-index-function 'js2-imenu-make-index)

      ;; (when (eq major-mode 'typescript-mode)
      ;;   (setq imenu-create-index-function 'lsp--imenu-create-index))

      (setq-local company-backends (remove 'company-lsp company-backends))
      (setq-local company-backends '((company-dabbrev-code :with company-keywords company-etags)
                                     company-files company-dabbrev)))))
(defun file-string (file)
  "Read the contents of a file and return as a string."
  (with-current-buffer (find-file-noselect file)
    (buffer-string)))

(defun mu4e-toggle-org-mode ()
  (interactive)
  (cond
   ((eq major-mode 'mu4e-view-mode) (mu4e-org-mode))
   ((eq major-mode 'mu4e-org-mode) (mu4e-view-mode))
   ((eq major-mode 'mu4e-compose-mode) (org-mu4e-compose-org-mode))
   ((eq major-mode 'org-mu4e-compose-org-mode) (mu4e-compose-mode))))


(with-eval-after-load 'mu4e-view
  (spacemacs/set-leader-keys-for-major-mode 'mu4e-view-mode
    "to" 'mu4e-toggle-org-mode))


(with-eval-after-load 'mu4e-utils
  (spacemacs/set-leader-keys-for-major-mode 'mu4e-org-mode
    "to" 'mu4e-toggle-org-mode))

(with-eval-after-load 'mu4e-compose
  (spacemacs/set-leader-keys-for-major-mode 'mu4e-compose-mode "to" 'mu4e-toggle-org-mode))


(with-eval-after-load 'org-mu4e
  (setq org-mu4e-convert-to-html t)
  (spacemacs/set-leader-keys-for-major-mode 'org-mu4e-compose-org-mode "to" 'mu4e-toggle-org-mode)
  ;; current org-mu4e attachment has some error, delay fix
  (defun org~mu4e-mime-replace-images (str current-file)
    "Replace images in html files with cid links."
    (let (html-images)
      (cons
       (replace-regexp-in-string ;; replace images in html
        "src=\"\\([^\"]+\\)\""
        (lambda (text)
          (format
           "src=\"cid:%s\""
           (let* ((url (and (string-match "src=\"\\(file://\\)?\\([^\"]+\\)\"" text)
                            (match-string 2 text)))
                  (path (expand-file-name
                         url (file-name-directory current-file)))
                  (email-dir (concat temporary-file-directory "email_tmp/"))
                  (tmp-path (if (file-name-absolute-p url)
                                (concat email-dir
                                        (file-name-nondirectory url))
                              (expand-file-name
                               url (concat (file-name-directory current-file) "email_tmp/"))
                                ))
                  (ext (file-name-extension path))
                  (id (replace-regexp-in-string "[\/\\\\]" "_" path))
                  (tmp-id (replace-regexp-in-string "[\/\\\\]" "_" tmp-path)))
             ;; (message (concat "url:" url))
             ;; (message (and (string-match "src=\"\\(file://\\)?\\([^\"]+\\)\"" text) (match-string 2 text)))
             ;; (message (concat "path:" path))
             ;; (message (concat "url:" url))
             ;; (message (concat "path:" path))
             ;; (message (concat "tmp-path:" tmp-path))
             ;; (message (concat "tmp-id:" tmp-id))
             (if (not (file-directory-p email-dir))
                 (mkdir email-dir)
                 )
             (if (not (file-directory-p (file-name-directory tmp-path)))
                 (mkdir (file-name-directory tmp-path))
               )
             (if (file-exists-p  url)
                 (copy-file url tmp-path 'replace)
               )
             (if (file-exists-p  path)
                 (copy-file path tmp-path 'replace)
               )
             (add-to-list 'html-images
                          (org~mu4e-mime-file
                           (concat "image/" ext) tmp-path tmp-id))
             tmp-id)))
        str)
       html-images)))

  (defun org~mu4e-mime-convert-to-html ()
    "Convert the current body to html."
    (unless (fboundp 'org-export-string-as)
      (mu4e-error "require function 'org-export-string-as not found."))
    ;; (setq temporary-file-directory "~/")
    (let* ((begin
            (save-excursion
              (goto-char (point-min))
              (search-forward mail-header-separator)))
           (end (point-max))
           (raw-body (buffer-substring begin end))

           (tmp-file (make-temp-name (expand-file-name "mail"
                                                       temporary-file-directory)))
           ;; (tmp-file (make-temp-name "~/"))
           (org-export-skip-text-before-1st-heading nil)
           (org-export-htmlize-output-type 'inline-css)
           (org-export-with-toc nil)
           (org-export-with-latex 'imagemagick)
           ;; (org-export-preserve-breaks t)
           (org-export-preserve-breaks nil)

           ;; (org-export-with-LaTeX-fragments
           ;;  (if (executable-find "dvipng") 'dvipng
           ;;    (mu4e-message "Cannot find dvipng, ignore inline LaTeX") nil))

           (org-export-with-LaTeX-fragments
            (if (executable-find "imagemagick") 'imagemagick
              (mu4e-message "Cannot find dvipng, ignore inline LaTeX") nil))
           (html-and-images
            (org~mu4e-mime-replace-images
             (concat (org-export-string-as raw-body 'html 't) (file-string "~/emailsign/emailsign.html"))
             ;; (org-export-string-as raw-body 'html 't)
             ;; ""
             tmp-file
             ))
           (html-images (cdr html-and-images))
           (html (car html-and-images)))
      (delete-region begin end)

        (goto-char begin)
        (newline)
        (insert (org~mu4e-mime-multipart
                 raw-body html (mapconcat 'identity html-images "\n"))))))

(defun zilongshanren/mu4e-org-compose ()
  "Switch to/from mu4e-compose-mode and org-mode"
   (interactive)
   (let ((p (point)))
     (goto-char (point-min))
     (let ((case-fold-search t))
       (when (not (search-forward "#+OPTIONS: tex:imagemagick" nil t))
         (goto-char (point-max))
         (insert "\n#+OPTIONS: tex:imagemagick\n#+OPTIONS: toc:0\n")))
     (goto-char p))
   (if (eq 'mu4e-compose-mode (buffer-local-value 'major-mode (current-buffer)))
       (org~mu4e-mime-switch-headers-or-body)
     (mu4e-compose-mode)))
