;;; init.el --- Spacemacs Initialization File
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Alexandros Kourtis <<akourtisdev@gmail.com>>
;; URL: https://github.com/akourtis98/spacemacs-config
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Without this comment emacs25 adds (package-initialize) here
(package-initialize)

;; Increase gc-cons-threshold, depending on your system you may set it back to a
;; lower value in your dotfile (function `dotspacemacs/user-config')


;; packages
;; load emacs 24's package system. Add MELPA repository.
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   ;; '("melpa" . "http://stable.melpa.org/packages/") ; many packages won't show if using stable
   '("melpa" . "http://melpa.milkbox.net/packages/")
   t))

(custom-set-variables
 '(eclim-eclipse-dirs '("/usr/bin/eclipse"))
 '(eclim-executable "/usr/bin/eclipse/eclim"))


(setq gc-cons-threshold 100000000)

(defconst spacemacs-version         "0.200.13" "Spacemacs version.")
(defconst spacemacs-emacs-min-version   "24.4" "Minimal version of Emacs.")

(if (not (version<= spacemacs-emacs-min-version emacs-version))
    (error (concat "Your version of Emacs (%s) is too old. "
                   "Spacemacs requires Emacs version %s or above.")
           emacs-version spacemacs-emacs-min-version)
  (load-file (concat (file-name-directory load-file-name)
                     "core/core-load-paths.el"))
  (require 'core-spacemacs)
  (spacemacs/init)
  (configuration-layer/sync)
  (spacemacs-buffer/display-startup-note)
  (spacemacs/setup-startup-hook)
  (require 'server)
  (unless (server-running-p) (server-start)))


(global-set-key (kbd "M-w") 'other-window)

(defun markdown-html (buffer)
  (princ (with-current-buffer buffer
           (format "<!DOCTYPE html><html><title>Impatient Markdown</title><xmp theme=\"united\" style=\"display:none;\"> %s  </xmp><script src=\"http://strapdownjs.com/v/0.2/strapdown.js\"></script></html>" (buffer-substring-no-properties (point-min) (point-max))))
         (current-buffer)))

(load-theme 'tango-dark)



					; jsp stuff
					;
					; functions to perform correct indentation of java code within a 
					; JSP page. I can live without syntax coloring - I can't live 
					; without proper indentation
					;
(setq auto-mode-alist
      (cons '("\\.jsp\\'" . html-mode) auto-mode-alist))

(defun jsp-java-sob ()
  "Return the point for the enclosing <% or nil if not present"
  (save-excursion 
    (search-backward "<%")))

(defun jsp-java-eob ()
  "Return the point for the enclosing %> or nil if not present"
  (save-excursion 
    (search-forward "%>")))

(defun jsp-in-java-debug ()
  (interactive)
  (if (jsp-in-java)
      (message "debug: in java code")
    (message "debug: not in java code")))

(defun jsp-in-java ()
  (save-excursion
    (let* ((current-point (point))
           (start-java-tag (jsp-java-sob))
           (end-java-tag (jsp-java-eob)))
      (and (> current-point start-java-tag) 
	   (< current-point end-java-tag)))))


(defun jsp-indent-java ()
  (interactive)
  "Indent a java block within a JSP page "
  (save-excursion 
    (let ((start (+ 2 (jsp-java-sob)))
          (end (jsp-java-eob) ))
      (if (string= "!" (char-to-string (char-after start)))
          (setq start (+ start 1))
        nil)
      (if (jsp-in-java)
          (progn 
            (java-mode) 
            (goto-char start)
            ;; needed for proper indentation - removed later
            (newline)    
            (insert "{")
            (newline)

            (goto-char end)
            ;; needed for proper indentation - removed later
            (newline)
            (insert "}") 
            (newline)
            ;; 
            ;; perform the indentation
            ;; 
            (indent-region (+ start 2) (- end 0) nil)
            ;;
            ;; now remove the text we termporarily added for indentation
            ;;
            (delete-region (- (point) 3) (point)) 
            (delete-region start (+ start 3))
            (html-mode))
        ()))))

(setq html-mode-hook
      '(lambda ()
         (define-key sgml-mode-map "\C-c\C-q"  'jsp-indent-java)
         ))

(spacemacs|defvar-company-backends java-mode)

(spacemacs|define-jump-handlers java-mode)

(defun spacemacs/java-completing-dot ()
  "Insert a period and show company completions."
  (interactive "*")
  (spacemacs//java-delete-horizontal-space)
  (insert ".")
  (company-emacs-eclim 'interactive))

(defun spacemacs/java-completing-double-colon ()
  "Insert double colon and show company completions."
  (interactive "*")
  (spacemacs//java-delete-horizontal-space)
  (insert ":")
  (let ((curr (point)))
    (when (s-matches? (buffer-substring (- curr 2) (- curr 1)) ":")
      (company-emacs-eclim 'interactive))))

(defun spacemacs//java-delete-horizontal-space ()
  (when (s-matches? (rx (+ (not space)))
                    (buffer-substring (line-beginning-position) (point)))
    (delete-horizontal-space t)))

(defun spacemacs/java-maven-test ()
  (interactive)
  (eclim-maven-run "test"))

(defun spacemacs/java-maven-clean-install ()
  (interactive)
  (eclim-maven-run "clean install"))

(defun spacemacs/java-maven-install ()
  (interactive)
  (eclim-maven-run "install"))

(setq java-packages
      '(
        company
        (company-emacs-eclim :toggle (configuration-layer/package-usedp 'company))
        eclim
        ggtags
        helm-gtags
        (java-mode :location built-in)
        ))

(defun java/post-init-company ()
  (spacemacs|add-company-hook java-mode))

(defun java/init-company-emacs-eclim ()
  (use-package company-emacs-eclim
    :defer t
    :init (push 'company-emacs-eclim company-backends-java-mode)))

(defun java/init-eclim ()
  (use-package eclim
    :defer t
    :init
    (progn
      (add-hook 'java-mode-hook 'eclim-mode)
      (add-to-list 'spacemacs-jump-handlers-java-mode 'eclim-java-find-declaration))
    :config
    (progn
      (spacemacs|hide-lighter eclim-mode)
      (require 'eclimd)
      (setq help-at-pt-display-when-idle t
            help-at-pt-timer-delay 0.1)
      (help-at-pt-set-timer)

      (add-to-list 'minor-mode-alist
                   '(eclim-mode (:eval (eclim-modeline-string))))

      (evil-define-key 'insert java-mode-map
		       (kbd ".") 'spacemacs/java-completing-dot
		       (kbd ":") 'spacemacs/java-completing-double-colon
		       (kbd "M-.") 'eclim-java-find-declaration
		       (kbd "M-,") 'pop-tag-mark
		       (kbd "M-<mouse-3>") 'eclim-java-find-declaration
		       (kbd "<mouse-8>") 'pop-tag-mark)

      (evil-define-key 'normal java-mode-map
		       (kbd "M-.") 'eclim-java-find-declaration
		       (kbd "M-,") 'pop-tag-mark
		       (kbd "M-<mouse-3>") 'eclim-java-find-declaration
		       (kbd "<mouse-8>") 'pop-tag-mark)

      (evil-define-key 'normal eclim-problems-mode-map
		       (kbd "a") 'eclim-problems-show-all
		       (kbd "e") 'eclim-problems-show-errors
		       (kbd "g") 'eclim-problems-buffer-refresh
		       (kbd "q") 'eclim-quit-window
		       (kbd "w") 'eclim-problems-show-warnings
		       (kbd "f") 'eclim-problems-toggle-filefilter
		       (kbd "c") 'eclim-problems-correct
		       (kbd "RET") 'eclim-problems-open-current)

      (evil-define-key 'normal eclim-project-mode-map
		       (kbd "N") 'eclim-project-create
		       (kbd "m") 'eclim-project-mark-current
		       (kbd "M") 'eclim-project-mark-all
		       (kbd "u") 'eclim-project-unmark-current
		       (kbd "U") 'eclim-project-unmark-all
		       (kbd "o") 'eclim-project-open
		       (kbd "c") 'eclim-project-close
		       (kbd "i") 'eclim-project-info-mode
		       (kbd "I") 'eclim-project-import
		       (kbd "RET") 'eclim-project-goto
		       (kbd "D") 'eclim-project-delete
		       (kbd "p") 'eclim-project-update
		       (kbd "g") 'eclim-project-mode-refresh
		       (kbd "R") 'eclim-project-rename
		       (kbd "q") 'eclim-quit-window)

      (spacemacs/set-leader-keys-for-major-mode 'java-mode
						"ea" 'eclim-problems-show-all
						"eb" 'eclim-problems
						"ec" 'eclim-problems-correct
						"ee" 'eclim-problems-show-errors
						"ef" 'eclim-problems-toggle-filefilter
						"en" 'eclim-problems-next-same-window
						"eo" 'eclim-problems-open
						"ep" 'eclim-problems-previous-same-window
						"ew" 'eclim-problems-show-warnings

						"ds" 'start-eclimd
						"dk" 'stop-eclimd

						"ff" 'eclim-java-find-generic

						"gt" 'eclim-java-find-type

						"rc" 'eclim-java-constructor
						"rg" 'eclim-java-generate-getter-and-setter
						"rf" 'eclim-java-format
						"ri" 'eclim-java-import-organize
						"rj" 'eclim-java-implement
						"rr" 'eclim-java-refactor-rename-symbol-at-point

						"hc" 'eclim-java-call-hierarchy
						"hh" 'eclim-java-show-documentation-for-current-element
						"hi" 'eclim-java-hierarchy
						"hu" 'eclim-java-find-references

						"mi" 'spacemacs/java-maven-clean-install
						"mI" 'spacemacs/java-maven-install
						"mp" 'eclim-maven-lifecycle-phases
						"mr" 'eclim-maven-run
						"mR" 'eclim-maven-lifecycle-phase-run
						"mt" 'spacemacs/java-maven-test

						"aa" 'eclim-ant-run
						"ac" 'eclim-ant-clear-cache
						"ar" 'eclim-ant-run
						"av" 'eclim-ant-validate

						"pb" 'eclim-project-build
						"pc" 'eclim-project-create
						"pd" 'eclim-project-delete
						"pg" 'eclim-project-goto
						"pi" 'eclim-project-import
						"pj" 'eclim-project-info-mode
						"pk" 'eclim-project-close
						"po" 'eclim-project-open
						"pp" 'eclim-project-mode
						"pu" 'eclim-project-update

						"tt" 'eclim-run-junit))))

(defun java/post-init-ggtags ()
  (add-hook 'java-mode-local-vars-hook #'spacemacs/ggtags-mode-enable))

(defun java/post-init-helm-gtags ()
  (spacemacs/helm-gtags-define-keys-for-mode 'java-mode))

(defun java/init-java-mode ()
  (setq java/key-binding-prefixes '(("me" . "errors")
                                    ("md" . "eclimd")
                                    ("mf" . "find")
                                    ("mg" . "goto")
                                    ("mr" . "refactor")
                                    ("mh" . "documentation")
                                    ("mm" . "maven")
                                    ("ma" . "ant")
                                    ("mp" . "project")
                                    ("mt" . "test")))
  (mapc (lambda(x) (spacemacs/declare-prefix-for-mode
                    'java-mode (car x) (cdr x)))
        java/key-binding-prefixes))

;; company
;;
(use-package company
  :ensure t
  :config
  (global-company-mode t)
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 3)
  (setq company-backends
        '((company-files
           company-keywords
           company-capf
           company-yasnippet
           )
          (company-abbrev company-dabbrev))))

(add-hook 'emacs-lisp-mode-hook (lambda () (set (make-local-variable 'company-backends) '(company-elisp))))


;;
;; change company complete common
;;
(advice-add 'company-complete-common :before (lambda () (setq my-company-point (point))))
(advice-add 'company-complete-common :after (lambda () (when (equal my-company-point (point))
                                                         (yas-expand))))


;;
;; projectile
;;
(use-package projectile
  :ensure t
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (projectile-mode t)
  (setq projectile-completion-system 'ivy)
  (use-package counsel-projectile
    :ensure t)
  )


;;
;; auto insert
;;
(use-package autoinsert
  :ensure t
  :config
  (setq auto-insert-query nil)
  (setq auto-insert-directory (locate-user-emacs-file "template"))
  (add-hook 'find-file-hook 'auto-insert)
  (auto-insert-mode t)
  )


(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(setq web-mode-engines-alist
      '(("php"    . "\\.phtml\\'")
        ("blade"  . "\\.blade\\."))
      )

(add-to-list 'auto-mode-alist '("\\.api\\'" . web-mode))
(add-to-list 'auto-mode-alist '("/some/react/path/.*\\.js[x]?\\'" . web-mode))

(setq web-mode-content-types-alist
  '(("json" . "/some/path/.*\\.api\\'")
    ("xml"  . "/other/path/.*\\.api\\'")
    ("jsx"  . "/some/react/path/.*\\.js[x]?\\'")))

(defun my-web-mode-hook ()
"Hooks for Web mode."
(setq web-mode-markup-indent-offset 2)
)
(add-hook 'web-mode-hook  'my-web-mode-hook)

(setq web-mode-comment-style 2)

(setq web-mode-extra-snippets
      '(("erb" . (("toto" . "<% toto | %>\n\n<% end %>")))
        ("php" . (("dowhile" . "<?php do { ?>\n\n<?php } while (|); ?>")
                  ("debug" . "<?php error_log(__LINE__); ?>")))
        ))

(setq web-mode-extra-auto-pairs
      '(("erb"  . (("beg" "end")))
        ("php"  . (("beg" "end")
                   ("beg" "end")))
        ))

(setq web-mode-enable-auto-pairing t)

(setq web-mode-enable-css-colorization t)

(setq web-mode-enable-current-element-highlight t)
(setq web-mode-enable-current-column-highlight t)

(setq web-mode-ac-sources-alist
      '(("css" . (ac-source-css-property))
        ("html" . (ac-source-words-in-buffer ac-source-abbrev))))

(setq web-mode-ac-sources-alist
      '(("php" . (ac-source-yasnippet ac-source-php-auto-yasnippets))
        ("html" . (ac-source-emmet-html-aliases ac-source-emmet-html-snippets))
        ("css" . (ac-source-css-property ac-source-emmet-css-snippets))))

(add-hook 'web-mode-before-auto-complete-hooks
          '(lambda ()
             (let ((web-mode-cur-language
                    (web-mode-language-at-pos)))
               (if (string= web-mode-cur-language "php")
                   (yas-activate-extra-mode 'php-mode)
                 (yas-deactivate-extra-mode 'php-mode))
               (if (string= web-mode-cur-language "css")
                   (setq emmet-use-css-transform t)
                 (setq emmet-use-css-transform nil)))))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))


(require 'flymd)

 (defun my-flymd-browser-function (url)
   (let ((browse-url-browser-function 'browse-url-firefox))
     (browse-url url)))
(setq flymd-browser-open-function 'my-flymd-browser-function)

(defun my-flymd-browser-function (url)
  (let ((process-environment (browse-url-process-environment)))
    (apply 'start-process
           (concat "firefox " url) nil
           "firefox"
           (list "--new-window" "--allow-file-access-from-files" url))))
(setq flymd-browser-open-function 'my-flymd-browser-function)

(provide 'init)
;;; init.el ends here
