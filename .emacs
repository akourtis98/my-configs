;;; package --- Summary
;;; Commentary:
;;; Emacs Startup File --- initialization for Emacs
;;; author: akourtis
;;; Code:

(setq package-enable-at-startup nil)

(package-initialize)

;; hide annoying pop up
(setq inhibit-startup-screen t)

;; hide extra stuff that take up space
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

;; turn off backups and autosaves and other temp files
(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files
(setq create-lockfiles nil)

;; bind ctrl and ` to move to next window
(global-set-key (kbd "C-`") 'other-window)

;; melpa
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)
      
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(custom-enabled-themes (quote (alect-black)))
 '(custom-safe-themes
   (quote
    ("ab04c00a7e48ad784b52f34aa6bfa1e80d0c3fcacc50e1189af3651013eb0d58" "04dd0236a367865e591927a3810f178e8d33c372ad5bfef48b5ce90d4b476481" "7356632cebc6a11a87bc5fcffaa49bae528026a78637acd03cae57c091afd9b9" "565aa482e486e2bdb9c3cf5bfb14d1a07c4a42cfc0dc9d6a14069e53b6435b56" "89f545ddc104836b27167696db89b371f23893d5b2f038d43383d877ee678d3d" default)))
 '(package-selected-packages
   (quote
    (multi-term klere-theme alect-themes creamsody-theme helm-company flycheck auto-complete)))
 '(pos-tip-background-color "#1A3734")
 '(pos-tip-foreground-color "#FFFFC8"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 98 :width normal :foundry "outline" :family "Courier New"))))
 '(font-lock-comment-face ((t (:foreground "old lace")))))


;; auto-complete
(ac-config-default)
(require 'auto-complete)

;; yasnippet
(add-to-list 'load-path
              "~/.emacs.d/plugins/yasnippet")
(require 'yasnippet)
(yas-global-mode 1)

(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"                 ;; personal snippets
        "/path/to/some/collection/"           ;; foo-mode and bar-mode snippet collection
        "/path/to/yasnippet/yasmate/snippets" ;; the yasmate collection
        ))

(yas-global-mode 1) ;; or M-x yas-reload-all if you've started YASnippet already.

;; flycheck
(require 'package)
(add-to-list 'package-archives
             '("MELPA Stable" . "http://stable.melpa.org/packages/") t)
(package-initialize)
(package-refresh-contents)

(package-install 'flycheck)

(global-flycheck-mode)


;; company mode
(add-hook 'after-init-hook 'global-company-mode)

(load-theme 'alect-black t)


;; multi-term
(require 'multi-term)

(setq multi-term-program "/bin/bash")

;; footer
(provide '.emacs)
;;; .emacs ends here
