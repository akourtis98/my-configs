
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
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
