(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(add-to-list 'custom-theme-load-path
             (file-name-as-directory
              (expand-file-name "~/.config/emacs/themes")))

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

(set-frame-font "MapleMono 12" nil t)

(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

(setq column-number-mode t)
(setq inhibit-startup-message t)

;; Let's enable pixel-precision scrolling for mouse & keyboard.
(use-package pixel-scroll
  :init
  (setq pixel-scroll-precision-large-scroll-height 40.0)
  (setq pixel-scroll-precision-interpolate-page 1)
  :bind (("C-v" . pixel-scroll-interpolate-down)
	 ("M-v" . pixel-scroll-interpolate-up))
  :config
  (pixel-scroll-precision-mode))

(use-package tree-sitter
  :ensure t
  :requires (php-mode)
  :config
  (global-tree-sitter-mode))

(use-package tree-sitter-langs
  :ensure t)

(use-package php-mode
  :ensure t)

;; Ensure we're subscribing the FreeDesktop's color-scheme changes
;; and switch the solarized theme accordingly.
(use-package solarized-theme
  :ensure t
  :demand t
  :init
  (require 'dbus)
  (require 'cl-lib)

  (defun custom/get-color-scheme-preference-from-dbus ()
    (let ((service "org.freedesktop.portal.Desktop")
	  (path "/org/freedesktop/portal/desktop")
	  (interface "org.freedesktop.portal.Settings")
	  (method "Read")
	  (arg-1 "org.freedesktop.appearance")
	  (arg-2 "color-scheme"))
      (car (car (dbus-call-method :session service path interface method arg-1 arg-2)))))

  (defun custom/color-scheme-preference-to-theme (color-scheme-preference)
    (cl-case color-scheme-preference
      (1 'solarized-dark)
      (2 'solarized-light)
      (0 'solarized-light)
      (t (error "Unknown color scheme preference: %s" color-scheme-preference))))

  (defun custom/appearance-color-scheme-change-handler (path setting value)
    (when (and (string-equal "org.freedesktop.appearance" path) (string-equal "color-scheme" setting))
      (load-theme (custom/color-scheme-preference-to-theme (car value)) t nil)))

  (let ((bus "org.freedesktop.impl.portal")
	(path "/org/freedesktop/portal/desktop")
	(interface "org.freedesktop.impl.portal.Settings")
	(dbus-signal "SettingChanged"))
    (dbus-register-signal :session
			  bus
			  path
			  interface
			  dbus-signal
			  #'custom/appearance-color-scheme-change-handler))

  (message "D-Bus signal handler registered. Waiting for signals...")

  :config
  (load-theme (custom/color-scheme-preference-to-theme (custom/get-color-scheme-preference-from-dbus)) t nil))

(use-package magit
  :ensure t
  :init
  (defun custom/list-directory-names (path)
    "List all directory names at the given PATH."
    (mapcar #'file-name-nondirectory
            (cl-remove-if-not 'file-directory-p (directory-files path t "^[^.]+"))))

  (defun custom/magit-interactive-status ()
    "Prompt to pick a repository at the '~/Code' path and call `magit-status` with prefix arg."
    (interactive)
    (let* ((code-dir (expand-file-name "~/Code"))
           (repo-names (custom/list-directory-names code-dir))
           (selected-repo (ivy-read "Select repository: " repo-names))
           (repo-path (expand-file-name selected-repo code-dir)))
      (let ((current-prefix-arg t))
	(magit-status repo-path))))
  (defalias 'git 'custom/magit-interactive-status)
  :config
  ;; Ensure the magit's frame is displayed full screen.
  ;; At some point this was the default behavior, but later on
  ;; got replaced with a display-in-horizontal-split.
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  :bind ("C-S-g" . 'custom/magit-interactive-status))

(use-package ivy
  :ensure t
  :config
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (ivy-mode))
