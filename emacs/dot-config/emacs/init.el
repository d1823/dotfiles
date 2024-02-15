(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(add-to-list 'custom-theme-load-path
             (file-name-as-directory
              (expand-file-name "~/.config/emacs/themes")))

(set-frame-font "Cascadia Mono 12" nil t)

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
(use-package solarized-theme
  :ensure t
  :demand t
  :init
  (require 'dbus)
  (require 'cl-lib)

  (defun get-color-scheme-preference-from-dbus ()
    (let ((service "org.freedesktop.portal.Desktop")
	  (path "/org/freedesktop/portal/desktop")
	  (interface "org.freedesktop.portal.Settings")
	  (method "Read")
	  (arg-1 "org.freedesktop.appearance")
	  (arg-2 "color-scheme"))
      (car (car (dbus-call-method :session service path interface method arg-1 arg-2)))))

  (defun color-scheme-preference-to-theme (color-scheme-preference)
    (cl-case color-scheme-preference
      (1 'solarized-dark)
      (2 'solarized-light)
      (0 'solarized-dark)
      (t (error "Unknown color scheme preference: %s" color-scheme-preference))))

  (defun appearance-color-scheme-change-handler (path setting value)
    (when (and (string-equal "org.freedesktop.appearance" path) (string-equal "color-scheme" setting))
      (load-theme (color-scheme-preference-to-theme (car value)) t nil)))

  (let ((bus "org.freedesktop.impl.portal")
	(path "/org/freedesktop/portal/desktop")
	(interface "org.freedesktop.impl.portal.Settings")
	(dbus-signal "SettingChanged"))
    (dbus-register-signal :session
			  bus
			  path
			  interface
			  dbus-signal
			  #'appearance-color-scheme-change-handler))

  (message "D-Bus signal handler registered. Waiting for signals...")

  :config
  (load-theme (color-scheme-preference-to-theme (get-color-scheme-preference-from-dbus)) t nil))

(use-package magit
  :ensure t
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))

(use-package ivy
  :ensure t
  :config
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (ivy-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("7f7052c1376b58baaaca41b680b068716eeacb8aaadbe58e1f87d7b5f0cdb64b" "a596677067a637e7ec53ac1fab875a5ade7c1ac0b30ff9eee27fd8c2e9902045" default))
 '(package-selected-packages '(solarized-theme ivy magit)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
