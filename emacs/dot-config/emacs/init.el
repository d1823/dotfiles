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

(setq inhibit-startup-message t)

(defun gnome-dark-mode-enabled-p ()
  "Check if frame is dark or not."
  (if (executable-find "gsettings")
      (string-equal (string-trim (thread-last "gsettings get org.gnome.desktop.interface color-scheme" shell-command-to-string) "[ \t\n\r']+" "[ \t\n\r']+") "prefer-dark")))

(defun load-auto-detected-theme ()
  (if (gnome-dark-mode-enabled-p)
				 (load-theme 'solarized-dark t)
			       (load-theme 'solarized-light t)))

(use-package solarized-theme
  :ensure t
  :config
  (run-with-timer 0 3 'load-auto-detected-theme))

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
