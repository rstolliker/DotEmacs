;; MELPA
(require 'package) ;; You might already have this line
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
  (add-to-list 'package-archives (cons "melpa" url) t))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) ;; You might already have this line
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(package-selected-packages
   (quote
    (pandoc-mode markdown-mode alchemist auto-package-update use-package ## magit rainbow-delimiters neotree company-irony irony intero company-anaconda anaconda-mode cider slime-company slime company))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; Added by me

;; auto install list
(setq my-packages '(
		    use-package
		    ))

; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

;; install the missing packages
(dolist (package my-packages)
  (unless (package-installed-p package)
    (package-install package)))

					; include use-package
(eval-when-compile
  (require 'use-package))

(setq use-package-always-ensure t)

(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (auto-package-update-maybe))

(use-package company
  :init (global-company-mode)
  :config
  (setq company-idle-delay .1))

(use-package slime
  :defer t
  :config
  (add-to-list 'slime-contribs 'slime-fancy))

(use-package slime-company
  :after (slime company)
  :defer t
  :init 
  (slime-setup '(slime-fancy slime-company)))

(use-package cider
  :defer t
  :hook((clojure-mode . cider-mode)
	(cider-mode . eldoc-mode))
  :config
  (setq cider-cljs-lein-repl "(do (use 'figwheel-sidecar.repl-api) (start-figwheel!) (cljs-repl))"))

(use-package anaconda-mode
  :defer t
  :hook python-mode)

(use-package company-anaconda
  :after company
  :defer t
  :init
  (add-to-list 'company-backends 'company-anaconda))

(use-package intero
  :defer t
  :hook (haskell-mode . intero-mode))

(use-package irony
  :defer t
  :hook (((c++-mode c-mode objc-mode) . irony-mode)
	 (irony-mode . irony-cdb-autosetup-compile-options))
  :init
  (add-to-list 'company-backends 'company-irony))

(use-package neotree
  :defer t
  :bind ([f8] . neotree-toggle))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package magit
  :defer t)

(use-package atom-one-dark-theme
  :init
  (load-theme 'atom-one-dark t))

(use-package alchemist
  :defer t
  :hook (elixir-mode . alchemist-mode))

(use-package org
  :defer t)

(use-package clojure-mode
  :defer t)

(use-package markdown-mode
  :defer t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "pandoc"))

(use-package pandoc-mode
  :defer t
  :hook ((markdown-mode . pandoc-mode)
	 (pandoc-mode . pandoc-load-default-settings)))






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Non-package configuration



;; Set auto pair
(electric-pair-mode 1)

;; Show Matching parens
(show-paren-mode 1)

;; auto reloads on change
(global-auto-revert-mode t)

;; Auto Save and Backup Directory
(setq backup-directory-alist
	`(("." . ,(concat user-emacs-directory "backups"))))

;; Make font size 14
(set-face-attribute 'default nil :height 140)

;; Jump to new line with Ctrl-Ret
(defun end-of-line-and-indented-new-line ()
  (interactive)
  (end-of-line)
  (newline-and-indent))
(global-set-key (kbd "<C-return>") 'end-of-line-and-indented-new-line)

;; Split windows horizontally
(setq split-height-threshold nil)
(setq split-width-threshold 0)

