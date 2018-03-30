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
 '(custom-enabled-themes (quote (tango-dark)))
 '(inhibit-startup-screen t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; Added by me

;; auto install list
(setq my-packages '(
		    company
		    slime
		    slime-company
		    cider
		    anaconda-mode
		    company-anaconda
		    intero
		    irony
		    company-irony
		    ;;all-the-icons
		    neotree
		    rainbow-delimiters
		    ))

; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package my-packages)
  (unless (package-installed-p package)
    (package-install package)))


;; Set auto pair
(electric-pair-mode 1)

;; Show Matching parens
(show-paren-mode 1)

;; Company mode hook for all
(add-hook 'after-init-hook 'global-company-mode)
(setq company-idle-delay .1)

;; Slime contribs
(add-to-list 'slime-contribs 'slime-fancy)

;; slime setup for company
(slime-setup '(slime-fancy slime-company))

;; anaconda mode
(eval-after-load "company"
  '(add-to-list 'company-backends 'company-anaconda))

;; python anaconda
(add-hook 'python-mode-hook 'anaconda-mode)

;; set lisp interpreter
(setq inferior-lisp-program "c:/cygwin64/bin/clisp.exe")

;; intero
;;(add-hook 'haskell-mode-hook 'intero-mode)
(intero-global-mode 1)

;; irony
;; requires libclang and cmake
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))

;; Neotree options
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

;; cider figwheel repl config
;; for other solutions see https://github.com/clojure-emacs/cider/blob/master/doc/up_and_running.md
(setq cider-cljs-lein-repl "(do (use 'figwheel-sidecar.repl-api) (start-figwheel!) (cljs-repl))")

;; auto reloads on change
(global-auto-revert-mode t)

;; rainbow delimiters hooks
;;(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
;;(add-hook 'lisp-mode-hook #'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
