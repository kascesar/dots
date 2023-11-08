;;; windows navigation configuration:

(global-set-key [M-left] 'windmove-left)     ;;; move to left window
(global-set-key [M-right] 'windmove-right)   ;;; move to right window
(global-set-key [M-up] 'windmove-up)         ;;; move to upper window
(global-set-key [M-down] 'windmove-down)     ;;; move to lower window

;; hide hiden files in dired
(require 'dired-x)
(setq dired-omit-files "^\\...+$")
(add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1)))

;; ===================================
;;       MELPA Package Support
;; ===================================
;; Enables basic packaging support
(require 'package)

;; Adds the Melpa archive to the list of available repositories
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)


;; Initializes the package infrastructure
(package-initialize)
;; If there are no archived package contents, refresh them
(when (not package-archive-contents)
  (package-refresh-contents))

;; package instalations
(defvar myPackages
  '(jedi
    highlight-indent-guides
    )
  )

;; Scans the list in myPackages
(mapc #'(lambda (package)
          (unless (package-installed-p package)
            (package-install package)))
      myPackages)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(custom-enabled-themes (quote (wheatgrass)))
 '(highlight-indent-guides-auto-character-face-perc 50)
 '(highlight-indent-guides-method (quote character))
 '(package-selected-packages
   (quote
    (flycheck use-package jedi highlight-indent-guides elpy company-jedi))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; === add color  to tex larger than 80 character ===
(setq-default
 whitespace-line-column 80
 whitespace-style       '(face lines-tail))
(add-hook 'prog-mode-hook #'whitespace-mode)

;; === Add row, column numbers to a boton ===
(column-number-mode)

;; === Enabeling line numbers in the left ===
(add-hook 'prog-mode-hook 
  (lambda ()
    (linum-mode 1)))
;; add some spate to the number column
(setq linum-format "%d ")

;; === enable indenation guide ===
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)


;; ------------ python --------------------------------
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;; === flychek ===
; install pylint before!!!!
; >>> pip install pylint

(use-package flycheck
:ensure t
:init
(global-flycheck-mode t))

;; === jedi ===
; install  epc
; >>> pip install epc


;; Standard package.el + MELPA setup
;; (See also: https://github.com/milkypostman/melpa#usage)
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)


;; Standard Jedi.el setting
;; Type:
;;     M-x package-install RET jedi RET                                
;;     M-x jedi:install-server RET                                     
;; Then open Python file.

(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)
;(jedi:install-server)

;; Type:
;;     M-x package-install RET jedi RET
;;     M-x jedi:install-server RET
;; Then open Python file.

(use-package jedi
:ensure t
:init
(add-hook 'python-mode-hook 'jedi:setup)
(add-hook 'python-mode-hook 'jedi:ac-setup))

;; Auto-refresh dired on file change                               
(add-hook 'dired-mode-hook 'auto-revert-mode)