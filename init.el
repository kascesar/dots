;; Add MELPA and GNU ELPA repositories for package installation
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
(package-initialize)

;; Install use-package if not already installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Configure use-package
(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)

;; Python development setup
(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred))))

(use-package lsp-ui
  :commands lsp-ui-mode)

(use-package company
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package pyvenv
  :ensure t
  :config
  (setq pyvenv-mode-line-indicator '(pyvenv " [venv:" pyvenv-virtual-env-name "] ")))

(use-package python-black
  :ensure t
  :hook (python-mode . python-black-on-save-mode))

(use-package py-isort
  :ensure t
  :hook (python-mode . py-isort-before-save))

;; Additional configurations
;; Move between windows using Meta key
(global-set-key [M-left] 'windmove-left)
(global-set-key [M-right] 'windmove-right)
(global-set-key [M-up] 'windmove-up)
(global-set-key [M-down] 'windmove-down)

;; Zoom in/out with mouse
(global-set-key [C-mouse-4] 'text-scale-increase)
(global-set-key [C-mouse-5] 'text-scale-decrease)

;; Hide hidden files in dired
(require 'dired-x)
(setq dired-omit-files "^\\...+$")
(add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1)))

;; Add color to lines longer than 80 characters in programming modes
(setq-default
 whitespace-line-column 80
 whitespace-style '(face lines-tail))
(add-hook 'prog-mode-hook #'whitespace-mode)

;; Add row and column numbers to the mode line
(column-number-mode)

;; Enable line numbers on the left
(add-hook 'prog-mode-hook
          (lambda ()
            (linum-mode 1)))
;; Add some space to the line number column
(setq linum-format "%d ")

;; Enable indentation guides
;; (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
(use-package highlight-indent-guides
  :ensure t
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-character "|"))

;;; add gitflow to magit
(require 'magit-gitflow)
(add-hook 'magit-mode-hook 'turn-on-magit-gitflow)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#2e3436" "#a40000" "#4e9a06" "#c4a000" "#204a87" "#5c3566" "#729fcf" "#eeeeec"])
 '(custom-enabled-themes '(tsdh-dark))
 '(highlight-indent-guides-auto-character-face-perc 50)
 '(highlight-indent-guides-auto-odd-face-perc 30)
 '(highlight-indent-guides-character "|")
 '(highlight-indent-guides-method 'bitmap)
 '(ispell-dictionary nil)
 '(package-selected-packages
   '(py-isort use-package pyvenv python-black pylint magit lsp-ui lsp-python-ms lsp-pyright lsp-docker jedi-direx highlight-indent-guides grip-mode flycheck dired-sidebar company)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(highlight-indent-guides-stack-character-face ((t (:foreground "white"))))
 '(highlight-indent-guides-top-character-face ((t (:foreground "white")))))
