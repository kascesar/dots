;;; package --- summary
;;; Commentary:
;;; Code:
;;; Add MELPA and GNU ELPA repositories for package installation
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
(package-initialize)

;;(set-face-attribute 'default nil :height 150)

;; Install use-package if not already installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Configure use-package
(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)

;; LIST of packages to be installed
(defvar needed-packages
  '(lsp-pyright
    lsp-ui company
    flycheck pyvenv
    python-black
    py-isort
    magit-gitflow
    autorevert
    recentf
    pulsar
    org-superstar
    tramp
    counsel-tramp
    highlight-indent-guides))

;; Install packages if not already installed
(dolist (package needed-packages)
  (unless (package-installed-p package)
    (package-install package)))
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

;; recarga los archivos al ser modificados
(use-package autorevert
  :ensure nil
  :diminish
  :hook (after-init . global-auto-revert-mode))

;; archivos visitados recientemente
(use-package recentf
  :defer 2
  :bind ("C-c r" . recentf-open-files)
  :init (recentf-mode)
  :custom
  (recentf-max-menu-items 10)
  (recentf-max-saved-items 50)
  (recentf-exclude (list "COMMIT_EDITMSG"
                         "~$"
                         "/scp:"
                         "/ssh:"
                         "/sudo:"
                         "diario.*"
                         "recentf*"
                         "bookmark*"
                         "/archivo*"
                         "birthday*"
                         "*elpa/*"
                         "/tmp/"
                         "drafts/*"
                         "/.elfeed"
                         "/.telega"
                         "/.config"
                         "~/.emacs.d/s*"))
    :config (run-at-time nil (* 5 60) 'recentf-save-list))

;; Pulsar
(require 'pulsar)

;; Check the default value of `pulsar-pulse-functions'.  That is where
;; you add more commands that should cause a pulse after they are
;; invoked

(setq pulsar-pulse t)
(setq pulsar-delay 0.07)
(setq pulsar-iterations 10)
(setq pulsar-face 'pulsar-magenta)
(setq pulsar-highlight-face 'pulsar-cyan)

(pulsar-global-mode 1)

;; OR use the local mode for select mode hooks

(dolist (hook '(org-mode-hook
		emacs-lisp-mode-hook))
  (add-hook hook #'pulsar-mode))

;; pulsar does not define any key bindings.  This is just a sample that
;; respects the key binding conventions.  Evaluate:
;;
;;     (info "(elisp) Key Binding Conventions")
;;
;; The author uses C-x l for `pulsar-pulse-line' and C-x L for
;; `pulsar-highlight-line'.
;;
;; You can replace `pulsar-highlight-line' with the command
;; `pulsar-highlight-dwim'.
;;(let ((map global-map))
;; (define-key map (kbd "C-c h p") #'pulsar-pulse-line)
;; (define-key map (kbd "C-c h h") #'pulsar-highlight-line))

;; pulsar en el minibuffer
(add-hook 'minibuffer-setup-hook #'pulsar-pulse-line)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Additional configurations
;; Move between windows using Meta key
(global-set-key [M-left] 'windmove-left)
(global-set-key [M-right] 'windmove-right)
(global-set-key [M-up] 'windmove-up)
(global-set-key [M-down] 'windmove-down)

;; Zoom in/out with mouse
(global-set-key [C-mouse-4] 'text-scale-increase)
(global-set-key [C-mouse-5] 'text-scale-decrease)

;; Incrementa el tamaño de la fuente
(global-set-key (kbd "C-+") 'text-scale-increase)
;; Disminuye el tamaño de la fuente
(global-set-key (kbd "C--") 'text-scale-decrease)

;; Hide hidden files in dired
(require 'dired-x)
(setq dired-omit-files "^\\...+$")
(add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1)))

;; Add color to lines longer than 80 characters in programming modes
(defun my-enable-whitespace-mode ()
  "Enable whitespace-mode with custom settings."
  (setq-local whitespace-line-column 80)
  (setq-local whitespace-style '(face lines-tail))
  (whitespace-mode 1))

;; Activar whitespace-mode en todos los modos de programación
(add-hook 'prog-mode-hook #'my-enable-whitespace-mode)

;; Activar whitespace-mode en markdown-mode
(add-hook 'markdown-mode-hook #'my-enable-whitespace-mode)
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

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(highlight-indent-guides-stack-character-face ((t (:foreground "white"))))
 '(highlight-indent-guides-top-character-face ((t (:foreground "white"))))
 '(italic ((t (:slant italic)))))

;; dont ask when kill buffer
(global-set-key [remap kill-buffer] #'kill-this-buffer)
  (kill-buffer "*scratch*")


;; ======================= Org mode ======================

(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

;; Activar lenguajes Babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((shell . t)
   (python . t)))
(setq org-babel-python-command "python3")

(use-package org
  ;:ensure org-contrib
  :config
  (setq org-ellipsis " ⤵")

  ;(setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

  (setq org-todo-keywords
        '((sequence "PORHACER(p!)"
                    "ENPROCESO(e!)"
                    "BLOQUEADO(b!)"
                    "|"
		    "HECHO(h!)"
		    "ARCHIVAR(a!)")))

   (setq org-todo-keyword-faces
         '(("PORHACER" . "red")
           ("ENPROCESO" . "magenta")
           ("BLOQUEADO" . "orange")
           ("HECHO" . "green")))

(setq org-file-apps
      '(("\\.html\\'" . browse-url-firefox)))

  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  ;; etiquetas que utilizo para mis notas
  (setq org-tag-alist '(("@nota" . ?n)
                        ("@casa" . ?c)
                        ("@finanzas" . ?d)
                        ("@fecha" . ?f)
                        ("@salud" . ?s)
                        ("@tarea" . ?t)
                        ("@coche" . ?h)
                        ("@trabajo" . ?b)
                        ("crypt" . ?C)))
  (setq org-tags-exclude-from-inheritance '("crypt"))

  ;; Progress Logging
  ;; When a TODO item enters DONE, add a CLOSED: property with current date-time stamp and into drawer
  (setq org-log-done 'time)
  (setq org-log-into-drawer "LOGBOOK")
  ;;
  ;; Alinea etiquetas
  (setq org-tags-column 70))

;; Aspecto mejorado al identar
(add-hook 'org-mode-hook 'org-indent-mode)


;; Finalmente haremos que cuando se visualice un fichero con extensión .org éste se adapte a la ventana y cuando la línea llegue al final de esta, haga un salto de carro.
(add-hook 'org-mode-hook 'visual-line-mode)
(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(require 'org-superstar)
(add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))


(use-package visual-fill-column
  :hook (org-mode . visual-fill-column-mode)
  :custom
  (visual-fill-column-center-text t)
  (visual-fill-column-width 80)
  (visual-fill-column-mode 1))

;;;; se necesita instalar grip (sudo apt install grip)
;; Configuración para activar auto-fill-mode y establecer fill-column en archivos Markdown
;; GIT
(use-package git-gutter
  :defer 0.3
  :delight
  :init (global-git-gutter-mode +1))

(use-package git-timemachine
  :defer 1
  :delight)


;; PARENTESIS

;; Mostrar al padre
(show-paren-mode 1)

;; auto close bracket insertion.
(electric-pair-mode 1)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))


;;; THEMES
(setq ef-themes-mixed-fonts t
      ef-themes-variable-pitch-ui t)

;; Disable all other themes to avoid awkward blending:
;(mapc #'disable-theme custom-enabled-themes)

  (use-package ef-themes
    :config
    (load-theme 'ef-summer t))

(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-z") 'undo)

(setq user-full-name "Cesar M.")
;;(setq inhibit-startup-message f)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
;; (tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room
(menu-bar-mode -1)          ; Disable the menu bar


;; org-roam
;(use-package org-roam
;  :ensure t
;  :custom
;  (require org-roam-directory (file-truename "~/org-roam"))
;  :bind (("C-c n l" . org-roam-buffer-toggle)
;         ("C-c n f" . org-roam-node-find)
;         ("C-c n g" . org-roam-graph)
;         ("C-c n i" . org-roam-node-insert)
;         ("C-c n c" . org-roam-capture)
;         ;; Dailies
;         ("C-c n j" . org-roam-dailies-capture-today))
;  :config
;  (org-roam-setup))
(set-frame-font "DejaVu Sans Mono-13" nil t)

;; Configuración para usar una sola ventana con Ediff
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(highlight-indent-guides-auto-character-face-perc 50)
 '(highlight-indent-guides-auto-odd-face-perc 30)
 '(highlight-indent-guides-character "|")
 '(highlight-indent-guides-method 'bitmap)
 '(ispell-dictionary nil)
 '(package-selected-packages
   '(## mugur gnuplot-mode gnuplot visual-fill-column org-bullets calfw-org calfw org-contrib pulsar magit-gitflow py-isort use-package pyvenv python-black pylint magit lsp-ui lsp-python-ms lsp-pyright lsp-docker jedi-direx highlight-indent-guides grip-mode flycheck dired-sidebar company)))
