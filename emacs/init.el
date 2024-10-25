;;; package --- summary
;;; Commentary:
;;; Code:
;;; Add MELPA and GNU ELPA repositories for package installation
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(setq-default
  truncate-lines t
  truncate-partial-width-windows nil
  auto-hscroll-mode 'current-line
)
;;;;;;;;; CODE FOLDING ;;;;;;;;;;;;;;;;;;;;
(add-hook 'prog-mode-hook 'hs-minor-mode)
(global-set-key (kbd "C-c h") 'hs-hide-block)
(global-set-key (kbd "C-c s") 'hs-show-block)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;; Python development setup ;;;;;;;;;;;;;;;;;;

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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;; EAF ;;;;;;;;;;;;;;;;;;;;;;;;
(use-package quelpa-use-package)
;; Don't forget to run M-x eaf-install-dependencies
(use-package eaf
  :demand t
  :quelpa (eaf :fetcher github
              :repo  "manateelazycat/emacs-application-framework"
              :files ("*"))
  :load-path "~/.emacs.d/site-lisp/emacs-application-framework" ; Set to "/usr/share/emacs/site-lisp/eaf" if installed from AUR
  :init
  (use-package epc      :defer t :ensure t)
  (use-package ctable   :defer t :ensure t)
  (use-package deferred :defer t :ensure t)
  (use-package s        :defer t :ensure t)
  (setq browse-url-browser-function 'eaf-open-browser))

(add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-application-framework/")
(require 'eaf)
(require 'eaf-git)
(require 'eaf-org-previewer)
(require 'eaf-system-monitor)
(require 'eaf-jupyter)
(require 'eaf-org-previewer)
(require 'eaf-pyqterminal)
(require 'eaf-mindmap)
(require 'eaf-markdown-previewer)
(require 'eaf-pdf-viewer)
(require 'eaf-terminal)
(require 'eaf-markmap)
(require 'eaf-browser)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;; BEACON ;;;;;;;;;;;;;;;;;;;;;
(blink-cursor-mode 0)
(setq-default cursor-type 'bar)

(use-package beacon
  :ensure t
  :config
  ;; Configurar el color del resplandor a un rozado muy fuerte
  (setq beacon-color "#FF69B4")

  ;; Reducir el tamaño del resplandor
  (setq beacon-size 20)

  ;; Variable para almacenar el tiempo desde el último movimiento del cursor
  (defvar my-last-cursor-move-time 0)

  ;; Función para mostrar el resplandor periódicamente cuando el cursor está quieto
  (defun my-beacon-periodic-blink ()
    "Muestra el resplandor si el cursor ha estado quieto por más de 0.8 segundos."
    (let ((idle-time (- (float-time) my-last-cursor-move-time)))
      (when (>= idle-time 1)  ;; Verificar si el cursor ha estado inactivo por 1 segundos
        (beacon-blink))))

  ;; Función para manejar el movimiento del cursor y el resplandor inmediato
  (defun my-beacon-on-cursor-move ()
    "Muestra el resplandor inmediatamente al mover el cursor y actualiza el tiempo."
    ;; Actualizar el tiempo del último movimiento del cursor
    (setq my-last-cursor-move-time (float-time))
    ;; Mostrar el resplandor inmediatamente
    (beacon-blink))

  ;; Ejecutar el resplandor periódico cada 0.8 segundos
  (run-with-timer 0 1 'my-beacon-periodic-blink)

  ;; Hook para mostrar el resplandor inmediatamente al mover el cursor
  (add-hook 'post-command-hook 'my-beacon-on-cursor-move)

  ;; Habilitar el modo beacon
  (beacon-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;; DASHBOARD ;;;;;;;;;;;;;;;;;;
(use-package all-the-icons)

(setq dashboard-icon-type 'all-the-icons) ;; use `all-the-icons' package
(use-package dashboard
  :custom
  (dashboard-startup-banner "~/Imágenes/deep-human.jpeg")
  (dashboard-banner-logo-title (format "Buen día %s" user-full-name))
  (dashboard-items '((recents . 4)
                     (bookmarks . 5)
                     (agenda . 2)))
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-set-heading-icons t
        dashboard-set-file-icons t
        dashboard-set-init-info t
        dashboard-set-navigator t)
  (setq dashboard-navigator-buttons
        `((
           (,(when (display-graphic-p)
               (all-the-icons-octicon "tools" :height 1.0 :v-adjust 0.0))
            "Configuración" "Abrir configuración de emacs (.el)"
            (lambda (&rest _) (find-file (expand-file-name  "~/.emacs.d/init.el"))))
           (,(when (display-graphic-p)
               (all-the-icons-octicon "calendar" :height 1.0 :v-adjust 0.0))
            "Agenda" "Agenda personal"
            (lambda (&rest _)
              (interactive)
              (if (get-buffer "*Org Agenda*")
                  (progn
                    (switch-to-buffer-other-window "*Org Agenda*")
                    (kill-buffer "*Org Agenda*")
                    (org-agenda-list))
                (split-window-right)
                (org-agenda-list))))
           ))))
;; F10 para ir al Dashboard
(global-set-key (kbd "<f10>") 'open-dashboard)

(defun open-dashboard ()
  "Open the *dashboard* buffer and jump to the first widget."
  (interactive)

  (delete-other-windows)
  (if (get-buffer dashboard-buffer-name)
      (kill-buffer dashboard-buffer-name))
  (dashboard-insert-startupify-lists)
  (switch-to-buffer dashboard-buffer-name))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;; MARKDOWN ;;;;;;;;;;;;;;
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "pandoc")
  :config
  (setq visual-line-column 80)
  (setq markdown-fontify-code-blocks-natively t)
  (setq markdown-enable-math t))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
			("@personal" . ?p)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ORG-MODERN ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; requeriment for org-modern
(use-package compat
  :quelpa (compat :fetcher github :repo "emacs-compat/compat"))
(use-package org-modern
  :ensure t)
;; Configuración de org-modern
(setq org-modern-star '("◉" "○" "✸" "✿")
      org-modern-list '((43 . "➤") (45 . "–") (42 . "•"))
      org-modern-block-fringe t
      org-modern-table t
      org-modern-checkbox '((?X . "✓") (?- . "✗") (?\s . "⬚")))

;; Activa org-modern en org-mode
(add-hook 'org-mode-hook #'org-modern-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;  ORG-AGENDA  ;;;;;;;;
(global-set-key (kbd "<f7>") 'org-agenda)

(use-package org-super-agenda
  :config
  (org-super-agenda-mode))

(setq org-agenda-skip-scheduled-if-done t
      org-agenda-skip-deadline-if-done t
      org-agenda-compact-blocks t
      org-agenda-window-setup 'current-window
      org-agenda-start-on-weekday 1
      org-deadline-warning-days 7
      org-agenda-time-grid '((daily today require-timed))
      org-agenda-custom-commands
      '(
        ("x" "Vista trabajo"
         ((agenda "" ((org-agenda-span 3)
                      (org-super-agenda-groups
                       '((:name "Hoy"
                                :discard (:tag "personal")
                                :time-grid t
                                :scheduled past
                                :deadline past
                                :date today
                                :order 1)))))
          (alltodo "" ((org-agenda-overriding-header "")
                       (org-super-agenda-groups
                        '((:discard (:tag "personal" ))
                          (:name "Vencimiento hoy"
                                 :deadline today
                                 :order 5)
                          (:name "Próximamente"
                                 :deadline future
                                 :order 11)
                          (:name "Atrasado"
                                 :scheduled past
                                 :deadline past
                                 :order 12)
                          (:name "Por hacer"
                                  ;:discard (:scheduled future :deadline future)
                                 :todo "PORHACER"
                                 :order 12)
                          (:name "Esperando"
                                 :todo "BLOQUEADO"
                                 :order 14)))))
          (tags "trabajo/HECHO"
                ((org-agenda-overriding-header " Tareas Hechas")))))

        ("z" "Vista personal"
         ((agenda "" ((org-agenda-span 3)
                      (org-super-agenda-groups
                       '((:name "Hoy"
                                :discard (:tag "trabajo" :scheduled past :deadline past)
                                :time-grid t
                                :date today
                                :scheduled today
                                :order 1)
                         (:name ""
                                :tag "agenda"
                                :todo "Aniversarios")))))
          (alltodo "" ((org-agenda-overriding-header "")
                       (org-super-agenda-groups
                        '((:discard (:tag "trabajo" ))
                          (:name "Vencimiento hoy"
                                 :deadline today
                                 :order 5)
                          (:name "Atrasado"
                                 :scheduled past
                                 :deadline past
                                 :order 11)
                          (:name "Por hacer"
                                 :discard (:scheduled future :deadline future)
                                 :todo "PORHACER"
                                 :order 12)
                          (:name "Esperando"
                                 :todo "BLOQUEADO"
                                 :order 14)))))
          (tags "personal/HECHO"
                ((org-agenda-overriding-header " Tareas Hechas")))))
        ))

(use-package calfw
    :config
  (setq cfw:org-overwrite-default-keybinding t)) ;; atajos de teclado de la agenda org-mode

(use-package calfw-org
    :ensure t
    :config
    (setq cfw:org-overwrite-default-keybinding t)
    :bind ([f8] . cfw:open-org-calendar))

(setq calendar-month-name-array
      ["Enero" "Febrero" "Marzo" "Abril" "Mayo" "Junio"
       "Julio"    "Agosto"   "Septiembre" "Octubre" "Noviembre" "Diciembre"])

(setq calendar-day-name-array
      ["Domingo" "Lunes" "Martes" "Miércoles" "Jueves" "Viernes" "Sábado"])

(setq org-icalendar-timezone "America/Santiago") ;; timezone
(setq calendar-week-start-day 1) ;; la semana empieza el lunes
(setq european-calendar-style t) ;; estilo europeo

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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

;;;;;;;;;;;; EF-THEMES ;;;;;;;;;

(quelpa 
  '(ef-themes 
    :repo "protesilaos/ef-themes" 
    :fetcher github))

(setq ef-themes-mixed-fonts t
      ef-themes-variable-pitch-ui t)

;; Disable all other themes to avoid awkward blending:
;(mapc #'disable-theme custom-enabled-themes)

(use-package ef-themes
  :config
  (setq ef-themes-to-toggle '(ef-summer ef-rosa))
  (load-theme 'ef-summer t))

;; Establece el nivel de transparencia del fondo
;;(set-frame-parameter (selected-frame) 'alpha '(90 . 90))
;;(add-to-list 'default-frame-alist '(alpha . (90 . 90)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
 '(org-agenda-files
   '("/home/cesar/develop/agenda/artificial-inteligence-template.org" "/home/cesar/develop/agenda/frd.org" "/home/cesar/develop/agenda/qmk.org"))
 '(package-selected-packages
   '(## mugur gnuplot-mode gnuplot visual-fill-column org-bullets calfw-org calfw org-contrib pulsar magit-gitflow py-isort use-package pyvenv python-black pylint magit lsp-ui lsp-python-ms lsp-pyright lsp-docker jedi-direx highlight-indent-guides grip-mode flycheck dired-sidebar company)))
