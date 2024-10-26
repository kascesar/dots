(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-visual-line-mode t)

(setq inhibit-startup-screen t)    ;; Desactiva la pantalla de inicio
(setq inhibit-startup-message t)   ;; Desactiva el mensaje de inicio

;; Mostrar el número de columna en el modo de línea
(column-number-mode)

;; Desactivar la visualización de números de columna en org-mode
(add-hook 'org-mode-hook (lambda () (column-number-mode -1)))
;; Habilitar los números de línea en la izquierda en modos de programación
(add-hook 'prog-mode-hook
          (lambda ()
            (display-line-numbers-mode 1)))
;; Ajustar el formato de los números de línea con espacio adicional
(setq display-line-numbers-width-start t)

;; Ocultar archivos ocultos en dired
(require 'dired-x)
(setq dired-omit-files "^\\...+$")
(add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1)))

;; dont ask when kill buffer
(global-set-key [remap kill-buffer] #'kill-this-buffer)
(kill-buffer "*scratch*")

(require 'org-tempo)
(with-eval-after-load 'org-tempo
(define-key org-mode-map (kbd "C-c TAB") 'org-tempo-expand))

(defun reload-config ()
  (interactive)
  (load-file user-init-file)
  (load-file user-init-file))

(set-frame-font "DejaVu Sans Mono-13" nil t)

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

;; undo and redo
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-z") 'undo)

;; org-agenda
(global-set-key (kbd "<f7>") 'org-agenda)
;; F10 para ir al Dashboard
(global-set-key (kbd "<f10>") 'open-dashboard)

(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                 ,@(when-let ((depth (plist-get order :depth)))
                                                     (list (format "--depth=%d" depth) "--no-single-branch"))
                                                 ,(plist-get order :repo) ,repo))))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(elpaca ef-themes
(use-package ef-themes
  :ensure t
  :demand t
  :config
  (load-theme 'ef-summer :no-confirm)))
(setq ef-themes-to-toggle '(ef-summer ef-cherie))

;; Desactivar el parpadeo del cursor y cambiarlo a tipo barra
(blink-cursor-mode 0)
(setq-default cursor-type 'bar)

;; Configuración de beacon usando elpaca
(elpaca beacon
;; Configurar el color del resplandor a un rozado muy fuerte
(setq beacon-color "#FF69B4")

;; Reducir el tamaño del resplandor
(setq beacon-size 20)

;; Variable para almacenar el tiempo desde el último movimiento del cursor
(defvar my-last-cursor-move-time 0)

;; Función para mostrar el resplandor periódicamente cuando el cursor está quieto
(defun my-beacon-periodic-blink ()
"Muestra el resplandor si el cursor ha estado quieto por más de 1 segundo."
(let ((idle-time (- (float-time) my-last-cursor-move-time)))
      (when (>= idle-time 1)  ;; Verificar si el cursor ha estado inactivo por 1 segundo
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

(elpaca highlight-indent-guides
  (use-package highlight-indent-guides
    :hook (prog-mode . highlight-indent-guides-mode)
    :config
    ;; Configurar el método de guía de indentación a 'bitmap
    (setq highlight-indent-guides-method 'bitmap)
    ;; (setq highlight-indent-guides-character "|")
    ;; Configurar el color de los guías de indentación
    (custom-set-faces
     '(highlight-indent-guides-stack-odd-face ((t (:background "gray20"))))
     '(highlight-indent-guides-stack-even-face ((t (:background "gray40"))))
     '(highlight-indent-guides-character-face ((t (:foreground "dim gray"))))
     '(italic ((t (:slant italic))))))
)

(elpaca all-the-icons)

(setq user-full-name "Cesar M.")
(setq dashboard-icon-type 'all-the-icons) ;; usar `all-the-icons`

;; Configuración de `dashboard` usando `elpaca`
(elpaca dashboard
  :ensure t
  :config
  ;; Configurar banner y título
  (setq dashboard-startup-banner "~/.emacs.d/dh.jpeg")
  (setq dashboard-banner-logo-title (format "Buen día %s" user-full-name))
  (setq dashboard-items '((recents . 4)
			  (bookmarks . 5)
			  (agenda . 2)))
  ;; Activar el hook de inicio de dashboard
  (dashboard-setup-startup-hook)
  ;; Configuración de íconos y navegación
  (setq dashboard-set-heading-icons t
	dashboard-set-file-icons t
	dashboard-set-init-info t
	dashboard-set-navigator t)
  ;; Definir botones de navegación personalizados
  (setq dashboard-navigator-buttons
	`((,(when (display-graphic-p)
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
	       (org-agenda-list)))))))

;; Función para abrir el dashboard con la tecla F10
;(defun open-dashboard ()
;  "Abre el buffer *dashboard* y salta al primer widget."
;  (interactive)
;  (delete-other-windows)
;  (if (get-buffer dashboard-buffer-name)
;	(kill-buffer dashboard-buffer-name))
;  (dashboard-insert-startupify-lists)
;  (switch-to-buffer dashboard-buffer-name))

(defun open-dashboard ()
"Open the *dashboard* buffer and jump to the first widget."
(interactive)

(delete-other-windows)
(if (get-buffer dashboard-buffer-name)
    (kill-buffer dashboard-buffer-name))
(dashboard-insert-startupify-lists)
(switch-to-buffer dashboard-buffer-name))

(elpaca magit
  :ensure t
  :demand t)

(use-package org
:config
(setq org-ellipsis  "⤵"))

(elpaca toc-org)
(use-package toc-org
  :commands toc-org-enable
  :init (add-hook 'org-mode-hook 'toc-org-enable))

(elpaca org-bullets
  :ensure t  ;; Asegúrate de que se instale correctamente
  :config
  (setq org-bullets-bullet-list '("●" "○" "◆" "▶" "▷" "❖" "✦")))
(add-hook 'org-mode-hook 'org-bullets-mode)

(elpaca org-super-agenda
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


(setq calendar-month-name-array
      ["Enero" "Febrero" "Marzo" "Abril" "Mayo" "Junio"
       "Julio"    "Agosto"   "Septiembre" "Octubre" "Noviembre" "Diciembre"])

(setq calendar-day-name-array
      ["Domingo" "Lunes" "Martes" "Miércoles" "Jueves" "Viernes" "Sábado"])

(setq org-icalendar-timezone "America/Santiago") ;; timezone
(setq calendar-week-start-day 1) ;; la semana empieza el lunes
(setq european-calendar-style t) ;; estilo europeo

(elpaca org-modern
  :ensure t)

;; Add frame borders and window dividers
(modify-all-frames-parameters
 '((right-divider-width . 10)
   (internal-border-width . 10)))

(setq org-modern-star '("●" " ■" "   ▲" "    ▼")  ;; Usar símbolos negros
      org-modern-list '((43 . "➤") (45 . "–") (42 . "•"))
      org-modern-block-fringe t
      org-modern-table t
      org-modern-checkbox '((?X . "✓") (?- . "✗") (?\s . "⬚")))
(add-hook 'org-modern-mode-hook
	  (lambda ()
	    (set-face-attribute 'org-modern-star-1 nil :foreground "black")   ;; Nivel 1
	    (set-face-attribute 'org-modern-star-2 nil :foreground "red")     ;; Nivel 2
	    (set-face-attribute 'org-modern-star-3 nil :foreground "green")   ;; Nivel 3
	    (set-face-attribute 'org-modern-star-4 nil :foreground "blue")))   ;; Nivel 4
;; Activa org-modern-mode automáticamente al abrir un archivo org
(add-hook 'org-mode-hook 'org-modern-mode)
(setq org-indent-mode-turns-on-hiding-stars nil) ;; Para mantener visibles los asteriscos
(setq org-indent-indentation-per-level 2) ;; Ajustar el tamaño de indentación por nivel
(setq org-modern-block nil)
