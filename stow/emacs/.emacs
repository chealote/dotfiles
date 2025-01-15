(tool-bar-mode -1)
(menu-bar-mode -1)
(savehist-mode 1)
(blink-cursor-mode -1)

(global-auto-revert-mode t)

(whitespace-mode t)
(scroll-bar-mode 0)

;; from a 4chan lisp post in /g/
(setq isearch-allow-scroll 'unlimited)

;; https://stackoverflow.com/questions/15390178/emacs-and-symbolic-links
(setq vc-follow-symlinks t)

;; https://emacs.stackexchange.com/questions/17172/how-to-make-end-of-buffer-visible
;; TODO don't want to use setq-default because t is not the default value
;; and setq _should_ be global?
(setq-default indicate-empty-lines t)

;; ignored directories for M-x rgrep
;; (push "node_modules" grep-find-ignored-directories)

(setq save-interprogram-paste-before-kill t)
(setq x-select-enable-clipboard nil)
(global-set-key (kbd "C-c y") 'clipboard-yank)
(global-set-key (kbd "C-c w") 'clipboard-kill-ring-save)

(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying t)

(defun che-full-open-command (filepath)
  (concat che-custom-open-command "'" filepath "'"))

;; open is https://github.com/chealote/open
(defvar che-custom-open-command "open "
  "The custom command that I want to open files with")

;; TODO this should be using ffap or something else
(defun che-dired-custom-open-file ()
  "Open a file using dired-custom-open"
  (interactive)
  ;; (call-process-shell-command
  ;; (setq-local display-buffer-alist 'display-buffer-no-window)
  (call-process-shell-command
   (che-full-open-command (dired-get-file-for-visit)) nil 0))

(add-hook 'dired-mode-hook
          (lambda ()
              (local-set-key (kbd "C-c C-o") 'che-dired-custom-open-file)))

(add-hook 'js-mode-hook
	  (lambda ()
	    (setq-local js-indent-level 2)
	    (indent-tabs-mode -1)))

(add-hook 'go-mode-hook
	  (lambda ()
	    (setq-local tab-width 4)))

(add-hook 'c-mode-hook
	  (lambda ()
	    (indent-tabs-mode 1)
	    (setq-local tab-width 4)
	    (setq-local c-basic-offset 4)))

(add-hook 'prog-mode-hook
	  (lambda ()
	    (whitespace-mode)
	    (display-line-numbers-mode)))

(add-hook 'csharp-mode-hook
	  (lambda ()
	    (indent-tabs-mode -1)
	    (setq-local c-basic-offset 2)
	    (setq-local tab-width 2)))

(add-hook 'mhtml-mode-hook
	  (lambda ()
	    (indent-tabs-mode -1)))

(add-hook 'markdown-mode-hook
	  (lambda ()
	    (local-unset-key (kbd "M-n"))
	    (local-unset-key (kbd "M-p"))
	    (flyspell-mode)
	    (flyspell-buffer)))

(add-hook 'transient-mark-mode-hook
	  (lambda ()
	    (whitespace-mode nil)))

(global-unset-key (kbd "C-z"))

(add-hook 'ediff-mode-hook
	  (lambda ()
	    (setq-local ediff-keep-variants nil)
	    (setq-local ediff-window-setup-function 'ediff-setup-windows-plain)
	    (setq-local ediff-split-window-function 'split-window-horizontally)))

(add-hook 'sh-mode-hook
	  (lambda ()
	    (indent-tabs-mode -1)))

(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (indent-tabs-mode -1)))

;; TODO map key to delete images easily
;; TODO function to set current image as background
(defun che-image-mode-rm ()
  (interactive)
  (che-image-mode--prompt-format-command "rm " buffer-file-name))

(defun che-image-mode-background ()
  (interactive)
  (che-image-mode--prompt-format-command "feh --bg-fill " buffer-file-name))

(defun che-image-mode--prompt-format-command (command filename)
  (if (equal major-mode 'image-mode)
      (let ((full-command (concat command filename)))
        (if (y-or-n-p (concat "Run " full-command "?"))
	    (shell-command full-command)
	  (message "Command cancelled...")))
    (message "Not in image-mode")))

;; c-a moves to beggining of line or beggining of text
(global-set-key (kbd "C-a") 'beginning-of-line-or-indentation)
(defun beginning-of-line-or-indentation ()
  "move to beginning of line, or indentation"
  (interactive)
  (if (bolp)
          (back-to-indentation)
    (beginning-of-line)))

(global-set-key (kbd "M-N") 'scroll-up-line)
(global-set-key (kbd "M-P") 'scroll-down-line)

(setq inhibit-splash-screen t)

(global-set-key (kbd "C-=") 'er/expand-region)

(setq org-agenda-files
      '("~/Documents/org" "~/Documents/org/work"))

;; TODO make this command interactive only in c-mode
(defun che-c-indent ()
  ;; format c files with "indent -linux"
  (interactive)
  ;; TODO this save-excursion is supposed to save my cursor position
  ;; and return after running the shell cmd, but it's not doing that
  (if (= (call-process "which" nil nil nil "indent") 0)
      (call-process-region
       (point-min)
       (point-max)
       "indent"
       :delete t
       :display t)
    (message "`indent` program is not installed")))

   ;; :buffer (current-buffer)
   ;; :args "-linux")
  ;; (save-excursion  (call-process-region
  ;; 		    (point-min)
  ;; 		    (point-max)
  ;; 		    "indent"
  ;; 		    :buffer (current-buffer)
  ;; 		    :args "-linux")))

(defvar che-journal-base-path "~/Documents/journal/")

;; TODO keep working on journal functions
(defun che-journal-today ()
  (interactive)
  (defconst base-path che-journal-base-path)
  (find-file (concat base-path (format-time-string "%Y-%m-%d.txt.gpg"))))

(defun che-open-terminal ()
  (interactive)
  (call-process-shell-command "st" nil 0))
(global-set-key (kbd "C-c C-t") 'che-open-terminal)

(defun che-save-image (image-name)
  (interactive "MName of the image: ")
  (call-process-shell-command
   (concat "xclip -selection clipboard -t image/png -o > " image-name)))

(defvar che-json-format-command "jq")
(defun che-json-format-between-points (pb pe)
  (shell-command-on-region pb pe che-json-format-command t t))

;; pos-bol, pos-eol taken from here: http://xahlee.info/emacs/emacs/elisp_all_about_lines.html
(defun che-json-format ()
  (interactive)
  (if (region-active-p)
      (che-json-format-between-points (region-beginning) (region-end))
    ;; I usually paste a single line with the json and want that formatted
    (che-json-format-between-points (line-beginning-position) (line-end-position))))

;; watch the video again of multiple-cursors
;; TODO not using these enough
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-c j") 'mc/mark-all-dwim)

(defvar che-wallpapers-path "~/Pictures/Wallpapers")
(defun che-wallpapers-update-wallpaper ()
  (interactive)
  (let ((files (directory-files che-wallpapers-path t "[0-9]+\\.\\(jpg\\|png\\)")))
    (let ((file (nth (random (length files)) files)))
      (shell-command (concat "feh --bg-fill " file)))))

(setq che--is-light-theme t)
(defun che-toggle-theme ()
  (interactive)
  (if che--is-light-theme
      (progn ;; set dark theme
        (set-face-attribute 'default nil :background "#111" :foreground "#ddd" :height 135 :family "Fira Code")
        (set-face-attribute 'region  nil :background "#333")
        (set-face-attribute 'whitespace-hspace  nil :foreground "#444")
        (set-face-attribute 'whitespace-indentation  nil :foreground "#444" :background "#111")
        (set-face-attribute 'whitespace-missing-newline-at-eof  nil :background "#444")
        (set-face-attribute 'whitespace-space  nil :foreground "#444")
        (set-face-attribute 'whitespace-tab  nil :foreground "#444")
        (set-face-attribute 'whitespace-space-before-tab nil)
        (set-face-attribute 'whitespace-line nil :inherit 'default :background "#333")
        (set-face-attribute 'line-number nil :inherit 'default :foreground "#777")
        (set-face-attribute 'line-number-current-line nil :inherit 'line-number :weight 'bold :foreground "#777")
        ;; (set-face-attribute 'markdown-header-face nil :inherit 'font-lock-function-name-face)
        ;; (set-face-attribute 'markdown-header-face-1 nil :inherit 'outline-1 :height 1.5)
        ;; (set-face-attribute 'markdown-header-face-2 nil :inherit 'outline-2 :height 1.4)
        ;; (set-face-attribute 'markdown-header-face-3 nil :inherit 'outline-3 :height 1.3)
        ;; (set-face-attribute 'markdown-header-face-4 nil :inherit 'markdown-header-face :height 1.1)

        (setq che--is-light-theme nil))
    (progn ;; set light theme
      (set-face-attribute 'default nil :background "#ddd" :foreground "#111" :height 135 :family "Liberation Mono")
      (set-face-attribute 'region  nil :background "#bbb")
      (set-face-attribute 'whitespace-hspace  nil :foreground "#bbb")
      (set-face-attribute 'whitespace-indentation  nil :foreground "#bbb")
      (set-face-attribute 'whitespace-missing-newline-at-eof  nil :background "#bbb")
      (set-face-attribute 'whitespace-space  nil :foreground "#bbb")
      (set-face-attribute 'whitespace-tab  nil :foreground "#bbb")
      (set-face-attribute 'whitespace-space-before-tab nil)
      (set-face-attribute 'line-number nil :inherit 'default :foreground "#777")
      (set-face-attribute 'line-number-current-line nil :inherit 'line-number :weight 'bold :foreground "#444")
      ;; (set-face-attribute 'markdown-header-face nil :inherit 'font-lock-function-name-face)
      ;; (set-face-attribute 'markdown-header-face-1 nil :inherit 'outline-1 :height 1.5)
      ;; (set-face-attribute 'markdown-header-face-2 nil :inherit 'outline-2 :height 1.4)
      ;; (set-face-attribute 'markdown-header-face-3 nil :inherit 'outline-3 :height 1.3)
      ;; (set-face-attribute 'markdown-header-face-4 nil :inherit 'markdown-header-face :height 1.1)

      (setq che--is-light-theme t))))

(che-toggle-theme)

;; TODO old theme from custom-var
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(default ((t (:inherit nil :extend nil :stipple nil :background "#ddd" :foreground "#111" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 135 :width normal :foundry "1ASC" :family "Liberation Mono"))))
;;  '(fixed-pitch ((t (:background "#333" :family "Liberation Mono"))))
;;  '(line-number ((t (:inherit default :foreground "#777"))))
;;  '(line-number-current-line ((t (:weight bold :foreground "#999" :inherit line-number))))
;;  '(markdown-header-face ((t (:inherit font-lock-function-name-face))))
;;  '(markdown-header-face-1 ((t (:inherit outline-1 :height 1.5))))
;;  '(markdown-header-face-2 ((t (:inherit outline-2 :height 1.4))))
;;  '(markdown-header-face-3 ((t (:inherit outline-3 :height 1.3))))
;;  '(markdown-header-face-4 ((t (:inherit markdown-header-face :height 1.1))))
;;  '(org-level-1 ((t (:inherit outline-1 :extend nil))))
;;  '(org-level-2 ((t (:inherit outline-2 :extend nil))))
;;  '(region ((t (:extend t :background "#444"))))
;;  '(whitespace-empty ((t (:extend t))))
;;  '(whitespace-hspace ((t (:foreground "#444"))))
;;  '(whitespace-indentation ((t (:foreground "#444"))))
;;  '(whitespace-line ((t nil)))
;;  '(whitespace-missing-newline-at-eof ((t (:background "#444"))))
;;  '(whitespace-space ((t (:foreground "#444"))))
;;  '(whitespace-tab ((t (:foreground "#444")))))

