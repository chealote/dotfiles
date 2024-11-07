(tool-bar-mode -1)
(menu-bar-mode -1)
(savehist-mode 1)
(blink-cursor-mode -1)

(whitespace-mode t)
(scroll-bar-mode 0)

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

(add-hook 'c-mode-common-hook
	  (lambda ()
	    (c-set-style "linux")))

(add-hook 'prog-mode-hook
	  (lambda ()
	    (whitespace-mode)
	    (display-line-numbers-mode)))

(add-hook 'csharp-mode-hook
	  (lambda ()
	    (indent-tabs-mode -1)
	    (setq-local tab-width 4)))

(add-hook 'mhtml-mode-hook
	  (lambda ()
	    (indent-tabs-mode -1)))

(add-hook 'markdown-mode-hook
	  (lambda ()
	    (local-unset-key (kbd "M-n"))
	    (local-unset-key (kbd "M-p"))
	    (flyspell-mode)
	    (flyspell-buffer)))

(global-unset-key (kbd "C-z"))

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

;; TODO make this command interactive only in c-mode
(defun che-c-indent ()
  ;; format c files with "indent -linux"
  (interactive)
  ;; TODO this save-excursion is supposed to save my cursor position
  ;; and return after running the shell cmd, but it's not doing that
  (save-excursion  (call-process-region
		    (point-min)
		    (point-max)
		    "indent"
		    :buffer (current-buffer)
		    :args "-linux")))

(defvar che-journal-base-path "~/Documents/journal/")

;; TODO keep working on journal functions
(defun che-journal-today ()
  (interactive)
  (defconst base-path che-journal-base-path)
  (find-file (concat base-path (format-time-string "%Y-%m-%d.txt.gpg"))))

(defun che-open-terminal ()
  (interactive)
  (call-process-shell-command "st"))
(global-set-key (kbd "C-c C-t") 'che-open-terminal)

(setq org-agenda-files
      '("~/Documents/org/"))

(defun che-save-image (image-name)
  (interactive "MName of the image: ")
  (call-process-shell-command
   (concat "xclip -selection clipboard -t image/png -o > " image-name)))
