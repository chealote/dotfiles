(tool-bar-mode -1)
(menu-bar-mode -1)
(savehist-mode 1)
(blink-cursor-mode -1)

(whitespace-mode t)
(scroll-bar-mode 0)

;; from a 4chan lisp post in /g/
(setq isearch-allow-scroll 'unlimited)

;; https://stackoverflow.com/questions/15390178/emacs-and-symbolic-links
(setq vc-follow-symlinks t)

;; https://emacs.stackexchange.com/questions/17172/how-to-make-end-of-buffer-visible
;; TODO don't want to use setq-default because t is not the default value
;; and setq _should_ be global?
(setq indicate-empty-lines t)

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

;; TODO map key to delete images easily
;; TODO function to set current image as background
(defun che-image-mode-rm ()
  (interactive)
  (che-image-mode-format-command "rm " buffer-file-name))

(defun che-image-mode-background ()
  (interactive)
  (che-image-mode-format-command "feh --bg-fill " buffer-file-name))

(defun che-image-mode-format-command (command filename)
  (if (equal major-mode 'image-mode)
      (let (
	    (full-command (concat command filename))
	    )
      (if (y-or-n-p (concat "Run " full-command "?"))
	  (message full-command)
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
  (call-process-shell-command "st" nil 0))
(global-set-key (kbd "C-c C-t") 'che-open-terminal)

(setq org-agenda-files
      '("~/Documents/org" "~/Documents/org/work"))

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
    (che-json-format-between-points (pos-bol) (pos-eol))))

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
