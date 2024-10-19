;; (package-refresh-contents) ;; TODO do I need to run this everytime?

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
  (concat custom-open-command "'" filepath "'"))

;; open is https://github.com/chealote/open
(defvar custom-open-command "open "
  "The custom command that I want to open files with")

(defun che-dired-custom-open-file ()
  "Open a file using dired-custom-open"
  (interactive)
  (call-process-shell-command
   (che-full-open-command (dired-get-file-for-visit))))

(add-hook 'dired-mode-hook
          (lambda ()
              (local-set-key (kbd "C-c C-o") 'che-dired-custom-open-file)))

(add-hook 'js-mode-hook
	  (lambda ()
	    (setq-local js-indent-level 2)))

(add-hook 'go-mode-hook
	  (lambda ()
	    (setq-local tab-width 4)))

(add-hook 'c-mode-common-hook
	  (lambda ()
	    (c-set-style "linux")))

(add-hook 'prog-mode-hook
	  (lambda ()
	    (whitespace-mode)))

(add-hook 'csharp-mode-hook
	  (lambda ()
	    (indent-tabs-mode -1)
	    (setq-local tab-width 4)))

(add-hook 'mhtml-mode-hook
	  (lambda ()
	    (indent-tabs-mode -1)))

;; c-a moves to beggining of line or beggining of text
(global-set-key (kbd "C-a") 'beginning-of-line-or-indentation)
(defun beginning-of-line-or-indentation ()
  "move to beginning of line, or indentation"
  (interactive)
  (if (bolp)
          (back-to-indentation)
    (beginning-of-line)))

(defun che-move-empty-line (move-to-line)
  (interactive)
  (if (string= (thing-at-point 'line t) "\n")
      (while (string= (thing-at-point 'line t) "\n") (funcall move-to-line)))
  (while (not (string= (thing-at-point 'line t) "\n")) (funcall move-to-line)))

(defun che-next-empty-line ()
  (interactive)
  (che-move-empty-line 'next-line))

(defun che-previous-empty-line ()
  (interactive)
  (che-move-empty-line 'previous-line))
(global-set-key (kbd "M-n") 'che-next-empty-line)
(global-set-key (kbd "M-p") 'che-previous-empty-line)

(global-set-key (kbd "M-N") 'scroll-down-line)
(global-set-key (kbd "M-P") 'scroll-up-line)

(setq inhibit-splash-screen t)

(global-set-key (kbd "C-=") 'er/expand-region)
;; TODO find a way to not replace negative argument for only this er command
;; (global-set-key (kbd "C--") 'er/contract-region)

;; TODO I'm trying to match a group even if there's spaces before
;; whatever is considered a group
(defun che-forward-list ()
  (interactive)
  (while (and (not (forward-list))
	      (not (= (char-after) 10)) (forward-char))))

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
