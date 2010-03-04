;;; nb.el --- Nota Bene, a quick note-taking tool for Emacs

(defun nb-rename-buffer-accordingly ()
  (interactive)
  (unless (zerop (buffer-size))
    (let* ((first-line (save-excursion
                         (goto-char (point-min))
                         (end-of-line)
                         (buffer-substring (point-min) (point))))
           (new-name (concat (file-name-directory (buffer-file-name))
                             (replace-regexp-in-string
                              "[^a-z0-9.-]" "-" first-line))))
      (unless (equal (buffer-file-name) new-name)
        (let ((backup-by-copying nil)
              (buffer-backed-up nil))
          (backup-buffer))
        (set-visited-file-name new-name)))))

(defun nb-remove-empty-files ()
  (interactive)
  (when (zerop (buffer-size))
    (delete-file (buffer-file-name))
    (message "Removed empty note %s" (buffer-file-name))))

(defun nb-new-note ()
  (interactive)
  (switch-to-buffer "*new note*")
  (set-visited-file-name "~/NB/.new")
  (rename-buffer "*new note*")
  (nb-mode 1)
  (set-buffer-modified-p nil))

(defun nb (terms)
  (interactive "MSearch terms: ")
  (let ((output (shell-command-to-string (concat "nb " terms))))
    (if (zerop (length output))
        (progn
          (message "Nothing found.")
          (nb-new-note)
          (insert terms)
          (insert "\n\n")
          (set-buffer-modified-p nil))
      (if (= 2 (length (split-string output "\n")))
          ;; one match
          (progn
            (find-file (concat "~/NB/" (substring output 0 -1)))
            (nb-mode 1))
      (switch-to-buffer "*nb-search*")
      (delete-region (point-min) (point-max))
      (insert output)
      (nb-make-links)
      (goto-char (point-min))
      (setq buffer-read-only t)
      ))))

(defun nb-make-links ()
  (interactive)
  (goto-char (point-min))
  (while (not (eobp))
    (when (looking-at "\\S-")
      (let ((button (make-button (point-at-bol) (point-at-eol))))
        (button-put button 'action #'nb-find-note)
        (button-put button 'file
                    (concat "~/NB/"
                            (buffer-substring (point-at-bol) (point-at-eol))))
        ))
    (forward-line)))

(defun nb-find-note (button)
  (kill-buffer)
  (find-file (button-get button 'file))
  (nb-mode 1))

(defun nb-find-at-point ()
  (interactive)
  (if (save-excursion
        (re-search-backward "\\[\\[\\|\\]\\]" (point-at-bol) t)
        (looking-at "\\[\\[\\(.*?\\)\\]\\]"))
      (nb (match-string 1))
    (insert "\n")))

(defun nb-find-at-mouse (event)
  (interactive "e")
  (if (save-excursion
        (set-buffer (window-buffer
                     (posn-window (event-start event))))
        (goto-char (posn-point (event-start event)))
        (re-search-backward "\\[\\[\\|\\]\\]" (point-at-bol) t)
        (looking-at "\\[\\[\\(.*?\\)\\]\\]"))
      (nb (match-string 1))
    (mouse-set-mark event)))

(define-minor-mode nb-mode
  "Minor mode for quick note taking."
  nil
  " NB"
  '()
  (add-hook 'before-save-hook 'nb-rename-buffer-accordingly t t)
  (add-hook 'after-save-hook 'nb-remove-empty-files t t))

(define-key nb-mode-map [return] 'nb-find-at-point)
(define-key nb-mode-map [mouse-1] 'nb-find-at-mouse)

(provide 'nb)
