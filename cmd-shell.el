(defun cmd-shell-prompt-input (type)
  (funcall (gethash type cmd-shell-input-forms
                    (lambda ()
                      (read-from-minibuffer "Text input: ")))))

(defun cmd-shell-prompt-create-input (cmd-str)
  ;; Use null string as escaped % for simplicity of implementation.
  (let ((start 0)
        (built-str "")
        (cmd-str (replace-regexp-in-string "%%" "\0" cmd-str)))
    (while (string-match "%\\(\\w+\\)" cmd-str start)
      (let ((mstart (match-beginning 0))
            (mend (match-end 0))
            (type (match-string 1 cmd-str)))
        (setq built-str (concat built-str (substring cmd-str start mstart)))
        (setq built-str (concat built-str
                                (cmd-shell-prompt-input (match-string 1 cmd-str))))
        (setq start mend)))
    (setq built-str (concat built-str (substring cmd-str start)))
    (replace-regexp-in-string "\0" "%" built-str)))

(defun cmd-shell-fetch-current-cmd ()
  (interactive)
  (save-excursion
    (let ((old-point (point))
          end-of-cmd
          beginning-of-cmd)

      ;; Find end of cmd.
      (end-of-line)
      (while (and (eq (char-before) ?\\) (not (eq (point) (point-max))))
        (forward-line)
        (end-of-line))
      (setq end-of-cmd (point))

      (goto-char old-point)
      ;; Find start of cmd.
      (beginning-of-line)
      (while (eq (char-before (- (point) 1)) ?\\)
        (previous-line)
        (beginning-of-line))

      (setq start-of-cmd (point))
      (buffer-substring start-of-cmd end-of-cmd))))

(defun cmd-shell-send-lines (cmd-str)
  (with-current-buffer cmd-shell-shell
    (goto-char (point-max))
    (comint-kill-input)
    (let ((lines (split-string cmd-str "\n")))
      (mapcar (lambda (line)
                (goto-char (point-max))
                (insert line)
                (comint-send-input)
                (accept-process-output)) lines))))

(defun cmd-shell-send-input ()
  (interactive)
  (let ((cmd-str (cmd-shell-prompt-create-input (cmd-shell-fetch-current-cmd)))
        (process (get-buffer-process cmd-shell-shell)))
    (if (not (string= cmd-str ""))
        (cmd-shell-send-lines cmd-str))))

(defun cmd-shell-return ()
  (interactive)
  (if (not (eq (char-before) ?\\))
      (cond ((eq (point) (point-max))
             (progn
               (cmd-shell-send-input)
               (newline)))
            ((and (eq (point) (- (point-max) 1))
                  (eq (char-after) ?\n))
             (progn
               (cmd-shell-send-input)
               (forward-line)))
            (t (newline)))
    (newline)))

(setq cmd-shell-input-forms (make-hash-table :test 'equal))

(let ((git-branches
       (lambda ()
         (completing-read
          "Branch: "
          (delete "" (split-string
                      (shell-command-to-string "git branch | sed 's/^[* ] //'")
                      "\n"))
          nil t))))
  (puthash "branch" git-branches cmd-shell-input-forms)
  (puthash "b" git-branches cmd-shell-input-forms)
  (puthash "file" 'ido-find-file cmd-shell-input-forms)
  (puthash "f" 'ido-find-file cmd-shell-input-forms))

(define-derived-mode cmd-shell-mode text-mode "Cmd Shell"
  "Cmd shell mode
\\{cmd-shell-mode-map}"
  (set (make-local-variable 'cmd-shell-shell-name) "*cmd-shell-shell*")
  (save-window-excursion
    (shell cmd-shell-shell-name))
  (set (make-local-variable 'cmd-shell-shell) (get-buffer cmd-shell-shell-name)))

(define-key cmd-shell-mode-map (kbd "<return>") 'cmd-shell-return)
(define-key cmd-shell-mode-map (kbd "<C-return>") 'cmd-shell-send-input)

(provide 'cmd-shell)