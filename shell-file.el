;; TODO more documentation

(defcustom shell-file-path "~/bin/shell-file.sh"
  "Path to the shell-file collection of shell scripts")

(defcustom shell-file-dir (format "/tmp/shell-file.%s" (user-login-name))
  "Where shell-file commands and output will be logged")

(defcustom shell-file-remote-host nil
  "Remote host to execute shell-file commands on.
Set this to a string like \"user@host\" to enable remote execution.")

(setq shell-file-init-line-re "^### START #########################.*\n")
(setq shell-file-exit-line-re "^exit ##############################.*\n")

(setq shell-file-mode-map (make-sparse-keymap))

(define-minor-mode shell-file-mode
  "Minor mode for use within the shell-file"
  :lighter " shell-file"
  :keymap 'shell-file-mode-map)

(defun shell-file-find ()
  "Open the shell-file"
  (interactive)
  (when (not (file-exists-p shell-file-path))
    (find-file shell-file-path)
    (insert (format "\
#!%s
# -*- eval: (shell-file-mode t) -*-
source ~/bin/shell-file-prelude.sh # lines above START get run every time

### START ##########################################################

echo 'hello world'
echo 'this message brought to you by shell-file'

exit ###############################################################

cd ~/src/foo/
make

exit ###############################################################

cd /home
for user in *; do
  echo hello $user
done

exit ###############################################################
" shell-file-name) )
    (save-buffer)
    )
  (find-file shell-file-path))

(defun shell-file-search-text (re)
  (save-window-excursion
    (save-excursion
      (shell-file-find)
      (goto-char (point-min))
      (search-forward-regexp re)
      (buffer-substring-no-properties (match-beginning 0) (match-end 0))
      )))

(defun shell-file-init-line ()
  (shell-file-search-text shell-file-init-line-re))

(defun shell-file-exit-line ()
  (shell-file-search-text shell-file-exit-line-re))

(defun shell-file-insert-block (&optional block-text)
  "insert a new shell block on top of the shell-file"
  (interactive)
  (unless block-text
    (when (and (featurep 'evil) (evil-visual-state-p))
      (setq block-text
            (buffer-substring-no-properties
             (region-beginning) (region-end)))
      (evil-normal-state)))
  (let* ((init-line (shell-file-init-line))
         (exit-line (shell-file-exit-line)))
    (shell-file-find)
    (goto-line 1)
    (search-forward init-line)
    (insert "\n\n\n" exit-line)
    (forward-line -3)
    (when block-text (insert block-text)))
  )

(defun shell-file-cd-command (dir)
  "the cd command inserted into shell-file.sh"
  (concat "cd " dir))

(defun shell-file-insert-cd ()
  "insert a new shell block on top of the shell-file"
  (interactive)
  (let* ((dir default-directory))
    (shell-file-insert-block (shell-file-cd-command dir))))

(defun shell-file-insert-file ()
  "insert a new shell block on top of the shell-file"
  (interactive)
  (let* ((dir default-directory)
         (file
          (file-name-nondirectory
           (if (eq major-mode 'dired-mode) (dired-filename-at-point)
             (buffer-file-name)))))
    (shell-file-insert-block (concat (shell-file-cd-command dir) "\n./" file))))

(defun shell-file-delete-block (num-times)
  "delete a shell block off the top of the shell-file"
  (interactive "p")
  (let* ((init-line (shell-file-init-line))
         (exit-line (shell-file-exit-line)))
    (dotimes (_ num-times)
      (when (not (search-backward exit-line nil t)) (search-backward init-line))
      (next-line)
      (let ((beg (point)))
        (search-forward exit-line)
        (delete-region beg (point)))
      )))

(defun shell-file-split-block ()
  "delete a shell block off the top of the shell-file"
  (interactive)
  (let* ((f (shell-file-exit-line)))
    (beginning-of-line)
    (insert "\n" f)
    (forward-line -2)
    ))

(defun shell-file-bubble-block (keep-top)
  "bubble a shell block up to the top of the shell-file"
  (interactive "P")
  (catch 'body
    (let* ((init-line (shell-file-init-line))
           (exit-line (shell-file-exit-line)))
      (when (not (search-backward exit-line nil t))
        (if keep-top (throw 'body nil)
          (progn
            (search-forward exit-line)
            (search-backward exit-line))))
      (next-line)
      (let ((beg (point)))
        (search-forward exit-line)
        (setq last-command nil)  ; Don't append to the previous kill.
        (kill-region beg (point)))
      (goto-line 1)
      (search-forward init-line)
      (yank)
      (goto-line 1)
      (search-forward init-line)
      (next-line 1))))

(defun shell-file-next-block (num-times)
  "move one block down in the shell-file"
  (interactive "p")
  (let* ((init-line (shell-file-init-line))
         (exit-line (shell-file-exit-line)))
    (dotimes (_ num-times)
      (cond
       ((search-backward exit-line nil t) (search-forward exit-line) (search-forward exit-line))
       ((search-backward init-line nil t) (next-line) (search-forward exit-line))
       (t (search-forward init-line))
       )
      (next-line))))

(defun shell-file-prev-block (num-times)
  "move one block up in the shell-file"
  (interactive "p")
  (let* ((init-line (shell-file-init-line))
         (exit-line (shell-file-exit-line)))
    (dotimes (_ num-times)
      (cond
       ((not (search-backward exit-line nil t)) (search-backward init-line))
       ((search-backward exit-line nil t) (search-forward exit-line))
       (t (search-backward init-line) (next-line))
       )
      (next-line))))

(defun shell-file-current-buffer-is-block-buffer ()
  (let* ((cur-buf (current-buffer))
         (shell-file-buf (find-buffer-visiting shell-file-path)))
    (equal shell-file-buf cur-buf)))

(defun shell-file-output ()
  "open the first inactive *shell-file.NUM.out* output buffer"
  (catch 'body
    (let* ((slot 0))
      (while t
        (let* ((name (format "*shell-file.%d.out*" slot))
               (buf (get-buffer name)))
          (when (not buf) (throw 'body (switch-to-buffer name)))
          (when (not (save-window-excursion (get-buffer-process buf)))
            (throw 'body (switch-to-buffer name)))
          (setq slot (+ slot 1)))))))


(defun shell-file-log (message)
  "Log a message to the *Messages* buffer."
  (message "[Shell-File] %s" message))

(defun shell-file-run ()
  "Run shell-file top block in the background, locally or remotely."
  (interactive)
  (when (shell-file-current-buffer-is-block-buffer)
    (shell-file-bubble-block t))
  (let* ((remote-prefix (when shell-file-remote-host
                          (format "/ssh:%s:" shell-file-remote-host)))
         (default-directory (if remote-prefix
                                (concat remote-prefix "/")
                              default-directory))
         (dir (if remote-prefix
                  (concat remote-prefix shell-file-dir)
                shell-file-dir))
         (_   (make-directory dir t))
         (uid (format-time-string "%Y-%m-%d.%H:%M:%S"))
         (out-file (expand-file-name (format "%s.output" uid) dir)))

    (shell-file-log "Preparing to run command...")
    (shell-file-log (format "Remote host: %s" (or shell-file-remote-host "local")))
    (shell-file-log (format "Working directory: %s" default-directory))

    (shell-file-write-command-file out-file)
    (shell-file-execute-command out-file remote-prefix)))

(defun shell-file-write-command-file (cmd-file)
  "Write the shell-file command to the specified file, including the prelude and first block."
  (shell-file-log (format "Writing command to %s" cmd-file))
  (save-window-excursion
    (save-excursion
      (shell-file-find)
      (goto-char (point-min))
      (let ((block-end (progn
                         (search-forward-regexp (shell-file-init-line))
                         (search-forward-regexp (shell-file-exit-line))
                         (search-forward-regexp "^")
                         (match-beginning 0))))
        (let ((content (buffer-substring-no-properties (point-min) block-end)))
          (write-region content nil cmd-file))))))

(defun shell-file-execute-command (out-file remote-prefix)
  "Execute the shell-file command, locally or remotely."
  (let* ((out-file-local (file-local-name out-file))
         (full-cmd (format "bash %s 2>&1 | tee -a %s"
                           (shell-quote-argument out-file-local)
                           (shell-quote-argument out-file-local))))
    (shell-file-log (format "Executing command: %s" full-cmd))
    (let ((buf (shell-file-output)))
      (if remote-prefix
          (let ((remote-cmd (format "ssh %s %s"
                                    (shell-quote-argument (file-remote-p remote-prefix 'host))
                                    (shell-quote-argument full-cmd))))
            (shell-file-log (format "Remote command: %s" remote-cmd))
            (async-shell-command remote-cmd buf))
        (async-shell-command full-cmd buf))
      (display-buffer buf nil 'visible))
    (shell-file-log "Command execution initiated.")))

(defun shell-file-set-remote-host (host)
  "Set the remote host for shell-file execution."
  (interactive "sEnter remote host (user@host): ")
  (setq shell-file-remote-host (if (string-empty-p host) nil host))
  (message "Remote host set to %s" (or shell-file-remote-host "nil (local execution)")))

(defun shell-file-visit-outputs ()
  (interactive)
  (dired shell-file-dir)
  (dired-sort-other "-lt")  ;; Sort by time, newest first.
  (goto-char (point-min))
  (forward-line))

(defun shell-file-define-global-keys (map key-prefix)
  "add shell-file keybindings that should be available globally"
  (dolist
      (binding
       '((shell-file-find "f" "\C-f")
         (shell-file-insert-block "i" "\C-i")
         (shell-file-insert-file "x" "\C-x")
         (shell-file-insert-cd "g" "\C-g")
         (shell-file-run "r" "\C-r")
         (shell-file-set-remote-host "h" "\C-h")
         (shell-file-visit-outputs "o" "\C-o")))
    (let* ((def (car binding))
           (keys (cdr binding)))
      (dolist (key keys)
        (define-key map (concat key-prefix key) def)))))

(defun define-shell-file-mode-key (key-prefix key def)
  (let ((key (concat key-prefix key)))
    (if (featurep 'evil)
        (dolist (state (list 'normal 'motion))
          (evil-define-key state shell-file-mode-map key def))
      (define-key shell-file-mode-map key def))))

(defun shell-file-define-minor-mode-keys (key-prefix)
  "add shell-file keybindings that should be available in shell-file-mode"
  (dolist
      (binding
       '((shell-file-bubble-block "b" "\C-b")
         (shell-file-delete-block "d" "\C-d")
         (shell-file-next-block "j" "n" "\C-j" "\C-n")
         (shell-file-prev-block "k" "p" "\C-k" "\C-p")
         (shell-file-split-block "s" "\C-s")))
    (let* ((def (car binding))
           (keys (cdr binding)))
      (dolist (key keys)
        (define-shell-file-mode-key key-prefix key def)))))

(provide 'shell-file)
;;; shell-file ends here
