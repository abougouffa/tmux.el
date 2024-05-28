;;; tmux.el --- Interact with tmux from Emacs        -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Abdelhak Bougouffa
;; SPDX-License-Identifier: MIT

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>
;; Keywords: tools, terminals, unix

;;; Commentary:

;; This package is based on:
;; https://github.com/doomemacs/doomemacs/tree/master/modules/tools/tmux

;;; Code:


(defgroup tmux nil
  "Tmux stuff."
  :group 'tools)

;;
;; Custom variables

(defcustom tmux-project-root-function nil
  "The function to use for detecting the project root.

If nil, use the built-in `project-root'."
  :group 'tmux
  :type '(choice function (symbol nil)))

(defcustom tmux-verbose nil
  "Enable verbose mode."
  :group 'tmux
  :type 'boolean)

;;
;; Internals

(defvar tmux-last-command nil
  "The last command ran by `tmux'. Used by `tmux-rerun'.")

(defvar tmux-last-retcode nil
  "The last tmux return code.")

(defvar tmux-command-history '()
  "Command history.")

(with-eval-after-load 'savehist
  (add-to-list 'savehist-additional-variables 'tmux-command-history))

(defun tmux-project-root ()
  "Return the root directory of the current project.

Respects `tmux-project-root-function'."
  (if (fboundp tmux-project-root-function)
      (funcall tmux-project-root-function)
    (when-let ((proj (project-current))) (project-root proj))))

;;
;; Commands

;;;###autoload
(defun tmux (command &rest args)
  "Execute COMMAND with optional extra ARGS in tmux."
  (let ((bin (executable-find "tmux")))
    (unless bin
      (error "Could not find tmux executable"))
    (let* ((args (mapcar #'shell-quote-argument (delq nil args)))
           (cmdstr (format "%s %s" bin (if args (apply #'format command args) command)))
           (stdout (get-buffer-create " *tmux stdout*"))
           (stderr (get-buffer-create " *tmux stderr*")))
      (unwind-protect
          (if (zerop (setq tmux-last-retcode
                           (let ((message-log-max (and tmux-verbose message-log-max)))
                             (if tmux-verbose
                                 (shell-command cmdstr stdout stderr)
                               (with-temp-message (or (current-message) "")
                                 (shell-command cmdstr stdout stderr))))))
              (with-current-buffer stdout
                (setq tmux-last-command `(,(substring cmdstr (+ 1 (length bin))) ,@args))
                (buffer-string))
            (error "[%d] tmux $ %s (%s)" tmux-last-retcode (with-current-buffer stderr (buffer-string)) cmdstr))
        (and (kill-buffer stdout) (kill-buffer stderr))))))

;;;###autoload
(defun tmux-run (command &optional noreturn)
  "Run COMMAND in tmux.

If NORETURN is non-nil, send the commands as keypresses but do not execute them."
  (interactive (list (read-string "tmux $ " nil 'tmux-command-history) current-prefix-arg))
  (tmux (concat "send-keys C-u " (shell-quote-argument command) (unless noreturn " Enter"))))

;;;###autoload
(defun tmux-send-region (beg end &optional noreturn)
  "Send region (BEG . END) to tmux.

NORETURN, see `tmux-run'."
  (interactive (list (region-beginning) (region-end) current-prefix-arg))
  (tmux-run (string-trim (buffer-substring-no-properties beg end)) noreturn))

;;;###autoload
(defun tmux-rerun ()
  "Rerun the last command executed by `tmux' and `tmux-run'."
  (interactive)
  (unless tmux-last-command
    (user-error "No last command to run"))
  (apply #'tmux tmux-last-command))

;;;###autoload
(defun tmux-cd (&optional directory noreturn)
  "Change the pwd of the currently active tmux pane to DIRECTORY.

DIRECTORY defaults to `default-directory' if omitted, or to `tmux-project-root'
if prefix arg is non-nil.

If NORETURN is non-nil, send the cd command to tmux, but do not execute the
command."
  (interactive "D")
  (tmux-run (format "cd %S" (expand-file-name (or directory (if current-prefix-arg (tmux-project-root) default-directory)))) noreturn))

;;;###autoload
(defun tmux-cd-to-here ()
  "Call \"cd\" into `default-directory' in tmux."
  (interactive)
  (tmux-cd default-directory))

;;;###autoload
(defun tmux-cd-to-project ()
  "Call \"cd\" into the current project's root directory in tmux."
  (interactive)
  (tmux-cd (tmux-project-root)))

;;
;; Data functions

;;;###autoload
(defun tmux-list-sessions ()
  "Return the list of opened tmux sessions."
  (let ((lines (tmux "list-sessions -F '#{session_id};#{session_name};#{session_attached}'")))
    (if lines
        (cl-loop for line in (split-string lines "\n" t)
                 collect
                 (let ((sess (split-string line ";")))
                   (list (nth 0 sess)
                         :name (nth 1 sess)
                         :attached (equal (nth 2 sess) "1"))))
      (error "There are no sessions"))))

;;;###autoload
(defun tmux-list-windows (&optional session)
  "Return the list of panes in SESSION."
  (if-let* ((lines
             (tmux (format "list-windows %s -F '#{window_id};#{session_id};#{window_active};#{window_name};#{window_activity_flag}'"
                           (if session
                               (concat "-t " (shell-quote-argument (car session)))
                             "-a")))))
      (cl-loop for line in (split-string lines "\n" t)
               collect (let ((window (split-string line ";")))
                         (list (nth 0 window)
                               :session-id (nth 1 window)
                               :name (nth 3 window)
                               :active (equal (nth 2 window) "1")
                               :activity (equal (nth 4 window) "1"))))
    (error "There are no windows")))

;;;###autoload
(defun tmux-list-panes (&optional sess-or-win)
  "Return the list of panes in session or window SESS-OR-WIN."
  (if-let* ((lines
             (tmux (format "list-panes %s -F '#{pane_id};#{window_id};#{session_id};#{pane_active};#{pane_title};#{pane_current_path}'"
                           (if sess-or-win
                               (concat (if (string-prefix-p "$" (car sess-or-win)) "-s ")
                                       "-t "
                                       (shell-quote-argument (car sess-or-win)))
                             "-a")))))
      (cl-loop for line in (split-string lines "\n" t)
               collect (let ((pane (split-string line ";")))
                         (list (nth 0 pane)
                               :window-id (nth 1 pane)
                               :session-id (nth 2 pane)
                               :name (nth 4 pane)
                               :active (equal (nth 3 pane) "1")
                               :pwd (nth 5 pane))))
    (error "There are no panes")))


(provide 'tmux)
;;; tmux.el ends here
