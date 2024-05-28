;;; tmux-evil.el --- Evil integration      -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Abdelhak Bougouffa
;; SPDX-License-Identifier: MIT

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>
;; Keywords: tools, terminals, unix

;;; Commentary:

;; This package is based on:
;; https://github.com/doomemacs/doomemacs/tree/master/modules/tools/tmux

;;; Code:

(require 'tmux)
(require 'evil)

;;
;; Evil integration

;;;###autoload (autoload 'evil-tmux-run "tmux-evil" nil t)
(evil-define-command evil-tmux-run (bang &optional command)
  (interactive "<!><fsh>")
  (if (evil-visual-state-p)
      (tmux-send-region evil-visual-beginning evil-visual-end bang)
    (tmux-run command bang)))

;;;###autoload (autoload 'evil-tmux-cd-here "tmux-evil" nil t)
(evil-define-command evil-tmux-cd-here (bang)
  (interactive "<!>")
  (if bang (tmux-cd-to-here) (tmux-cd-to-project)))


(provide 'tmux)
;;; tmux-evil.el ends here
