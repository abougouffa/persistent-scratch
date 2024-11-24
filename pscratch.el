;;; pscratch.el --- Persistent per-project scratch buffers -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Abdelhak Bougouffa
;;
;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Maintainer: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: Decembre 2, 2022
;; Modified: November 24, 2024
;; Version: 0.0.1
;; Keywords: convenience files
;; Homepage: https://github.com/abougouffa/persistent-scratch
;; Package-Requires: ((emacs "28.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Persistent per-project scratch buffers This package has started as a
;; re-implementation of a feature I've seen in Doom Emacs. Some of the code is
;; inspired by Doom's implementation of the persistent buffers.
;;
;;; Code:

(require 'cl-lib)
(autoload 'project-root "project")
(autoload 'project-prompt-project-dir "project")


(defgroup pscratch nil
  "Persistent per-project scratch buffers."
  :group 'convenience)


;;; Customs

(defcustom pscratch-initial-major-mode nil
  "What major mode to start fresh scratch buffers in.

Scratch buffers preserve their last major mode, however, so this only affects
the first, fresh scratch buffer you create. This accepts:

  t           Inherits the major mode of the last buffer you had selected.
  nil         Uses `fundamental-mode'.
  MAJOR-MODE  Any major mode symbol."
  :type '(choice (const t) (const nil) symbol)
  :group 'pscratch)

(defcustom pscratch-dir (locate-user-emacs-file "pscratch/")
  "Where to save persistent scratch buffers."
  :type 'directory
  :group 'pscratch)

(defcustom pscratch-default-file "__default"
  "The default file name for the top-level scratch buffer."
  :type 'string
  :group 'pscratch)

(defcustom pscratch-buffer-created-hook nil
  "The hooks to run after a scratch buffer is created."
  :type '(repeat function)
  :group 'pscratch)


;;; Helpers

(defalias 'pscratch--project-name
  (if (fboundp 'project-name)
      #'project-name
    (lambda (project)
      (file-name-nondirectory (directory-file-name (project-root project))))))


;;; Internals

(defvar-local pscratch-current-project nil "The name of the project associated with the current scratch buffer.")
(put 'pscratch-current-project 'permanent-local t)

(defvar pscratch--buffers nil "A list of active scratch buffers.")

(defun pscratch-restore (&optional proj-name)
  "Restore PROJ-NAME's persistent buffer, default to `pscratch-default-file'."
  (setq pscratch-current-project (or proj-name pscratch-default-file))
  (let ((scratch-file (expand-file-name (concat pscratch-current-project ".pscratch") pscratch-dir)))
    (make-directory pscratch-dir t)
    (when (file-readable-p scratch-file)
      (let ((inhibit-message t))
        (message "Reading persistent scratch from %s" scratch-file))
      (cl-destructuring-bind (content point mode)
          (with-temp-buffer
            (save-excursion (insert-file-contents scratch-file))
            (read (current-buffer)))
        (erase-buffer)
        (funcall mode)
        (insert content)
        (goto-char point)
        t))))

(defun pscratch-get-buffer (&optional dont-restore-p mode directory proj-name)
  "Return a scratchpad buffer in major MODE.

When DONT-RESTORE-P, do not load the previously saved persistent buffer. Load
persistent buffer dedicated to PROJ-NAME when provided.

When provided, set the `default-directory' to DIRECTORY."
  (let* ((buff-name (if proj-name (format "*pscratch:%s*" proj-name) "*pscratch*"))
         (pscratch-buff (get-buffer buff-name)))
    (with-current-buffer (or pscratch-buff (get-buffer-create buff-name))
      (setq-local default-directory (or directory default-directory)
                  so-long--inhibited t)
      (if dont-restore-p
          (erase-buffer)
        (unless pscratch-buff
          (pscratch-restore proj-name)
          (when (and (eq major-mode 'fundamental-mode) (functionp mode))
            (funcall mode))))
      (cl-pushnew (current-buffer) pscratch--buffers)
      (add-hook 'kill-buffer-hook #'pscratch-persist-buffer nil 'local)
      (run-hooks 'pscratch-buffer-created-hook)
      (current-buffer))))

(defun pscratch-persist-buffer (&optional buffer)
  "Save BUFFER to `pscratch-dir'."
  (with-current-buffer (or buffer (current-buffer))
    (let ((content (buffer-substring-no-properties (point-min) (point-max)))
          (curr-point (point))
          (mode major-mode))
      (with-temp-file (expand-file-name (concat (or pscratch-current-project pscratch-default-file) ".pscratch") pscratch-dir)
        (prin1 (list content curr-point mode) (current-buffer))))))

(defun pscratch-save-all-buffers (&rest _args)
  "Save all scratch buffers to `pscratch-dir'."
  (setq pscratch--buffers (cl-delete-if-not #'buffer-live-p pscratch--buffers))
  (mapc #'pscratch-persist-buffer pscratch--buffers))

;;;###autoload
(defun pscratch-setup ()
  "Setup the persistent buffers integration."
  (unless noninteractive
    ;; Persist scratch buffers on some hooks
    (add-hook 'kill-emacs-hook #'pscratch-save-all-buffers)
    (add-hook 'window-buffer-change-functions #'pscratch-save-all-buffers)
    (add-hook 'server-visit-hook #'pscratch-save-all-buffers)
    (add-hook 'window-selection-change-functions #'pscratch-save-all-buffers)))

;; Setup the package when loaded
(pscratch-setup)


;;; Commands

;;;###autoload
(defun pscratch-buffer (&optional discard project same-window)
  "Pop up a persistent scratch buffer.

When DISCARD is non-nil, do not restore the last scratch buffer.

If PROJECT is non-nil, open a persistent scratch buffer associated with the
current project.

When SAME-WINDOW is non-nil, open in the current window."
  (interactive)
  (let ((discard (or discard
                     (when current-prefix-arg
                       (y-or-n-p "Discard the previously saved content if any?"))))
        (project (or project
                     (when current-prefix-arg
                       (project-current nil (project-prompt-project-dir "Create a persistent scratch for the project")))))
        (same-window (or same-window
                         (when current-prefix-arg
                           (y-or-n-p "Open in the same window?")))))
    (funcall
     (if same-window #'switch-to-buffer #'pop-to-buffer)
     (pscratch-get-buffer
      discard
      (cond ((eq pscratch-initial-major-mode t)
             (unless (or buffer-read-only ;; not a read-only buffer
                         (derived-mode-p 'special-mode) ;; not in some sort of special mode (view only)
                         (string-match-p "^ ?\\*" (buffer-name))) ;; not a hidden buffer
               major-mode))
            ((symbolp pscratch-initial-major-mode)
             pscratch-initial-major-mode))
      (and project (project-root project))
      (and project (pscratch--project-name project))))))

;;;###autoload
(defun pscratch-project-buffer (&optional discard same-window)
  "Opens the project's persistent scratch buffer.

When DISCARD is non-nil, do not restore the last scratch buffer.
When SAME-WINDOW is non-nil, open in the same window.

When invoked with \\[universal-argument], interactively ask about the parameters."
  (interactive)
  (let ((discard (or discard (when current-prefix-arg
                               (y-or-n-p "Discard the previously saved content if any?"))))
        (same-window (or same-window (when current-prefix-arg
                                       (y-or-n-p "Open in the same window?")))))
    (pscratch-buffer discard 'project same-window)))

(defun pscratch-revert-scratch-buffer ()
  "Revert scratch buffer to last persistent state."
  (interactive)
  (if (memq (current-buffer) pscratch--buffers)
      (when (pscratch-restore pscratch-current-project)
        (message "Reloaded scratch buffer"))
    (user-error "Not in a scratch buffer")))

(defun pscratch-delete-persistent-scratch-file (&optional delete-all)
  "Deletes a scratch buffer file in `pscratch-dir'.

When DELETE-ALL (\\[universal-argument]) is non-nil, delete all persistent scratches."
  (interactive "P")
  (if delete-all
      (when (y-or-n-p (format "Are you sure to delete all saved scratch files under %S?"
                              (abbreviate-file-name pscratch-dir)))
        (mapc #'delete-file
              (file-expand-wildcards (concat (file-name-as-directory pscratch-dir) "*.pscratch")))
        (message "Cleared %S" (abbreviate-file-name pscratch-dir)))
    (make-directory pscratch-dir t)
    (let ((file (read-file-name "Delete scratch file > " pscratch-dir "scratch")))
      (if (not (file-exists-p file))
          (message "%S does not exist" (abbreviate-file-name file))
        (delete-file file)
        (message "Successfully deleted %S" (abbreviate-file-name file))))))

;;;###autoload
(define-minor-mode pscratch-mode
  "Override the default *scratch* with a persistent scratch.

This requires Emacs 29+."
  :global t
  (if pscratch-mode
      (if (< emacs-major-version 29)
          (setq pscratch-mode nil) ; Don't enable
        (advice-add 'scratch-buffer :override #'pscratch-buffer)
        (advice-add 'get-scratch-buffer-create :override #'pscratch-get-buffer))
    (advice-remove 'scratch-buffer #'pscratch-buffer)
    (advice-remove 'get-scratch-buffer-create #'pscratch-get-buffer)))


(provide 'pscratch)
;;; pscratch.el ends here
