;;; framegroups.el --- Workspaces for emacs using frames. -*- lexical-binding: t -*-

;; Author: Fox Kiester
;; URL: https://github.com/noctuid/framegroups.el
;; Created: April 6, 2018
;; Keywords: convenience, window, window-configuration, frames
;; Package-Requires: ((cl-lib "0.5"))
;; Version: 0.1

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; A workspace package that uses frames.

;; For more information see the README in the online repository.

;;; Code:
(require 'cl-lib)

;; * Settings
(defgroup framegroups nil
  "Provide commands for using frames as named workspaces."
  :group 'convenience
  :prefix "fg-")

(defcustom fg-hide-with-xdotool t
  "Whether to hide the old frame when switching to a new one.
This only works on X and requires xdotool to be installed."
  :group 'framegroups
  :type 'boolean)

(defcustom fg-switch-with-xdotool t
  "Whether to use xdotool instead of `select-frame-set-input-focus'.
This only works on X and requires xdotool to be installed."
  :group 'framegroups
  :type 'boolean)

(defcustom fg-auto-create t
  "Whether to automatically create non-existent framegroups.
If the existing frame has not been managed by this package (i.e. it does not
have a framegroup name), `fg-switch-to-frame' will use `fg-rename-frame' instead
of `fg-create-frame'."
  :group 'framegroups
  :type 'boolean)

(defcustom fg-create-hook nil
  "Hook run after creating a new frame.
Currently each function will be passed the name of the new frame. More arguments
may be added in the future, so please include &rest _ in the function argument
list."
  :type 'hook
  :group 'framegroups)

(defcustom fg-after-switch-hook nil
  "Hook run after switching to another frame with `fg-switch-to-frame'.
Currently each function will be passed the name of the new frame. More arguments
may be added in the future, so please include &rest _ in the function argument
list."
  :type 'hook
  :group 'framegroups)

;; * Helper Functions
(defun fg--wid (&optional frame)
  "Return the window id of FRAME.
When FRAME is nil, return the window id of the current frame."
  (cdr (assq 'outer-window-id (frame-parameters frame))))

(defun fg--name (&optional frame)
  "Return the framegroup name of FRAME.
When FRAME is nil, return the  name of the current frame."
  (cdr (assq 'fg-name (frame-parameters frame))))

;; used as exists-p as well
(defun fg--frame-names ()
  "Return all existing framegroup names."
  (delq nil (cl-loop for frame in (frame-list)
                     collect (fg--name frame))))

(defvar fg--last-name nil
  "Holds the name of the last framegroup.")

(defun fg--save-last ()
  "Stor the current framegroup name."
  (let ((current-name (fg--name)))
    (when current-name
      (setq fg--last-name current-name))))

(defun fg--get-frame (name)
  "Return the frame with the framegroup name NAME or nil."
  (cl-dolist (frame (frame-list))
    (when (string= (fg--name frame) name)
      (cl-return frame))))

(defun fg--use-xdotool-to-hide-p ()
  "Return whether xdotool should be used to unmap frames."
  (and fg-hide-with-xdotool
       (eq window-system 'x)
       (executable-find "xdotool")))

(defun fg--use-xdotool-to-switch-p ()
  "Return whether xdotool should be used to activate/switch frames."
  (and fg-switch-with-xdotool
       (eq window-system 'x)
       (executable-find "xdotool")))

;; * Commands
;;;###autoload
(defun fg-rename-frame (name)
  "Rename the current frame's framegroup name to NAME."
  (interactive (list (read-string "Name: ")))
  (set-frame-parameter nil 'fg-name name)
  (run-hook-with-args 'fg-create-hook name))

;;;###autoload
(defun fg-create-frame (name &optional background)
  "Create and return new frame with the framegroup name NAME.
When BACKGROUND is non-nil unmap the newly created frame with xdotool."
  (interactive (list (read-string "Name: ")))
  (when (fg--get-frame name)
    (error "Framegroup %s already exists" name))
  (fg--save-last)
  (when (and (fg--use-xdotool-to-hide-p)
             (not background))
    (start-process "fg-switch" nil "xdotool" "windowunmap" (fg--wid)))
  (let ((frame (make-frame (list (cons 'fg-name name)))))
    (when (and (fg--use-xdotool-to-hide-p)
               background)
      (start-process "fg-bg" nil "xdotool" "windowunmap" (fg--wid frame)))
    (with-selected-frame frame
      (run-hook-with-args 'fg-create-hook name))
    frame))

;;;###autoload
(defun fg-switch-to-frame (name)
  "Swith to the frame with the framegroup name NAME."
  (interactive (list (completing-read
                      "Switch to frame: "
                      (delete (fg--name) (fg--frame-names)))))
  (when (string= name (fg--name))
    (error "Already on framegroup %s" name))
  (let* ((frame (fg--get-frame name))
         (old-wid (fg--wid))
         (target-wid (fg--wid frame)))
    (cond
     (frame
      (fg--save-last)
      (cond ((fg--use-xdotool-to-hide-p)
             (start-process "fg-switch" nil "xdotool" "windowunmap" old-wid
                            "windowmap" target-wid "windowactivate" target-wid))
            ((fg--use-xdotool-to-switch-p)
             (start-process "fg-switch" nil "xdotool" "windowmap" target-wid
                            "windowactivate" target-wid))
            (t
             (select-frame-set-input-focus frame)))
      (with-selected-frame frame
        (run-hook-with-args 'fg-after-switch-hook (fg--name frame))))
     (t
      (unless fg-auto-create
        (error "Framegroup %s does not exist" name))
      (if (fg--name)
          (fg-create-frame name)
        (fg-rename-frame name))))))

;;;###autoload
(defun fg-switch-to-last-frame ()
  "Switch to the previously focused framegroup frame."
  (interactive)
  (if fg--last-name
      (fg-switch-to-frame fg--last-name)
    (error "No last frame")))

;;;###autoload
(defmacro fg-switch (name)
  "Create and return a command to switch to the framegroup named NAME."
  (let ((func-name (intern (concat "fg-switch-to-frame-" name))))
    `(progn
       (defun ,func-name ()
         ,(format "Switch to the framegroup named %s." name)
         (interactive)
         (fg-switch-to-frame ,name))
       #',func-name)))

;; * Modeline Integration
;;;###autoload
(defun fg-mode-line-string ()
  "Return the framegroup name formatted for the mode line."
  (let ((name (fg--name)))
    (when name
      (format "(fg: %s) " (fg--name)))))

;; * Desktop Integration
;;;###autoload
(defun fg-unmap-other-frames ()
  "Unmap all frames besides the selected one."
  (when (fg--use-xdotool-to-hide)
    (let ((inhibit-redisplay t))
      (dolist (frame (frame-list))
        (unless (eq frame (selected-frame))
          (start-process "fg-unmap" nil "xdotool" "windowunmap"
                         (fg--wid frame)))))))

;;;###autoload
(defun fg-desktop-setup (&optional undo)
  "Unmap all but the last focused frame when restoring session with desktop.el"
  (if undo
      (remove-hook 'desktop-after-read-hook #'fg-unmap-other-frames)
    (add-hook 'desktop-after-read-hook #'fg-unmap-other-frames)))

(provide 'framegroups)
;;; framegroups.el ends here
