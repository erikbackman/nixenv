;;; nixenv.el --- Load nix-shell into emacs on a per project basis. -*- lexical-binding: t -*-

;; Copyright (C) 2022 Erik Bäckman

;; Author: Erik Bäckman <contact@ebackman.net>
;; Keywords: processes, tools
;; Homepage: https://github.com/erikbackman/nixenv
;; Package-Requires:
;; Package-Version: 0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

;; TODO: Parse env variables etc from declare -p instead?

;;;###autoload
(defvar nixenv--dirs nil)

(defvar-local nixenv--status 'off
  "State of the current buffers nixenv.")

(defun nixenv--dir-lookup (proj)
  (assoc-default proj nixenv--dirs))

(defun nixenv--push (proj path)
  (car (push
	`(,proj . ,(append exec-path path))
	nixenv--dirs)))

(defconst nixenv--output-buffer-name "*nixenv-output*")

(defun nixenv--load (project done)
  (with-current-buffer
      (progn (when (get-buffer nixenv--output-buffer-name)
	       (kill-buffer nixenv--output-buffer-name))
	     (get-buffer-create nixenv--output-buffer-name))
    (make-process
     :buffer nixenv--output-buffer-name
     :name "*nixenv*"
     :connection-type 'pipe
     :command '("nix-shell" "--run" "echo $PATH")
     :sentinel (lambda (proc evt)
		 (funcall done (nixenv--push
				project
				(with-current-buffer (get-buffer nixenv--output-buffer-name)
				  (split-string (buffer-substring-no-properties (point-min) (point-max))
						"[\:]"))))

		 (message "Nix env loaded for %s!" project)))))

(defun nixenv-load ()
  (interactive)
  (when-let ((project (project-root (project-current))))
    (if (file-exists-p (format "%sshell.nix" project))
	(progn 
	  (with-current-buffer (current-buffer)
	    (if-let ((path (nixenv--dir-lookup project)))
		(setq-local exec-path path)
	      (setq nixenv--project project)
	      (nixenv--load project (lambda (path) (setq-local exec-path path)))
	      (setq-local nixenv--status 'on)))))))

(defface nixenv-mode-line-on-face '((t :inherit success))
  "Face used for the nixenv-on-lighter.")

(defface nixenv-mode-line-off-face '((t :inherit warning))
  "Face used for the nixenv-off-lighter.")

(defcustom nixenv-on-lighter '(" nixenv[" (:propertize "on" face nixenv-mode-line-on-face) "]")
  "Lighter used by `nixenv-lighter' when nixenv is on."
  :type 'sexp)

(defcustom nixenv-off-lighter '(" nixenv[" (:propertize "off" face nixenv-mode-line-off-face) "]")
  "Lighter used by `nixenv-lighter' when nixenv is off."
  :type 'sexp)

(defun nixenv--lighter ()
  "Return the nixenv-lighter."
  (pcase nixenv--status
    (`on nixenv-on-lighter)
    (`off nixenv-off-lighter)))

(defcustom nixenv-lighter '(:eval (nixenv--lighter))
  "The mode line lighter for `nixenv-mode'."
  :type 'sexp)

(put 'nixenv-lighter 'risky-local-variable t)

(define-minor-mode nixenv-mode
  "Minor mode for nixenv."
  :init-value nil
  :lighter nixenv-lighter
  :after-hook (nixenv-load))
