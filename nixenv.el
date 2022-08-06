;; -*- lexical-binding: t -*-

;;;###autoload
(defvar ebn/nix-env--dirs nil)

(defun ebn/nix-env--dir-lookup (proj)
  (assoc-default proj ebn/nix-env--dirs))

(defun ebn/nix-env--push (proj path)
  (car (push
	`(,proj . ,(append exec-path path))
	ebn/nix-env--dirs)))

(defun ebn/nix-env-process-sentinel ()
  nil)

(defconst ebn/nix-env--output-buffer-name "*nix-env-output*")

(defun ebn/nix-env--load (project done)
  (with-current-buffer
      (progn (when (get-buffer ebn/nix-env--output-buffer-name)
	       (kill-buffer ebn/nix-env--output-buffer-name))
	     (get-buffer-create ebn/nix-env--output-buffer-name))
    (make-process
     :buffer ebn/nix-env--output-buffer-name
     :name "*nix-env*"
     :connection-type 'pipe
     :command '("nix-shell" "--run" "echo $PATH")
     :sentinel (lambda (proc evt)
		 (funcall done (ebn/nix-env--push
				project
				(with-current-buffer (get-buffer ebn/nix-env--output-buffer-name)
				  (split-string (buffer-substring-no-properties (point-min) (point-max))
						"[\:]"))))

		 (message "Nix env loaded for %s!" project)))))

(defun ebn/nix-env-load ()
  (interactive)
  (when-let ((project (project-root (project-current))))
    (if (file-exists-p (format "%sshell.nix" project))
	(progn 
	  (with-current-buffer (current-buffer)
	    (if-let ((path (ebn/nix-env--dir-lookup project)))
		(setq-local exec-path path)
	      (setq ebn/nix-env--project project)
	      (ebn/nix-env--load project (lambda (path) (setq-local exec-path path)))))))))

