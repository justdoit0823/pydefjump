;; A fast python def jump plugin in emacs.

;; Author: justoit

;; Email: justdoit920823@gmail.com

;; This project's homepage is at https://github.com/justdoit0823/pydefjump


(require 'epc)


(defvar jump-exec-file (expand-file-name "~/.emacs.d/pydefjump/py_def_list_server.py"))


(defvar jump-python-execute "python2.7")


(defun jump-set-exec-file (filename)
  "set jump executable file"
  (setq jump-exec-file filename))


(defun jump-set-python-execute (python-execute)
  "set jump python executable"
  (setq jump-python-execute python-execute))


(defun jump-start-epc ()
  "start jump epc server"
  (epc:start-epc jump-python-execute (list jump-exec-file)))


(defun jump-stop-epc ()
  "stop jump epc server"
  (epc:stop-epc jump-epc))


(defun jump-reset-epc ()
  (jump-stop-epc)
  (setq jump-epc (jump-start-epc)))


(defun jump-call (method args)
  "epc call wrapper"
  (when (not (epc:live-p jump-epc)) (jump-reset-epc))
  (epc:call-sync jump-epc method args))


(defvar jump-epc
  (jump-start-epc))


(defun jump-to-def ()
  "fast way to jump to def position in python"
  (interactive)
  (if (not (buffer-file-name))
      (message "buffer no file name")
    (if (not (equal (file-name-extension (buffer-file-name)) "py"))
	(message "not python file")
      (let ((def-keys (jump-call 'get_file_def_pos (list (buffer-file-name) nil)))
	    )
	(setq-local def-name (completing-read "def name: " def-keys))
	(setq-local def-pos (jump-call 'get_file_def_pos (list (buffer-file-name) def-name)))
	(goto-line (car def-pos))
	(move-to-column (nth 1 def-pos))))))


(defun jump-refresh-def ()
  "refresh jump def pos when buffer post"
  (jump-call 'refresh_file_def_pos (list (buffer-file-name))))


(defun jump-refresh-def-wrap ()
  "refresh jump def pos command"
  (interactive)
  (message "refresh current buffer file def")
  (jump-refresh-def))


(defun jump-python ()
  "jump hook in python"
  (add-hook 'after-save-hook 'jump-refresh-def t t)
  (local-set-key (kbd "C-c r") 'jump-refresh-def-wrap)
  (local-set-key (kbd "C-c d") 'jump-to-def))


(defun jump-python-switch ()
  "switch jump python version between 2 and 3"
  (interactive)
  (if (string-equal jump-python-execute "python2.7")
      (progn
	(setq-local switch-msg "jump python switch to python 3")
	(setq-local pexecute "python3"))
    (progn (setq-local switch-msg "jump python switch to python 2")
	   (setq-local pexecute "python2.7")))
  (jump-set-python-execute pexecute)
  (jump-reset-epc)
  (message switch-msg))


(provide 'pydefjump)
