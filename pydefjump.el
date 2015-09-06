;; A fast python def jump plugin in emacs.

;; Author: justoit

;; Email: justdoit920823@gmail.com

;; This project's homepage is at https://github.com/justdoit0823/pydefjump



(require 'epc)


(defvar jump-exec-file (expand-file-name "~/.emacs.d/pydefjump/py_def_list_server.py"))


(defun set-jump-exec-file (filename)
  "set jump executable file"
  (setq jump-exec-file filename)
  (setq def-epc (epc:start-epc "python" (list jump-exec-file))))


(defvar def-epc
  (epc:start-epc "python" (list jump-exec-file)))


(defun jump-to-def ()
  "fast way to jump to def position in python"
  (interactive)
  (if (not (buffer-file-name))
      (message "buffer no file name")
    (if (not (equal (file-name-extension (buffer-file-name)) "py"))
	(message "not python file")
      (let ((def-keys (epc:call-sync def-epc 'get_file_def_pos (list (buffer-file-name) nil)))
	    )
	(setq-local def-name (completing-read "def name: " def-keys))
	(setq-local def-pos (epc:call-sync def-epc 'get_file_def_pos (list (buffer-file-name) def-name)))
	(goto-line (car def-pos))
	(move-to-column (nth 1 def-pos))))))


(defun jump-refresh-def ()
  "refresh jump def pos when buffer post"
  (epc:call-sync def-epc 'refresh_file_def_pos (list (buffer-file-name))))


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


(provide 'pydefjump)
