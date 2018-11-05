;; this is my custom eshell-mode
;; main for show eshell at bottom for windows
;; author: stanhe
;; date: 2018-10-27

;;;; Usage:

;; just bind fast-eshell-pop and eshell-pop-toggle to your key-bindings
;;
;; eshell-pop-toggle:
;;      pop-eshell as side window,much like some IDE.
;; fast-eshell-pop:
;;      open eshell with project mode,open git or gradle project.

;;;; Change log:

;; 2018/11/5
;;     * Improve docs.
;;
;; 2018/11/1
;;     * Expose var for search parent directory,see `pop-find-parent-directory'.
;;
;; 2018/10/31
;;     * Add feature mult windows support,jump between mult windows.
;;     * Add prefix arg to full screen eshell to locate current file directory.
;;
;; 2018/10/31
;;     * First released.



;;define var
(defvar my-eshell " *BOTTOM-TERMINAL*" "my shell name,use eshell.")
(defvar my-full-eshell " *FULL-TERMINAL*" "my full shell name,use eshell.")

(defvar pop-find-parent-directory nil "find the files in parent directory,if find,return the path as parent directory,else try the next")

(defvar pre-buffer nil "previous buffer.")
(defvar pre-path nil "previous directory.")
(defvar pre-parent-path nil "previous parent directory.")

;;functions
(defun get-current-directory (&optional buffer)
  "get current directory."
  (if buffer
      (with-current-buffer buffer
	(file-name-directory (or (buffer-file-name) default-directory)))
    (file-name-directory (or (buffer-file-name) default-directory))))

(defun get-parent-dir (name)
  "Get the parent name dir."
  (locate-dominating-file default-directory name))

(defun get-project-root-directory (buffer)
  "find current project root,set by `pop-find-parent-directory',
this function will check the parent directory for special file in order. "
    (with-current-buffer buffer
      (if (setq parent (cl-some #'get-parent-dir pop-find-parent-directory))
	  parent
	(get-current-directory))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; side window bottom ;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun eshell-pop-bottom()
  "pop eshell at bottom"
  (interactive)
  (let ((pos-buffer (current-buffer))
	(tmp-eshell (get-buffer my-eshell))
	(dir (get-current-directory)))
    ;;check if my-eshell exist,if not create one.
    (unless tmp-eshell
      (setq tmp-eshell (eshell 100))
      (with-current-buffer tmp-eshell
	(eshell/clear-scrollback)
	(rename-buffer my-eshell)
	(switch-to-buffer pos-buffer)))
    (setq window
	  (select-window
	   (display-buffer-in-side-window tmp-eshell '((side . bottom))) t))
    (set-window-dedicated-p window t)
    (when (not (equal pre-path dir))
      (eshell/cd dir)
      (eshell-send-input)
      (setq pre-path dir))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; full window ;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;###autoload
(defun fast-eshell-pop (&optional pre-arg)
  "fast jump between current buffer and full screen eshell buffer,
with any pre-arg locate to current buffer directory,
otherwise to the parent directory,set by `pop-find-parent-directory' "
  (interactive "p")
  (let* ((buffer (current-buffer))
	 (shell (get-buffer my-full-eshell))
	 (dir (if current-prefix-arg
		  (get-current-directory buffer)
		(get-project-root-directory buffer))))
    ;;check if my-full-eshell exist,if not create one.
    (unless shell
      (setq shell (eshell 101))
      (with-current-buffer shell
	(eshell/clear-scrollback)
	(rename-buffer my-full-eshell)))
    ;;check and handle swap.
    (if (equal my-full-eshell (buffer-name buffer))
	(progn
	  (if (setq pre-window (get-buffer-window pre-buffer 'A))
	      (select-window pre-window)
	    (switch-to-buffer pre-buffer)))
      (progn
	(if (setq exist-window (get-buffer-window my-full-eshell 'A))
	    (select-window exist-window)
	  (switch-to-buffer shell))
	(setq pre-buffer buffer)
	(when (not (equal pre-parent-path dir))
	  (eshell/cd dir)
	  (eshell-send-input)
	  (setq pre-parent-path dir))))))

;; keymaps
(defvar pop-eshell-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-e C-f") 'fast-eshell-pop)
    (define-key map (kbd "C-c C-e C-e") 'eshell-pop-toggle)
  map)
  "pop eshell mode key maps.")

;;;###autoload
(defun eshell-pop-toggle ()
  "pop or hide bottom eshell side window."
  (interactive)
  (if (get-buffer-window my-eshell)
      (delete-windows-on my-eshell)
    (eshell-pop-bottom)))

;;;###autoload
(define-minor-mode pop-eshell-mode "my pop eshell mode"
  :global t)

(provide 'pop-eshell-mode)
;;; my pop-eshell-mode end.
