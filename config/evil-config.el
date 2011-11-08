(add-to-list 'load-path "~/.emacs.d/external/evil")
(require 'evil)
(evil-mode 1)
;;(require 'surround)
;;(global-surround-mode 1)
;; --------------------------------------
;; evil variables
;; --------------------------------------
(setq evil-shift-width 2)
(setq evil-visual-state-cursor 'hbar)

(defun yank-file-name-no-path ()
  (interactive)
  (kill-new (buffer-name)))
;; --------------------------------------
;; general navigation keymaps   in insert mode
;; --------------------------------------
;; (setq evil-toggle-key "C-M-z")
(define-key evil-insert-state-map (kbd "C-p") 'previous-line)
(define-key evil-insert-state-map (kbd "C-n") 'next-line)
(define-key evil-insert-state-map (kbd "C-e") 'end-of-line)
(define-key evil-insert-state-map (kbd "C-k") 'evil-delete-line)
(define-key evil-insert-state-map (kbd "C-w") 'cdip-backward-kill-word)
(define-key evil-normal-state-map (kbd "gb") 'electric-buffer-list)
(define-key evil-normal-state-map (kbd "gB") (lambda () (interactive) (switch-to-buffer nil)))
(define-key evil-normal-state-map (kbd "gm") 'bookmark-jump)
(define-key evil-normal-state-map (kbd "!") 'shell-command)
(define-key evil-normal-state-map (kbd "gf") 'find-file)
(define-key evil-normal-state-map (kbd "gK") 'kill-buffer)
(define-key evil-normal-state-map (kbd "go") 'other-window)
(define-key evil-normal-state-map (kbd "gy") 'yank-file-name-no-path)
(define-key evil-normal-state-map (kbd "<delete>") 'evil-delete-char)
;; --------------------------------------
;; mode that starts in emacs-state
;; --------------------------------------
;;(unless (memq 'org-toc-mode evil-emacs-state-modes)
;;  (add-to-list 'evil-emacs-state-modes 'org-toc-mode))
;; --------------------------------------
;;(require 'python-mode)
;;(evil-define-key 'normal py-mode-map
;;  "zm" 'hide-body
;;  "zr" 'show-all
;;  "zo" 'show-entry
;;  "zc" 'hide-entry)
;; --------------------------------------
;; org-mode compatiablity
;; --------------------------------------
;;(require 'org)
;;(evil-define-key 'normal org-mode-map
;;  (kbd "<tab>") 'org-cycle)
;;
;;(require 'org-toc)
;;(define-key org-toc-mode-map (kbd "j") 'org-toc-next)
;;(define-key org-toc-mode-map (kbd "k") 'org-toc-previous)
;; --------------------------------------
;; for electric-buffer-list compatiablity
;; --------------------------------------
;;(require 'ibuffer)
;;(define-key ibuffer-mode-map "j" 'ibuffer-forward-line)
;;(define-key ibuffer-mode-map "k" 'ibuffer-backward-line)
;; (evil-define-key 'normal ibuffer-mode-map
;;   (kbd "RET") 'Electric-buffer-menu-select
;;   "q" 'Electric-buffer-menu-quit
;;   "d" 'Buffer-menu-delete
;;   "u" 'Buffer-menu-unmark
;;   (kbd "<backspace>") 'Buffer-menu-backup-unmark)

;; --------------------------------------
;; for info mode compatiablity
;; --------------------------------------
;;(require 'info)

;;(evil-define-key 'motion Info-mode-map
;;  (kbd "n") 'Info-next
;;  (kbd "p") 'Info-prev
;;  (kbd "u") 'Info-up
;;  (kbd "f") 'Info-follow-reference
;;  (kbd "b") 'Info-history-back)

;; --------------------------------------
;; for cscope compatiablity
;; --------------------------------------
;;(require 'xcscope)
;;(define-key evil-normal-state-map (kbd "M-.") 'cscope-find-global-definition)
;;(define-key evil-normal-state-map (kbd "M-/") 'cscope-pop-mark)
;;(define-key cscope-list-entry-keymap "j" 'next-logical-line)
;;(define-key cscope-list-entry-keymap "k" 'previous-logical-line)
;;(define-key cscope-list-entry-keymap "q"
;;  (lambda () (interactive) (bury-buffer) (other-window 1) (delete-other-windows)))

;;(evil-define-key 'normal cscope-list-entry-keymap
;;  "q" (lambda () (interactive) (bury-buffer) (other-window 1) (delete-other-windows)))
  


;; --------------------------------------
;; for general compatiablity
;; --------------------------------------
;;(define-key evil-normal-state-map (kbd "<tab>") 'indent-for-tab-command)
;;(define-key evil-normal-state-map (kbd "C-t") 'goto-paired-paren)
;;(define-key evil-insert-state-map (kbd "C-t") 'goto-paired-paren)
;; C-r was evil-paste-from-register, which I seldom use.
(define-key evil-insert-state-map (kbd "C-r") 'undo-tree-redo)

;; --------------------------------------
;; for toggle chinese input method
;; --------------------------------------
(define-key evil-normal-state-map (kbd "C-SPC") nil)


;; --------------------------------------
;; customize mode-line face
;; --------------------------------------
(defun evil-refresh-mode-line (&optional state)
  "Refresh mode line tag."
  (let (name next string temp)
    (setq string (and state (symbol-value
                             (evil-state-property state :tag)))
          name (evil-state-property state :name))
    ;; add tooltip
    (when (stringp string)
      (setq string
            (propertize string
                        'help-echo name
                        'face 'mode-line-highlight
                        'mouse-face 'mode-line-highlight)))
    (setq evil-mode-line-tag string)
    ;; refresh mode line data structure
    (when (or (null evil-local-mode)
              (null state)
              (not (eq evil-mode-line-format 'before)))
      (setq mode-line-position
            (delq 'evil-mode-line-tag mode-line-position)))
    (when (or (null evil-local-mode)
              (null state)
              (not (eq evil-mode-line-format 'after)))
      (while global-mode-string
        (setq next (pop global-mode-string))
        (if (eq next 'evil-mode-line-tag)
            (pop temp) ; remove the ""
          (push next temp)))
      (setq global-mode-string (nreverse temp)))
    (when evil-local-mode
      (when (eq evil-mode-line-format 'before)
        (add-to-list 'mode-line-position 'evil-mode-line-tag t 'eq))
      (when (eq evil-mode-line-format 'after)
        (unless (memq 'evil-mode-line-tag global-mode-string)
          (setq global-mode-string
                (nconc global-mode-string '("" evil-mode-line-tag))))))
    (force-mode-line-update)))

(byte-compile 'evil-refresh-mode-line)
              
;; (defvar eim-in-use nil
;;   "buffer-local variable to keep track of whether eim is on
;; when toggle between eim normal/insert stats.")

;; (make-local-variable 'eim-in-use)
  
;; (defun my-evil-toggle-eim-hook-for-normal-and-insert ()
;;   "When switch to Normal Mode, turn eim off if it is on.
;; Latter turn it on if it was on."  
;;   (interactive)
;;   (if (string= evil-state "normal")­n
;;       (progn
;;         (setq eim-in-use (current-input-method))
;;         (setq current-input-method nil))
;;     (when (eim-in-use)
;;         (setq current-input-method (eim-in-use)))

  

;; Make jj behave like ESC in evil-insert-mode
;; (defun viper-escape-if-next-char (c)
;;   (self-insert-command 1)           
;;     (let ((next-key (read-event)))
;;       (if (= c next-key)
;;         (progn          
;;           (delete-backward-char 1)
;;             (viper-mode))
;;             (setq unread-command-events (list next-key)))))
;; (defun viper-escape-if-next-char-is-j (arg)                
;;   (interactive "p")                        
;;     (if (= arg 1)  
;;       (viper-escape-if-next-char ?j)
;;         (self-insert-command arg)))
;; (define-key viper-insert-basic-map (kbd "j") 'viper-escape-if-next-char-is-j)
