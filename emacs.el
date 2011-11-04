;;;; # Including path

(add-to-list 'load-path "~/.emacs.d/config/")


;;;; # Color theme #

(add-to-list 'load-path "~/.emacs.d/color-theme/")
(require 'color-theme)
(color-theme-initialize)
(color-theme-clarity)


;;;; # General key mapping #

(add-to-list 'load-path "~/.emacs.d/highlight-symbol/")
(require 'highlight-symbol)
(global-set-key (kbd "C-<f3>") 'highlight-symbol-at-point)
(global-set-key (kbd "<f3>") 'highlight-symbol-next)
(global-set-key (kbd "S-<f3>") 'highlight-symbol-prev)
(global-set-key (kbd "M-<f3>") 'highlight-symbol-prev)

(global-set-key (kbd "C-x C-b") 'electric-buffer-list)
(global-set-key (kbd "C-z") 'set-mark-command)

(global-set-key (kbd "<f9>") 'view-mode)
(global-set-key (kbd "C-c C-v") 'view-mode)


;;;; # System-based detecting functions #

;;; Check if system is Darwin/Mac OS X
(defun system-type-is-darwin ()
  (interactive)
  "Return true if system is darwin-based (Mac OS X)"
  (string-equal system-type "darwin")
  )

;;; Check if system is GNU/Linux
(defun system-type-is-gnu ()
  (interactive)
  "Return true if system is GNU/Linux-based"
  (string-equal system-type "gnu/linux")
  )


;;;; # UI settings #
(global-font-lock-mode 1) ; always syntax highlighting
;;(menu-bar-mode -1)        ; do not use menu bar
(column-number-mode 1)    ; line number
(show-paren-mode 1)       ; parenthese pairing

;; ## for OSX only ##
(if (system-type-is-darwin)
    (progn
      (menu-bar-mode -1)                 ; do not use menu bar
      (setq mac-allow-anti-aliasing nil) ; do not use anti-aliasing fonts
      (if window-system
          (progn
            ;; fringe
            (set-fringe-style "default")
            (set-face-background 'fringe "gray5")
            )
      )
  )

;; for Linux only
(if (system-type-is-gnu)
    (progn
      ;;(menu-bar-mode -1) ; use menu bar
      (setq window-system-default-frame-alist
            '(
              ;; if frame created on x display
              (x
               ;; mouse
               (mouse-wheel-mode . 1)
               (mouse-wheel-follow-mouse . t)
               (mouse-avoidance-mode . 'exile)
               ;; face
               (font . "Droid Sans Mono-8")
               )
              ;; if on term
              (nil
               )
              )
            )
      ;; fringe
      (set-fringe-style "default")
      (set-face-background 'fringe "gray5")
      )
  )
)


;;;; # encoding scheme #
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-selection-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
(set-next-selection-coding-system 'utf-8)


;;;; # auto backup #
(setq auto-save-default nil)
(setq delete-auto-save-file t
      auto-save-timeout 300
      ;;auto-save-interval 400
      )
(setq auto-save-mode t)
(setq version-control t)
(setq kept-old-versions 2)
(setq kept-new-versions 10)
(setq delete-old-versions t)
(setq backup-directory-alist '(("." . "~/.emacs.d/backup")))
(setq backup-by-copying t)
(setq make-backup-file nil)
(setq dired-kept-versions 1)


;;;; # clipboard #
;;(transient-mark-mode 1)           ; makes the region act quite like the text "highlight" in many apps.
(setq mouse-drag-copy-region nil)   ; stops selection with a mouse being immediately injected to the kill ring
(setq x-select-enable-primary nil)  ; stops killing/yanking interacting with primary X11 selection
(setq x-select-enable-clipboard t)  ; makes killing/yanking interact with clipboard X11 selection
;; leave the following two lines if above works fine.
(setf interprogram-cut-function 'x-select-text)
(if (system-type-is-gnu)
    (setf interprogram-paste-function 'x-cut-buffer-or-selection-value)
  )
;; Workaround: bug #902
;; it makes "highlight/middlebutton" style (X11 primary selection based) copy-paste work as expected
(setq select-active-regions t)                  ; active region sets primary X11 selection
(global-set-key [mouse-2] 'mouse-yank-primary)  ; make mouse middle-click only paste from primary X11 selection,
                                                ; not clipboard and kill ring.
;; with this, doing an M-y will also affect the X11 clipboard, making
;; emacs act as a sort of clipboard history, at least of text you've
;; pasted into it in the first place.
; (setq yank-pop-change-selection t)  ; makes rotating the kill ring change the X11 clipboard.


;;;; # misc #
(setq inhibit-startup-message t) ; skip startup screen
(icomplete-mode 1)               ; hints for M-x
(fset 'yes-or-no-p 'y-or-n-p)    ; y/n instead of yes/no
(setq track-eol t)               ; keep at line end when C-n/C-p
(global-set-key (kbd "C-<") 'undo) ; rebinde undo & redo
(global-set-key (kbd "C->") 'redo)
(setq default-major-mode 'text-mode) ; default mode for blank startup
(setq-default tab-width 4)       ; tab width
(setq-default indent-tabs-mode nil) ; disable indent with tabs as default

;; implement of redo function {
(provide 'redo)

(defvar redo-version "1.02"
  "Version number for the Redo package.")

(defvar last-buffer-undo-list nil
  "The head of buffer-undo-list at the last time an undo or redo was done.")
(make-variable-buffer-local 'last-buffer-undo-list)

(make-variable-buffer-local 'pending-undo-list)

;; Emacs 20 variable
(defvar undo-in-progress)

(defun redo (&optional count)
  "Redo the the most recent undo.
  Prefix arg COUNT means redo the COUNT most recent undos.
  If you have modified the buffer since the last redo or undo,
  then you cannot redo any undos before then."
  (interactive "*p")
  (if (eq buffer-undo-list t)
    (error "No undo information in this buffer"))
  (if (eq last-buffer-undo-list nil)
    (error "No undos to redo"))
  (or (eq last-buffer-undo-list buffer-undo-list)
      ;; skip one undo boundary and all point setting commands up
      ;; until the next undo boundary and try again.
      (let ((p buffer-undo-list))
        (and (null (car-safe p)) (setq p (cdr-safe p)))
        (while (and p (integerp (car-safe p)))
               (setq p (cdr-safe p)))
        (eq last-buffer-undo-list p))
      (error "Buffer modified since last undo/redo, cannot redo"))
  (and (or (eq buffer-undo-list pending-undo-list)
           (eq (cdr buffer-undo-list) pending-undo-list))
       (error "No further undos to redo in this buffer"))
  (or (eq (selected-window) (minibuffer-window))
      (message "Redo..."))
  (let ((modified (buffer-modified-p))
        (undo-in-progress t)
        (recent-save (recent-auto-save-p))
        (old-undo-list buffer-undo-list)
        (p (cdr buffer-undo-list))
        (records-between 0))
    ;; count the number of undo records between the head of the
    ;; undo chain and the pointer to the next change.  Note that
    ;; by `record' we mean clumps of change records, not the
    ;; boundary records.  The number of records will always be a
    ;; multiple of 2, because an undo moves the pending pointer
    ;; forward one record and prepend a record to the head of the
    ;; chain.  Thus the separation always increases by two.  When
    ;; we decrease it we will decrease it by a multiple of 2
    ;; also.
    (while p
           (cond ((eq p pending-undo-list)
                  (setq p nil))
                 ((null (car p))
                  (setq records-between (1+ records-between))
                  (setq p (cdr p)))
                 (t
                   (setq p (cdr p)))))
    ;; we're off by one if pending pointer is nil, because there
    ;; was no boundary record in front of it to count.
    (and (null pending-undo-list)
         (setq records-between (1+ records-between)))
    ;; don't allow the user to redo more undos than exist.
    ;; only half the records between the list head and the pending
    ;; pointer are undos that are a part of this command chain.
    (setq count (min (/ records-between 2) count)
          p (primitive-undo (1+ count) buffer-undo-list))
    (if (eq p old-undo-list)
      nil ;; nothing happened
      ;; set buffer-undo-list to the new undo list.  if has been
      ;; shortened by `count' records.
      (setq buffer-undo-list p)
      ;; primitive-undo returns a list without a leading undo
      ;; boundary.  add one.
      (undo-boundary)
      ;; now move the pending pointer backward in the undo list
      ;; to reflect the redo.  sure would be nice if this list
      ;; were doubly linked, but no... so we have to run down the
      ;; list from the head and stop at the right place.
      (let ((n (- records-between count)))
        (setq p (cdr old-undo-list))
        (while (and p (> n 0))
               (if (null (car p))
                 (setq n (1- n)))
               (setq p (cdr p)))
        (setq pending-undo-list p)))
    (and modified (not (buffer-modified-p))
         (delete-auto-save-file-if-necessary recent-save))
    (or (eq (selected-window) (minibuffer-window))
        (message "Redo!"))
    (setq last-buffer-undo-list buffer-undo-list)))

(defun undo (&optional arg)
  "Undo some previous changes.
  Repeat this command to undo more changes.
  A numeric argument serves as a repeat count."
  (interactive "*p")
  (let ((modified (buffer-modified-p))
        (recent-save (recent-auto-save-p)))
    (or (eq (selected-window) (minibuffer-window))
        (message "Undo..."))
    (or (eq last-buffer-undo-list buffer-undo-list)
        ;; skip one undo boundary and all point setting commands up
        ;; until the next undo boundary and try again.
        (let ((p buffer-undo-list))
          (and (null (car-safe p)) (setq p (cdr-safe p)))
          (while (and p (integerp (car-safe p)))
                 (setq p (cdr-safe p)))
          (eq last-buffer-undo-list p))
        (progn (undo-start)
               (undo-more 1)))
    (undo-more (or arg 1))
    ;; Don't specify a position in the undo record for the undo command.
    ;; Instead, undoing this should move point to where the change is.
    ;;
    ;;;; The old code for this was mad!  It deleted all set-point
    ;;;; references to the position from the whole undo list,
    ;;;; instead of just the cells from the beginning to the next
    ;;;; undo boundary.  This does what I think the other code
    ;;;; meant to do.
    (let ((list buffer-undo-list)
          (prev nil))
      (while (and list (not (null (car list))))
             (if (integerp (car list))
               (if prev
                 (setcdr prev (cdr list))
                 ;; impossible now, but maybe not in the future 
                 (setq buffer-undo-list (cdr list))))
             (setq prev list
                   list (cdr list))))
    (and modified (not (buffer-modified-p))
         (delete-auto-save-file-if-necessary recent-save)))
  (or (eq (selected-window) (minibuffer-window))
      (message "Undo!"))
  (setq last-buffer-undo-list buffer-undo-list))
; }


;;;; # Automatic format detection #

;;; Lisp family
(setq auto-mode-alist
      (append '(
                ("\\.emacs$" . emacs-lisp-mode)
                ("\\.el$" . emacs-lisp-mode)
                ("\\.lisp$" . lisp-mode)
                ("\\.lsp$" . lisp-mode)
                ("\\.cl$" . lisp-mode)
                ("\\.system$" . lisp-mode)
                ("\\.scm$" . scheme-mode)
                ("\\.ss$" . scheme-mode)
                ("\\.sch$" . scheme-mode)
                )auto-mode-alist))


;;; =======================
;;;; # 3rd party plugins #
;;; =======================

;;;; # linum Plus #
;;; {
;;; This plugin shows the line number.
(if window-system
    (progn
      (add-to-list 'load-path "~/.emacs.d/external/linum-plus")
      (require 'linum+)
      (global-linum-mode t)
      )
  )
;;; }


;;;; # undo-tree #
;;; {
(add-to-list 'load-path "~/.emacs.d/external/undo-tree")
(require 'undo-tree)
;;; }


;;;; # Evil #
;;; {
;;; http://gitorious.org/evil/pages/Home
;;; This plugin depends on undo-tree.
;; see config/evil-config.el
(load "evil-config.el")
;;; }


;;;; # Yasnippad #
;;; {
(add-to-list 'load-path "~/.emacs.d/external/yasnippet-0.6.1c")
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/external/yasnippet-0.6.1c/snippets")
;;; }


;;;; # dtrt indent #
;;; {
;;; This plugin helps us to detect proper indent style automatically.
(add-to-list 'load-path "~/.emacs.d/external/dtrt-indent")
(require 'dtrt-indent)
(dtrt-indent-mode 1)
;;; }


;;;; # Go #
;;; {
(add-to-list 'load-path "~/.emacs.d/external/go" t)
(require 'go-mode-load)
;;; }


;;;; # Scala #
;;; {
;;; This plugin is included in official Scala repo under
;;; misc/scala-tool-support/emacs/ .
(add-to-list 'load-path "~/.emacs.d/external/scala")
(require 'scala-mode-auto)
;;; }


;;;; # ENSIME - ENhanced Scala Interaction Mode for Emacs #
;;; {
(add-to-list 'load-path "~/.emacs.d/external/ensime_2.9.1-0.7.6/elisp")
(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
;;; }


;;;; # Markdown mode #
;;; {
(add-to-list 'load-path "~/.emacs.d/external/markdown-mode")
(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t
  )
(add-to-list 'auto-mode-alist '("\\.md" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mdt" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mdwn" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown" . markdown-mode))
;; avoid key conflict for tab with Yasnippets
(defun markdown-unset-tab ()
  "markdown-mode-hook"
  (define-key markdown-mode-map (kbd "<tab>") nil))
(add-hook 'markdown-mode-hook '(lambda() (markdown-unset-tab)))
;;; }

