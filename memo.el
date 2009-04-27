;;; -*- mode: Lisp-Interaction -*-
;;; Emacs��p����

;;; ����(�L�[)�̕\�����@�F�X
;;; *** ��{�̓x�N�^
?h                                      ; 104
[?h]                                    ; [104]
?\C-h                                   ; 8
"\C-h"                                  ; ""
[(f9)]                                  ; F9
(coerce "\C-h" 'vector)                 ; [8] (vconcat "\C-h")
(global-set-key [(control ?c) (control ?v)] 'ignore)
(lookup-key global-map [(control ?c) (control ?v)]) ; ignore
(kbd "<up> <up> <down> <down> <left> <right> <left> <right> b a")
;; [up up down down left right left right 98 97]
(kbd "C-m")                             ; ""
[return]                                ; "\C-m"
(kbd "C-M-<up>")                        ; [C-M-up]
[(control meta up)]
(equal "\M-\C-e" "\C-\M-e")             ; t
(kbd "C-M-d")                           ; [134217732]
(kbd "C-.")                             ; [67108910]

(every #'char-valid-p '(0 1 2 254 255)) ; t

;;; Ctrl-H ��O1�����폜�ɕύX
(keyboard-translate ?\^h ?\177)
(keyboard-translate ?\^h 'backspace)
(keyboard-translate ?\C-h ?\C-?)
(load-library "keyswap")

;;; C-h �� BS �����������ɂȂ�
(global-set-key [backspace] 'backward-delete-char)
(keyboard-translate ?\C-h 'backspace)
(global-set-key [delete] 'delete-char)

backward-delete-char-untabify-method    ; untabify/hungry/all/nil
(define-key global-map "" 'backward-delete-char-untabify) ; == "\C-h"

;; isearch
(define-key isearch-mode-map "\C-k" 'isearch-edit-string)

;; ls �̏o�͂��p��ɂ��� "$ LANG=C ls"
;; <on cygwin> ���s��� LANG ���ύX���ꂽ�܂�
'(add-hook 'dired-mode-hook
           '(lambda ()
              (make-local-variable 'process-environment)
              (setenv "LANG" "C")))

;;; EmacsHelp�̌Ăяo�� (help-command��F1������Ăяo����)
(global-set-key "\C-c\C-h" 'help-command)
(global-set-key "\M-?" 'help-command)

;;; fill
(setq paragraph-start '"^\\([ �@�E��<\t\n\f]\\|(?[0-9a-zA-Z]+)\\)")

;;; �[������N�������� (emacs -nw) �Ƀ��j���[�o�[������
(if window-system (menu-bar-mode 1) (menu-bar-mode -1))

;;; �S�p�󔒂Ƃ����s�Ƃ��^�u�������\��
(require 'jaspace)
(autoload 'jaspace-mode-on "jaspace" nil t)
(setq jaspace-mode '(c-mode))
(setq jaspace-alternate-jaspace-string "��")
(if window-system
    (setq jaspace-alternate-eol-string "\xab\n"))
(setq jaspace-highlight-tabs ?^)	; use ^ as a tab marker

;;; �s���̖��ʂȋ󔒕�����\��
(setq-default show-trailing-whitespace t)
(set-face-background 'trailing-whitespace "plum") ;;"SteelBlue")
;; (set-face-underline 'trailing-whitespace t)
(defun toggle-trailing-whitespace (&optional arg)
  (interactive)
  (setq show-trailing-whitespace (not show-trailing-whitespace))
  (font-lock-mode t))
(defun turn-off-show-trailing-whitespace ()
  (interactive)
  (setq show-trailing-whitespace nil))
(add-hook 'shell-mode-hook 'turn-off-show-trailing-whitespace)

;; mew ����q�؂���(�炵��)
(defvar cfirchat-url-regexp
  "\\b\\(s?https?\\|ftp\\|file\\|gopher\\|news\\|telnet\\|wais\\|mailto\\):\\(//[-a-zA-Z0-9_.]+:[0-9]*\\)?[-a-zA-Z0-9_=?#$@~`%&*+|\\/.,]*[-a-zA-Z0-9_=#$@~`%&*+|\\/]"
  "* URL �Ƀ}�b�`���鐳�K�\��")

;; �J�[�\���ʒu��face�𒲂ׂ�֐�
(defun describe-face-at-point()
  "Return facce used at point."
  (interactive)
  (message "%s" (get-char-property (point) 'face)))

(global-set-key "\M-g" 'goto-line)

;;; shell-mode �ł̓p�X���[�h��\�����Ȃ�
;;; �����f�t�H���g
(add-hook 'comint-output-filter-functions
	  'comint-watch-for-password-prompt)

;;; ���h�D (�A���h�D�̃A���h�D)
;;; http://www.fan.gr.jp/~ring/Meadow/elisp/redo.el
(require 'redo)
(global-set-key [?\C-_] 'redo)

;;; �}�E�X�ɔ���
(define-key global-map [mouse-4] 'scroll-down)
(define-key global-map [mouse-5] 'scroll-up)
(mwheel-install)
(setq mouse-wheel-follow-mouse t)

;; �o�b�N�A�b�v�t�@�C���̕ۑ��ꏊ���w��
;; ~/emacs_backup �f�B���N�g���ɕۑ�
(setq make-backup-files t)
(setq backup-directory-alist
      (cons (cons "\\.*$" (expand-file-name "~/emacs_backup"))
            backup-directory-alist))

;; �F�t���Ɋւ��� (22�Ȃ� 'jit-lock-mode ���f�t�H 'lazy-lock-mode)
(setq font-lock-support-mode 'fast-lock-mode)

;;; �o�b�t�@�Ԉړ�
(global-set-key [?\C-x ?\C-.] 'next-buffer)
(global-set-key [?\C-x ?\C-,] 'previous-buffer)

;;; �~����� kill-line (�󔒂����̍s�͉��s�����łɍ폜����)
(setq kill-whole-line t)

;;; �J�[�\�����O�̕������܂߂ăJ�b�g (vi��dd)
(global-set-key "\M-k" 'kill-whole-line)

;; ;;; kill-ring �̓e�L�X�g������ۑ����Ȃ��Ă��� (������)
;; (defadvice kill-new (around my-kill-ring-disable-text-property activate)
;;   (let ((new (ad-get-arg 0)))
;;     (set-text-properties 0 (length new) nil new)
;;     ad-do-it))

;;; M-x: what-cursor-position (C-x =): �J�[�\���ʒu�̏��

;;; �J�[�l���ҏW�p C-mode
(defun linux-c-mode ()
  "C mode with adjusted defaults for use with the Linux kernel"
  (interactive)
  (c-mode)
  (setq c-indent-level 8)
  (setq c-brace-imaginary-offset 0)
  (setq c-brace-offset -8)
  (setq c-argdecl-indent 8)
  (setq c-label-offset -8)
  (setq c-continued-statement-offset 8)
  (setq indent-tabs-mode nil)
  (setq tab-width 8))
(setq auto-mode-alist (cons '("/usr/src/linux.*/.*\\.[ch]$" . linux-c-mode)
                            auto-mode-alist))

;;; �K�[�x�W�R���N�g�̉񐔂����炷 (�f�t�H�� 4000000)
(setq gc-cons-threshold 5000000)

;;; mode-info --- �֐��E�ϐ��̐��������Q�Ƃ���R�}���h
;;; Emacs lisp / C / Perl / Ruby �̃}�j���A����֗���
;;; http://namazu.org/~tsuchiya/elisp/mode-info.html

;;; ���[�h���������ƒZ��
(add-hook 'emacs-lisp-mode-hook
          #'(lambda () (setq mode-name "Elisp")))

;;; �����s�ړ�
(defun previous-window-line (n)
  (interactive "p")
  (let ((cur-col
	 (- (current-column)
	    (save-excursion (vertical-motion 0) (current-column)))))
    (vertical-motion (- n))
    (move-to-column (+ (current-column) cur-col)))
  (run-hooks 'auto-line-hook))
(defun next-window-line (n)
  (interactive "p")
  (let ((cur-col
	 (- (current-column)
	    (save-excursion (vertical-motion 0) (current-column)))))
    (vertical-motion n)
    (move-to-column (+ (current-column) cur-col)))
  (run-hooks 'auto-line-hook))
(global-set-key "\C-p" 'previous-window-line)
(global-set-key "\C-n" 'next-window-line)
(global-set-key [up] 'previous-window-line)
(global-set-key [down] 'next-window-line)

;;; ���K�\�����m�F���Ȃ���쐬 (M-x: re-builder)
(require 're-builder)

;;; ���݂̊֐��������[�h���C���ɕ\��
(which-function-mode t)

;;; M-x: cusomize-apropos

;;; ����t�@�C�����̃o�b�t�@���𕪂���₷��
;;; �Е��̃o�b�t�@�������Ƃ����ʂȃo�b�t�@���c��
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-ignore-buffers-re "*[^*]+*")

;;; �J���t���� diff M-x: ediff-files
;;; ���ɊJ���Ă���t�@�C��(�o�b�t�@)���m��diff�����ɂ� M-x: ediff-buffers

(when window-system
  (global-set-key [(f9)]
                  #'(lambda ()
                      (interactive)
                      (manual-entry (current-word)))))

;;; (point) ��̒P���������
(current-word)
(thing-at-point 'word)

;;; cygwin �� man �������������Ă�������
;;;;;  </lisp/man.el>�֐� Man-getpage-in-background 744�s�ڂ�����
;; ;; (let (...
;; ;;       (coding-system-for-read
;; ;;        (if default-enable-multibyte-characters
;; ;; 	         locale-coding-system 'raw-text-unix))
;; ;;       ...))
;;; �����̊��� locale-coding-system => cp392 ������
;; (defadvice man (around man-pages-ja activate)
;;   (let ((locale-coding-system
;; 	 (read-coding-system
;; 	  (format "Coding (default %s): " 'japanese-iso-8bit)
;; 	  'japanese-iso-8bit)))
;;     ad-do-it))

;;; �悻�̃E�B���h�E�ɃJ�[�\����\�����Ȃ�
;;; ���̃��C�u�������㏑�����Ă邩��
(setq cursor-in-non-selected-windows nil)

;;; �}�E�X�̈ʒu�łȂ��A�J�[�\���̈ʒu�Ƀy�[�X�g����
(setq mouse-yank-at-point t)

;; Shift+�J�[�\���Ń��[�W�����I��
(pc-selection-mode)

;;; (Meadow�ł̓G���[) setq: Spawning child process: exec format error
;; (setq exec-suffixes '(".exe" ".sh" ".pl"))

;; �N�I�[�g�ŃR�����g�A�E�g����S���̓R�����g�̐F�ɂ���
(defun elisp-font-lock-top-quote (limit)
  (when (re-search-forward "^' *(" limit t)
    (forward-char -1)
    (set-match-data (list (point) (progn (forward-sexp 1) (point))))
    t))
(font-lock-add-keywords
 'emacs-lisp-mode
 '((elisp-font-lock-top-quote 0 font-lock-comment-face prepend)))

;; http://www.emacswiki.org/cgi-bin/emacs/download/blank-mode.el
;; http://www.emacswiki.org/elisp/show-wspace.el

;; �������[�v�ɂȂ邩������Ȃ���
(defun kitaa ()
  (interactive)
  (let ((kita-list '("߁��" " ߁�" "   �" "    " "�   " "��� " "߁��")))
    (while t
      (dolist (kao kita-list)
        (message "��������(%s)������!!!!" kao)
        (sit-for .1)))))

(system-name)                           ; "YOUR-D1BE424ADF"
system-time-locale                      ; nil
system-messages-locale                  ; nil
system-configuration                    ; "i386-mingw-nt5.1.2600"
system-type                             ; windows-nt
temporary-file-directory                ; "c:/tmp/"

(split-string (getenv "PATH") ";")      ; == exec-path

(defun windows-p ()
  (if (memq system-type '(ms-dos windows-nt)) t nil))

;; etags �g�������ƂȂ�
(defun make-tags-file (dir)
  (interactive "DMake TAGS file: ")
  (declare (ignore dir))
  (shell-command (format ;; "%s *.[ch] *.el --output=TAGS"
                  "%s *.el --output=TAGS"
                  (expand-file-name "etags.exe" exec-directory))))

;; `;' �Ƃ� `{' ����͂���ƂƎ����I�ɉ��s�����
(c-toggle-auto-newline)                 ; C-c C-a

(key-description "\C-x \M-y \n \t \r \f123")
=>"C-x SPC M-y SPC C-j SPC TAB SPC RET SPC C-l 1 2 3"

(mapcar (lambda (x)
          (list (text-char-description x) (string x)))
        '(?\C-c ?\M-m ?\C-\M-m))
=>(("^C" "") ("\xed" "\355") ("\x8d" "\x8d"))

(getenv "programfiles")                 ; "C:\\Program Files"

;; 
(defun run-slime (&optional modern)
  (interactive "P")
  (shell-command
   (format "%s -K full -q -ansi %s -i %s &"
           (expand-file-name "bin/clisp.exe" clisp-directory)
           ;; inferior-lisp-program
           (if modern "-modern" "")
           (expand-file-name ".slime.lisp" clisp-directory)))
  (sleep-for 0.5)
  (slime-connect "127.0.0.1" 4005))

;;; CLISP��������2.46�̐ݒ�
(progn
(add-to-list 'load-path "c:/usr/local/clisp-2.46-full/lib/slime")
(load-library "slime")
(slime-setup '(slime-scratch slime-fancy inferior-slime))
(setq slime-net-coding-system 'utf-8-unix) ; ���{����g������
(setq slime-lisp-implementations
      '((clisp ("C:/usr/local/clisp-2.44/clisp.exe" "-K full"
                "-Efile utf-8" "-on-error debug" "-I"))
        (olio ("c:/usr/local/clisp-2.46-full/bin/clisp.exe")
         :init;; slime-olio-init-command
         slime-init-command
         )))
(setq slime-default-lisp 'olio)
(global-set-key [(control ?c) (control ?z)] 'slime-repl)
(define-key slime-repl-mode-map [(control ?c) (control ?c)] 'slime-quit-lisp)
(require 'hyperspec)
(global-set-key "\C-cH" 'hyperspec-lookup)
;; CLISP ��������
(load-library "clisp-olio")
)

;;; SLIME Tips
;;; http://www.cliki.net/SLIME%20Tips
;; Connecting automatically
(defun cliki:start-slime ()
  (unless (slime-connected-p)
    (save-excursion (slime))))
(add-hook 'slime-mode-hook 'cliki:start-slime)

;;; terminal-coding-system �� utf-8 ��Ԃ����� utf-8 �Ȑݒ�ɂ���
(labels ((set-coding-system (c)
           (set-default-coding-systems c)
           (set-terminal-coding-system c)
           (set-keyboard-coding-system c)
           (set-buffer-file-coding-system c)
           (setq default-buffer-file-coding-system c)))
  (set-coding-system (or (cdr (assoc (terminal-coding-system)
                                     '((utf-8 . utf-8)
                                       (japanese-iso-8bit . euc-jp)
                                       )))
                         'utf-8)))

;; �����_���̔�r
(equal #'(lambda (x) (+ x x))
       #'(lambda (x) (+ x x)))          ; t


;;; @@Encoding

;;; ��ASCII���� -- GNU Emacs Lisp���t�@�����X�}�j���A��
;;; http://www.bookshelf.jp/texi/elisp-manual/21-2-8/jp/elisp_33.html

;; �G���R�[�h�֐��E�ϐ� ���Ԃ�܂�����
;; mule{,cmds}.el
mule-keymap
universal-coding-system-argument (C-x RET c)
set-buffer-file-coding-system    (C-x RET f)
set-file-name-coding-system      (C-x RET F)
set-buffer-process-coding-system (C-x RET p)
revert-buffer-with-coding-system (C-x RET r)
set-terminal-coding-system       (C-x RET t)
set-keyboard-coding-system       (C-x RET k)

(list coding-system-for-read coding-system-for-write) ; (nil nil)
locale-coding-system                                  ; cp932

(apropos "default-.*-coding-system")
default-buffer-file-coding-system                     ; japanese-shift-jis
default-file-name-coding-system                       ; japanese-shift-jis
default-keyboard-coding-system                        ; japanese-shift-jis
default-process-coding-system                         ; (japanese-shift-jis-dos . japanese-shift-jis-unix)
default-sendmail-coding-system                        ; iso-2022-jp
default-terminal-coding-system                        ; japanese-shift-jis
(default-value 'buffer-file-coding-system)            ; japanese-shift-jis

(coding-system-base 'utf-8)                           ; mule-utf-8
(detect-coding-string "こばやし")                 ; (iso-latin-1 emacs-mule raw-text no-conversion)
(detect-coding-string (encode-coding-string "���΂₵" 'utf-8)) ; (japanese-shift-jis mule-utf-8 raw-text no-conversion)
(detect-coding-string "���΂₵") ; (japanese-shift-jis iso-latin-1 emacs-mule raw-text no-conversion)
(find-coding-systems-string "���΂₵")
(find-charset-string "abc�ق�")         ; (ascii japanese-jisx0208)
(find-charset-string "abc")             ; (ascii)

(let ((encode (encode-coding-string "���΂₵" 'utf-8)))
  (list encode (decode-coding-string encode 'utf-8))) ; ("\343\201\223\343\201\260\343\202\204\343\201\227" "���΂₵")

(map-internal-to-utf-8 "���΂₵") ; "こばやし" @xyzzy
(encode-coding-string "���΂₵" 'utf-8) ; "\343\201\223\343\201\260\343\202\204\343\201\227" @emacs
(string-as-multibyte (string-as-unibyte "���΂₵"))        ; "���΂₵"
(unibyte-char-to-multibyte (multibyte-char-to-unibyte ?��)) ; 2210 (����H) 
(apply #'make-char (split-char ?��))    ; 53794 (#o151042, #xd222, ?��)

;; �����񂩂�charset�̔�����@�́H (intern string) �łȂ���
coding-system-alist

(coding-system-list 'base-only)
charset-list
last-coding-system-used

(detect-coding-with-language-environment (point-min)
                                         (min (point-max) #x1000)
                                         "Japanese")
;; (iso-latin-1-unix emacs-mule-unix raw-text-unix no-conversion)
(detect-coding-region (point-min) (min (point-max) #x1000))
;; (iso-latin-1-unix emacs-mule-unix raw-text-unix no-conversion)

;; (set-language-environment "Japanese")
;; current-language-environment
;; (set-default-coding-systems 'japanese-shift-jis-dos)
;; (set-clipboard-coding-system 'japanese-shift-jis-dos)
;; (set-w32-system-coding-system 'japanese-shift-jis-dos)
;; (setq default-file-name-coding-system 'japanese-shift-jis)
;; (setq-default buffer-file-coding-system 'japanese-shift-jis-dos)
;; (setq default-terminal-coding-system 'japanese-shift-jis-dos)
;; (setq default-keyboard-coding-system 'japanese-shift-jis)
;; (setq default-process-coding-system '(japanese-iso-8bit . japanese-iso-8bit))


;; ����
(labels ((lookup-add-agents (dic path)
           (pushnew (list dic path) lookup-search-agents :test #'equal)))
  (let ((it "c:/meadow/packages/lisp/lookup/"))
    (when (file-exists-p it)
      (pushnew it load-path :test #'equal)
      (require 'lookup)
      (pushnew "c:/meadow/bin/" exec-path :test #'equal)
      (lookup-search-agents 'ndeb "c:/usr/local/dic/EDICT")
      (lookup-search-agents 'ndeb "c:/usr/local/dic/DEVIL")
      (lookup-search-agents 'ndeb "c:/usr/local/dic/ASCDATES")
      (lookup-search-agents 'ndeb "c:/usr/local/dic/JARGON")
      )))

;; �����̓��L�V�J����
(lexical-let ((count 0))
  (defun counter ()
    (incf count)))
(list (counter) (counter))              ; (1 2)

;; n�����Ԃ��֐���Ԃ��֐� (�ǂ������Horg)
;; http://ja.doukaku.org/comment/1273/
(defun make-ndays-later (n)
  (lexical-let ((n n))
    (lambda (time)
      (apply (lambda (s mi h d m y dow dst zone)
               (encode-time s mi h (+ d n) m y))
             (decode-time time)))))
(fset 'five-days-later (make-ndays-later 10))
(format-time-string "%Y/%m/%d %T" (five-days-later (current-time)))
;;=> "2008/12/25 06:02:02"

;;; elisp/CL �̈Ⴂ
;; 102: Emacs Lisp �� Common Lisp �͎��Ă���̂ł���?
;; http://stuff.mit.edu/afs/athena/astaff/project/babel/build/sun4/etc/FAQ.jp
;; �啶������������ʂ���
;; ���I�X�R�[�v
;; �p�b�P�[�W������
;; ���l������
;; ���[�_�}�N��������
;; �L�����A�s�������A���{������������

;; http://cl-cookbook.sourceforge.net/.emacs
;; NTEmacs ���ƒ��r���[�Ɋg�傳���񂾂�
(defun maximize-frame (&optional frame)
  "Maximize the selected FRAME."
  (interactive)
  (or frame (setq frame (selected-frame)))
  (let ((pixels-per-col (/ (float (frame-pixel-width))
                           (frame-width)))
        (pixels-per-row (/ (float (frame-pixel-height))
                           (frame-height))))
    (set-frame-size frame
                    ;; truncate or round?
                    (truncate (/ (x-display-pixel-width) pixels-per-col))
                    ;; reduce size to account for the toolbar
                    (- (truncate (/ (x-display-pixel-height) pixels-per-row)) 7))
    (set-frame-position frame 0 0)))

;;; @@window
(window-inside-edges)                   ; (1 1 123 20)

;; �}�E�X�|�W�V����
;; �t���[�����O���� nil �ɂȂ�炵��
(list (cdr (mouse-pixel-position))
      (cdr (mouse-position)))           ; ((587 . 269) (73 . 16))

;; �E�B���h�E�|�W�V�����A�����ƕ�
(list (cons (x-display-pixel-height) (x-display-pixel-width))
      (cons (x-display-mm-height) (x-display-mm-width))
      (posn-x-y (posn-at-point))
      (cons (window-height) (window-width)))

;; (window area-or-pos (x . y) timestamp object pos (col . row) image (dx . dy) (width . height))
(destructuring-bind (0posn-window
                     1posn-area
                     2posn-x-y
                     3posn-timestamp
                     4posn-string
                     5posn-point
                     6posn-actual-col-row
                     7posn-image
                     8posn-object-x-y
                     9posn-object-width-height)
    (posn-at-point) )

;; �t���[���|�W�V����
(list (cons (frame-pixel-height) (frame-pixel-width))
      (cons (frame-height) (frame-width))
      (cons (frame-char-height) (frame-char-width)))

;; �J�[�\���|�W�V����
(list (posn-col-row (posn-at-point))
      (posn-actual-col-row (posn-at-point))
      (cons (window-start) (window-end))
      (cons (point-min) (point-max)))

(current-frame-configuration)
(frame-parameters)
(current-window-configuration)

;; @@face/color
(list-colors-display)

(require 'linum)
(linum-on)

(defadvice indent-sexp (around indent-defun (&optional endpos))
  "Indent the enclosing defun (or top-level sexp)."
  (interactive)
  (save-excursion
    (beginning-of-defun)
    ad-do-it))

;; subr.el
(number-sequence 0 3 .5)                ; (0 0.5 1.0 1.5 2.0 2.5 3.0)

;; keymap �������O���� define-key ���ł��Ȃ��ăG���[
(define-key hoge-map [up] 'hoge-hoge)
;; �Ώ��@
(add-hook 'hoge-mode-hook
          #'(lambda ()
              (define-key hoge-map [up] 'hoge-hoge)))
(eval-after-load "hoge"
  '(define-key hoge-map [up] 'hoge-hoge))

;; Blogger �֘A
;; atom-api (+ nxml)
;; make �����s����̂Ŏ蓮��
;; $ emacs -batch -q -no-site-file -l rng-auto.el -f rng-byte-compile-load
(load "c:/tmp/nxml-mode-20041004/rng-auto")

;; $ emacs -Q --batch --eval '(print (upcase-initials "LL day and night"))'

(expand-file-name "~/Desktop.lnk")      ; "c:/home/lxuser/Desktop.lnk"
(file-truename "~/Desktop.lnk") ; "c:/Documents and Settings/shigeru/�f�X�N�g�b�v"

;; :key�g���Â炭�Ȃ����H
(funcall (lambda (&key absolute) (list absolute)) :absolute t) ; (t)
(funcall (lambda (&key absolute) (list absolute)))             ; ERROR
((lambda (a &optional &key absolute)
   (list a absolute))
 10)                                    ; (10 nil)

;; $ which chmod
(executable-find "chmod")               ; "c:/cygwin/bin/chmod.exe"

;; ���ʂ�������ƈႤ
(macroexpand '(do ((acc nil) (n 0 (1+ n))) ((> n 10) (nreverse acc))
               (push n acc)))
(cl-prettyexpand '(do ((acc nil) (n 0 (1+ n))) ((> n 10) (nreverse acc))
                   (push n acc)))

(cl-prettyexpand '(flet ((hoge () (list x (symbol-value 'x))))
                   (setq x 1)
                   (let ((x 10))
                     (hoge))))

(fset 'yes-or-no-p #'y-or-n-p)

(autoload 'mpg123 "mpg123" "A Front-end to mpg123/ogg123" t)

(defun buffer-list-by-name ()
  (sort (buffer-list)
        #'(lambda (x y)
            (string< (buffer-name x) (buffer-name y)))))

;; (buffer-list)�̏o�͂��\�[�g������@
(defun buffer-list-by-name ()
  (dolist (buffer (buffer-list-by-name))
    (bury-buffer buffer))
  (buffer-list))

(execute-kbd-macro "\M-\;")             ; 
(command-execute "\M-\;")               ; 

;; EOF�ȉ��̃o�b�t�@����Ȃ�
;; ���܂������ĂȂ��C������
(defun fit-window ()
  (interactive)
  (when (pos-visible-in-window-p (point-max))
    (enlarge-window (- (buffer-lines)
                       (window-lines)
                       (get-window-start-line)))))
;; (add-hook 'buffer-menu-mode-hook 'fit-window)

max-lisp-eval-depth                     ; 300

;; EmacsWiki����C���X�g�[�� (byrubikich)
;; http://www.emacswiki.org/cgi-bin/emacs/install-elisp.el

;;; �ꔭ�C���f���g
(defun indent-buffer ()
  "indent current buffer"
  (interactive)
  (indent-region (point-min) (point-max)))

;; elisp�̃f�o�b�O�֐��F�X
;; �f�o�b�K (debug, edebug)
;; http://www.bookshelf.jp/texi/elisp-manual/21-2-8/jp/elisp_18.html#Edebug
;; �v���t�@�C�� (profile)
;; http://www.mew.org/~kazu/doc/elisp/profile.html
(mapcar #'featurep '(profile elp disass debug edebug)) ; (nil nil nil t nil)

;; #1=(#1# x) �łȂ��́H
(let ((a '(x x))) (setcar a a))         ; (#0 x)
(let ((a '(x x))) (setcdr a a))         ; (x . #0)
(quote #1=(#1# x))                      ; (#0 x)

;; defun* (cl-macs.el) �̎g�������������

;;; @@dependence
;;; elisp�̈ˑ��֌W�𒲂ׂ�
(featurep 'loadhist)
(feature-file 'google)                  ; "c:/home/lxuser/lib/emacs/google.elc"
(feature-file 'cl)                      ; "c:/home/emacs/22.1/lisp/emacs-lisp/cl.elc"
(file-provides "cl")                    ; (cl cl-19)
(file-requires "cl")                    ; nil
(file-requires "xyzzy")                 ; (cl)
;; 'cl���ǂ̃t�@�C�����烍�[�h����Ă��邩
(file-dependents "cl")                  ; ("c:/home/emacs/22.1/lisp/emacs-lisp/cl-macs.elc" "c:/home/lxuser/lib/emacs/xyzzy-util.el" "c:/usr/local/clisp-2.47-full/lib/slime/hyperspec.elc" "c:/usr/local/clisp-2.47-full/lib/slime/slime.elc")

;; ?
(symbol-file 'cl)                       ; "c:/home/lxuser/lib/emacs/xyzzy.el"
(symbol-file 'xyzzy)                    ; "c:/home/lxuser/lib/emacs/xyzzy.el"
(symbol-file 'symbol-file)              ; "c:/home/emacs/22.1/lisp/subr.elc"

;; �ꉞ�c�����ǂ���Ȃ��Ǝv��
(defun sub-directory-p (dir parent)
  "DIRECTORY��PARENT�̃T�u�f�B���N�g���Ȃ�t�A�����łȂ����nil��Ԃ��B"
  (labels ((dirname (x)
             "������`/'�������f�B���N�g������Ԃ�: /home->/home/"
             (if (file-directory-p x)
                 (file-name-as-directory x)
                 (file-name-directory x))))
    ;; dirname��file-name-as-directory�����ł����Ǝv��
    (do ((x (pathname-directory (dirname dir)) (cdr x))
         (y (pathname-directory (dirname parent)) (cdr y)))
        ((null y) t)
      (if (or (null x)
              (not (equalp (car x) (car y))))
          (return nil)))))

;;; @@compile
;; �w�肵���f�B���N�g���ȉ����ăo�C�g�R���p�C��
(byte-recompile-directory "~/lib/emacs/" t) ; *.elc�̂Ȃ��t�@�C���������I�ɁH

;;; @@File-local Variables in Emacs
;;; http://www.kmc.gr.jp/~tak/memo/emacs-local-variable.html

(process-list)                          ; (#<process shell>)
(get-process "shell")                   ; #<process shell>

;; �}�E�X�z�C�[���ړ��̂Ƃ����� avoidance-mode �𖳌��ɂ������������A�o���Ȃ�����
(defadvice mwheel-scroll (around no-mouse-avoidance-mode activate)
  (let ((mouse-avoidance-mode nil))
    ad-do-it))

;; tabbar-mode
;; http://www.emacswiki.org/cgi-bin/wiki/TabBarMode

;; �T�ϕύX�ƃO���[�v���̕ύX
;; http://d.hatena.ne.jp/katsu_w/20080319
;; http://amitp.blogspot.com/2007/04/emacs-buffer-tabs.html

;; �J�����ĊJ���ꂽ�H�L�u�ɂ����̂��H
;; http://github.com/davidswelt/aquamacs-git/tree/master/src/site-lisp/tabbar/tabbar.el
(load-file "~/lib/tabbar.el")
(tabbar-mode t)
(remove-hook 'kill-buffer-hook 'tabbar-buffer-track-killed)

(global-set-key "\C-x\C-t\C-f" 'toggle-truncate-lines)

;; elisp�̃o�b�N�N�I�[�g�̈��� (backquote.el)

;; �Ȃ��G���[�ɂȂ�H -> elisp��&body�͂Ȃ��B&rest���g����defmacro*���g����
(defmacro when1 (test &body body)
  (let ((result (gensym)))
    `(let ((,result ,test))
       (when ,result ,@body)
       ,result)))
(when1 (position ?a "kobayashi")
       (princ "������!"))

(setq )
(set-variable )

;;; ���K�\���������̂��x������c�[��
(regexp-opt '("define" "lambda" "fn" "define-macro" "lambda-macro") t)
"\\(define\\(?:-macro\\)?\\|fn\\|lambda\\(?:-macro\\)?\\)"
(regexp-opt '("define" "lambda" "fn" "define-macro" "lambda-macro") 'words)
"\\<\\(define\\(?:-macro\\)?\\|fn\\|lambda\\(?:-macro\\)?\\)\\>"

;;; (0 1 2 3 4 ...) �Ȃ�ė��L����Ȃ�
(setq eval-expression-print-length nil
      eval-expression-print-level nil)
(list eval-expression-print-length eval-expression-print-level) ; (12 4)

;;; @@defadvice�}�N��
(ad-is-active 'eval-last-sexp)
;; ������/�s������
(ad-activate ad-deactivate)

;;; Emacs@vine�̃J�[�\���F�t�����Ăǂ�����Ă������H
(list input-method-activate-hook input-method-inactivate-hook)
(mapcar #'boundp '(mw32-ime-on-hook mw32-ime-off-hook))
(lambda () (set-cursor-color "brown"))

;;; !? string-to-list@mule-util.el
(append "���΂₵" nil)                 ; (53811 53840 53860 53815)

;;; @@etags
;; http://www8.atpages.jp/hotsuma/chalow/2002-04-16.html#2002-04-16-1
TAGS �R�}���h�܂Ƃ� [Emacs]
M-x find-tag (M-.) �V���{���̒�`�����ɔ�ԁB
M-x pop-tag-mark (M-*) �O�̏�Ԃɔ�ԁB
M-x tags-search �V���{��������������B
M-x tags-loop-continue (M-,) ���̌��ɔ�ԁB(������ԂŎ��s)
M-x find-tag-other-window (C-x 4 .) ��`������ʃE�B���h�E�ɕ\���B
M-x find-tag-other-frame (C-x 5 .) ��`������ʃt���[���ɕ\���B
M-x complete-symbol (M-TAB) �V���{�����̕⊮(�^�u�e�[�u�����ǂݍ��܂�Ă��鎞)
M-x visit-tags-table �^�u�e�[�u����ǂݍ��ݒ����B

(defun what-charset-region (from to)
  (interactive "r")
  (message "%s" (find-charset-region from to)))

frame-title-format
header-line-format
mode-line-format
icon-title-format

;; ���΃f�B���N�g���H
(expand-file-name (file-relative-name "c:/home/TODO.txt")) ; "c:/home/TODO.txt"

(list most-positive-fixnum most-negative-fixnum) ; (268435455 -268435456)

(defun redraw-emacs ()
  (redraw-display)
  (redraw-frame (selected-frame))
  (redraw-modeline t))

(every #'char-valid-p (number-sequence #x00 #xFF)) ; t

;; �֐��A�}�N���A�ϐ��𖢒�`������
;; �ˑ��֌W����������Ȃ��ƃG���[ (���̃t�@�C������ require ����Ă�Ƃ�)
(require 'redo)
(unload-feature 'redo)

;;; �R�}���h���C������
command-line-args                       ; ("C:\\home\\emacs\\22.1\\bin\\emacs.exe")
;;; �X�N���v�g����Ƃ��ė��p
noninteractive                          ; nil (�o�b�`������ t)
$ emacs --script (Emacs 22 �ȍ~�̃I�v�V����)
$ emacs --batch -Q -l 2> /dev/null
;; --quick, -Q (equivalent to -q --no-site-file --no-splash)
;;; �o�b�`�t�@�C������R���p�C��
;;; ��Œ��ׂ�
$ emacs -batch -q -no-site-file -l $FILE

$ emacs -batch -f batch-byte-compile *.el

;; #! ���R�����g�s�Ƃ��Ė�������� (����͋����[��)
(apply #'+
       #! (error "comment?")
       '(1 2 3 4 5 6 7 8 9 10))         ; 55

;;; auto-complete-mode
(require 'auto-complete)
(global-auto-complete-mode t)

(defun run-newlisp ()
  (interactive)
  ;; run-scheme?
  (let ((default-process-coding-system '(utf-8 . utf-8)))
    (run-scheme (format "C:/PROGRA~1/newlisp/newlisp.exe -C -w %s"
                        (expand-file-name default-directory)))))

;;; �S�p�󔒁A�^�u�Ȃǂ̋����\��
;;; ���ꂾ��font-lock���Ăяo����邽�тɃL�[���[�h���ǉ�����Ēx���Ȃ�Ȃ����H
;;; http://eigyr.dip.jp/diary/200712.html#%E5%85%A8%E8%A7%92%E3%82%B9%E3%83%9A%E3%83%BC%E3%82%B9%E3%82%84%E3%82%BF%E3%83%96%E3%82%92%E5%BC%B7%E8%AA%BF

;;; @@Info
(defun info-elisp-manual ()
  "Display the Emacs Lisp manual in Info mode."
  (interactive)
  (info "elisp"))
(Info-goto-node)                        ; info-mode �ŃC���^���N�e�B�u�ɌĂяo���Ă݂�(g)
(call-interactively #'info-lookup-symbol)
(directory-files (car Info-directory-list) nil "elisp*")
(info "(elisp)Regular Expressions")
(info "(elisp)Mapping Functions")
(info "(elisp)Definition of signal")
(info "(elisp)Definition of mapatoms")
(info "(libc)Formatted Output Functions")

;;; w32console.c
(set-message-beep nil)
(set-message-beep 'asterisk)
(set-message-beep 'exclamation)
(set-message-beep 'hand)
(set-message-beep 'ok)
(set-message-beep 'silent)

;;; Woman (C-x 5 0 :delete-frame)
(defun woman-at-point ()
  (interactive)
  (let ((woman-use-topic-at-point t))
    (woman)))
;; (global-set-key "\C-cw" 'woman-at-point)
(global-set-key "\C-cw" 'manual-entry)  ; �ł���� Woman ��肱����
(setq woman-use-own-frame nil)

;; �⊮��
;; http://www.bookshelf.jp/texi/elisp-manual/21-2-8/jp/elisp_20.html#SEC270
completion-ignore-case                                        ; t
(try-completion "f" '("foo" "bar" "baz" "bazz" "hoge" "for")) ; "fo"
(try-completion "b" '("foo" "bar" "baz" "bazz" "hoge" "for")) ; "ba"
(all-completions "f" '("foo" "bar" "baz" "bazz" "hoge" "for")) ; ("foo" "for")

(defun list-hexadecimal-colors-display ()
  (interactive)
  (list-colors-display
   (mapcar #'(lambda (x)
               (format "#%.6x" x))
           (number-sequence #x000000 #xe0e0e0 #x000020))))
(byte-compile 'list-hexadecimal-colors-display)
(logand #b0111)
(format "%x" #b0111)
(logand #b01111 #b0001)
(defun logcount (integer)
  (do ((n 0 (1+ n))
       (i integer (lsh)))))

;;; @@Bitwise Operations on Integers
;; lsh (logical shift: �_���V�t�g)
(lsh #b0101 #b0001)                     ; 10 #b1010
(lsh #b0111 #b0001)                     ; 14 #b1110
(lsh #b0110 -1)                         ;  3 #b0011

;; ash (arithmetic shift: �Z�p�V�t�g) 
(ash #b11111111111111111111111111010    ; -6
     #b11111111111111111111111111111    ; -1
     )                                  ; -3 #b11111111111111111111111111101

(encode-coding-string "���΂₵" 'cp932)  ; "\202\261\202\316\202\342\202\265"
(encode-coding-string "���΂₵" 'binary) ; "\222\244\263\222\244\320\222\244\344\222\244\267"
(encode-coding-string "���΂₵" 'sjis)   ; "\202\261\202\316\202\342\202\265"
(encode-coding-string "���΂₵" 'euc-jp) ; "\244\263\244\320\244\344\244\267"
(encode-coding-string "���΂₵" 'utf-8)  ; "\343\201\223\343\201\260\343\202\204\343\201\227"
(encode-coding-string "���΂₵" 'utf-16) ; "\376\3770S0p0\2040W"

(string-as-multibyte (string-as-unibyte "���΂₵")) ; "���΂₵"

(decode-sjis-char (encode-sjis-char ?z)) ; 122 (#o172, #x7a, ?z)

(normal-top-level-add-to-load-path DIRS)

;; �t�@�C����A�Ԃɂ���
;; �������A���Ƀ\�[�g����Ă���K�v����
(let ((n 0)
      (ext "jpg"))
  (dolist (file (directory-files #1=DIR nil (format "\\.%s$" ext)))
    (rename-file (expand-file-name file #1#)
                 (expand-file-name (format "%04d.%s" n ext) #1#))
    (setq n (1+ n))))

(defun list-to-string (char-list) (apply #'string char-list))
(list-to-string (string-to-list "����͊���������")) ; "����͊���������"

;; Y combinator �ۂ�����
(defun f (q)
  (lexical-let ((q q))
    (lambda (n)
      (if (= n 0) 1 (* n (funcall (funcall q q) (- n 1)))))))
(funcall (f 'f) 10)                     ; 3628800

;;; help-fns.el:describe-function-1:253
(defun function-truename (def)
  "DEF���֐��̃G�C���A�X�Ȃ�΁A�֐��̎��̖���Ԃ�."
  (while (symbolp (symbol-function def))
    (setq def (symbol-function def)))
  def)

(local-variable-if-set-p 'truncate-lines) ; t
(local-variable-p 'truncate-lines)        ; nil

;; http://condotti.blogspot.com/2007_06_01_archive.html
(defsubst buffer-bytes (buffer)
  "Return number of bytes in a buffer."
  (with-current-buffer buffer
    (1- (position-bytes (point-max)))))
;; ����A�t�@�C���T�C�Y�Ƃ͈Ⴄ�̂��ˁH
(list 
 (buffer-bytes ".emacs.my.el")
 (buffer-size (get-buffer ".emacs.my.el"))
 (nth 7 (file-attributes "~/.emacs.my.el"))
 )                                      ; (16404 16854 14852)

(format-time-string "%Y-%m-%dT%T%z")    ; "2009-04-14T04:44:54+0900"
;; UNIX time
(decode-time '(0 0 0))                  ; (0 0 9 1 1 1970 4 nil 32400)
;; Universal Time
(multiple-value-list
 (decode-universal-time 0))             ; (0 0 9 1 1 1900 0 NIL -9) [CL]

(defun symbol-describe (symbol)
  `((:name     ,(symbol-name symbol))
    (:value    ,(and (boundp symbol)
                     (symbol-value symbol)))
    (:function ,(and (fboundp symbol)
                     (if (byte-code-function-p #1=(symbol-function symbol))
                         symbol #1#)))
    (:plist    ,(symbol-plist symbol))
    (:file     ,(symbol-file symbol))))

(defun* bindp (key &optional (map global-map))
  (lookup-key map key))
