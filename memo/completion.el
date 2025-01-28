;;; memo/completion

;; シンボル補完 HOWTO
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Completion-in-Buffers.html#Completion-in-Buffers

;; 参考
(makefile-completions-at-point)
(lisp-completion-at-point)

;; How do I write a simple `completion-at-point`
;; http://emacs.stackexchange.com/questions/15276/how-do-i-write-a-simple-completion-at-point-functions-function

(completion-table-dynamic #'FUNCTION)

;; Completion annotations
;; - http://garethrees.org/2015/02/09/emacs/

(defun my-annotation-function (s)
  (let ((item (assoc s minibuffer-completion-table)))
    (when item (concat "  -- " (second item)))))

(let* ((my-completions '(("a" "description of a") ("b" "b's description")))
       (completion-extra-properties '(:annotation-function my-annotation-function))
       (completion-cycle-threshold nil))
  (completing-read "Prompt: " my-completions))

;; 補完用シンボルを絞る
;; - 正規表現で絞る
(let ((completion-regexp-list '("-mode\\'")))
  (completing-read "Mode: " obarray))
;; - predicateで絞る
(completing-read "Keymap: " obarray 'keymapp)

;; 複数選択可能なプロンプトには接頭辞にマークがあると嬉しいかも
(defun crm-indicator (args)
  (cons (concat "[CRM] " (car args)) (cdr args)))
(advice-add #'completing-read-multiple :filter-args #'crm-indicator)
