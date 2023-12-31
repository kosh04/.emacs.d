;;; config/EasyPG

;; TODO: gpg 本体のアップデートによって解消された可能性あり
'
(with-eval-after-load 'epg
  ;; monkey patching
  (defconst epg-pubkey-algorithm-alist
    '((1 . "RSA")
      (2 . "RSA_E")
      (3 . "RSA_S")
      (16 . "ELGAMAL_E")
      (17 . "DSA")
      (20 . "ELGAMAL")
      (22 . "ECC")                      ;+
      ))
  )

(use-package epa
  :preface
  (defun user::epa-list-keys-copy-fingerprint (pos &optional event)
    (interactive "@d")
    (let* ((button (get-char-property pos 'button))
           (epg-key (widget-get button :value))
           (sub-keys (epg-key-sub-key-list epg-key))
           (sub-key (elt sub-keys 0))
           (fingerprint (epg-sub-key-fingerprint sub-key)))
      (message "Copied %s" fingerprint)
      (kill-new fingerprint)))
  :bind
  (:map epa-key-list-mode-map
        ("w" . user::epa-list-keys-copy-fingerprint)))

;; gpg --> gpg-agent --> pinentry --> Emacs
(use-package pinentry
  :demand
  :config
  (pinentry-start))
