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
