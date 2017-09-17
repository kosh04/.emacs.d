;;; config/Tramp

(with-eval-after-load 'tramp
  ;; easily sudo with /sudo:root@remote-host:<path-to-file>
  (add-to-list 'tramp-default-proxies-alist '(nil "\\`root\\'" "/ssh:%h:")))
