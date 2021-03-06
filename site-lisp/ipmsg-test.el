(require 'ipmsg)
(require 'cl-lib)
(require 'ert)

(ert-deftest command ()
  (should (= (ipmsg--command-to-number []) 0))
  (should (= (ipmsg--command-to-number ()) 0))
  (should (= (ipmsg--command-to-number [NOOPERATION]) 0))

  (let ((cmds '(UTF8OPT))
        (value 8388608))
    (should (= (ipmsg--command-to-number cmds) value))
    (should (equal (ipmsg--number-to-command value) cmds)))

  (should-error (ipmsg--command-to-number [unknown]))
  )

(ert-deftest parse-message ()
  (let* ((data "1:1467180876:lxuser-<0123456789abcdef>:localhost:224395265:Nicknamex  
UN:lxuser-<0123456789abcdef>
HN:localhost
NN:Nickname
GN:")
         (msg (ipmsg--parse-message data)))
    ;; msg => [cl-struct-ipmsg-message 1 1467180876 ...]
    (should (=     (ipmsg-message-version msg) 1))
    (should (=     (ipmsg-message-packet msg) 1467180876))
    (should (equal (ipmsg-message-user msg) "lxuser-<0123456789abcdef>"))
    (should (equal (ipmsg-message-host msg) "localhost"))
    (should (=     (ipmsg-message-command msg) 224395265))
    ))

'
(progn
  (defvar ipmsg-client-process nil)
  (setq ipmsg-client-process
        (make-network-process
         :name "ipmsg-cli"
         :service 2425
         ;;:host 'local
         ;;:remote "255.255.255.255"
         :host "255.255.255.255"
         :broadcast t
         :family 'ipv4
         :type 'datagram
         :coding '(raw-text . raw-text)
         :filter (lambda (proc data)
                   (message "[%s] Recv %s" proc data))
         :sentinel (lambda (proc)
                     (message "[%s] Quit" proc))))
  (ipmsg--send-message
   ipmsg-client-process
   ;;ipmsg-server-process
   (make-ipmsg-message :command 1 :body "HELLO HELLO HELLO"))
  (process-datagram-address ipmsg-client-process)
  (process-datagram-address ipmsg-server-process)
  (delete-process ipmsg-client-process)
  )
