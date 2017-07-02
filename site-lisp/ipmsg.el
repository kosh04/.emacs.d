;;; ipmsg.el --- IP Messenger client                 -*- lexical-binding: t; -*-

;; Copyright (C) 2016  KOBAYASHI Shigeru

;; Author: KOBAYASHI Shigeru <shigeru.kb@gmail.com>
;; Version: 0.1snapshot
;; Package-Requires: ((emacs "24.4"))
;; Keywords: comm

;;; Commentary:

;; 簡易版 IP Messanger クライアント
;; [TODO] メッセージの送受信とメンバ一覧の表示くらいは実装したい

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'pcase)
  (require 'rx))

(defconst ipmsg-commands
  '(
    (#x00000000 . NOOPERATION)
    (#x00000001 . BR_ENTRY)
    (#x00000002 . BR_EXIT)
    (#x00000003 . ANSENTRY)
    (#x00000004 . BR_ABSENCE)

    (#x00000010 . BR_ISGETLIST)
    (#x00000011 . OKGETLIST)
    (#x00000012 . GETLIST)
    (#x00000013 . ANSLIST)
    (#x00000018 . BR_ISGETLIST2)

    (#x00000020 . SENDMSG)
    (#x00000021 . RECVMSG)
    (#x00000030 . READMSG)
    (#x00000031 . DELMSG)
    (#x00000032 . ANSREADMSG)

    (#x00000040 . GETINFO)
    (#x00000041 . SENDINFO)

    ;; option for all command
    (#x00000100 . ABSENCEOPT)
    (#x00000200 . SERVEROPT)
    (#x00010000 . DIALUPOPT)
    (#x00200000 . FILEATTACHOPT)
    (#x00400000 . ENCRYPTOPT)
    (#x00800000 . UTF8OPT)
    (#x01000000 . CAPUTF8OPT)
    (#x04000000 . ENCEXTMSGOPT)
    (#x08000000 . CLIPBOARDOPT)
    (#x00001000 . CAPFILEENCOPT)
    ))

(define-error 'ipmsg-error "IPMSG error")
(define-error 'ipmsg-unknown-command "Unknown command" 'ipmsg-error)

(defun ipmsg--command-to-number (cmds)
  (cl-labels ((to-num (cmd)
                (or (car (rassoc cmd ipmsg-commands))
                    (signal 'ipmsg-unknown-command cmd))))
    (apply #'logior (mapcar #'to-num cmds))))

(defun ipmsg--number-to-command (n)
  (cl-loop for (nc . cmd) in ipmsg-commands
           if (not (zerop (logand n nc)))
           collect cmd))

(cl-defstruct (ipmsg-message
               (:constructor ipmsg-message-create
                             (cmds text &aux
                                   (body (concat text "\0"))
                                   (command (ipmsg--command-to-number cmds)))))
  (version 1 :type number)
  (packet (truncate (float-time)) :type number)
  (user (user-login-name) :type string)
  (host (system-name) :type string)
  (command 0 :type number)                           ; [NOOPERATION]
  (body     "" :type string)
  (original "" :type string))

(defun ipmsg-message-string (msg)
  (format "%d:%d:%s:%s:%d:%s"
          (ipmsg-message-version msg)
          (ipmsg-message-packet msg)
          (ipmsg-message-user msg)
          (ipmsg-message-host msg)
          (ipmsg-message-command msg)
          (ipmsg-message-body msg)))

(defconst ipmsg-message-re
  (rx string-start
      (group (1+ num)) ":"                 ; Ver(1)
      (group (1+ num)) ":"                 ; Packet
      (group (1+ (not (any ":")))) ":"     ; User
      (group (1+ (not (any ":")))) ":"     ; Host
      (group (1+ num)) ":"                 ; Command
      (group (0+ any))                     ; Optional (?)
      (* "\0")
      ;;string-end
      ))

(defun ipmsg--parse-message (data)
  (cl-labels ((decode (str &optional (charset 'cp932))
                (decode-coding-string str charset)))
    (when (string-match ipmsg-message-re data)
      (make-ipmsg-message
       :version (string-to-number (match-string 1 data))
       :packet  (string-to-number (match-string 2 data))
       :user    (decode (match-string 3 data))
       :host    (decode (match-string 4 data))
       :command (string-to-number (match-string 5 data))
       :body    (decode (match-string 6 data))
       :original data
       ))))

(defun ipmsg--send-message (proc msg)
  (process-send-string proc (ipmsg-message-string msg)))

(defun ipmsg-send (proc cmds text)
  (let* ((msg (ipmsg-message-create cmds text))
         (data (ipmsg-message-string msg)))
    (process-send-string proc data)))

(defsubst ipmsg-log (fmt &rest args)
  (let ((date (format-time-string "%FT%T%z"))
        (msg (let ((print-escape-newlines t))
               (apply #'format fmt args))))
    (message "%s %s" date msg)))

(defun ipmsg--server-filter (proc data)
  (let ((msg (ipmsg--parse-message data)))
    (ipmsg-log "[%s] -> recv from %s %S" proc (process-contact proc :remote) (or msg data))
    (when (ipmsg-message-p msg)
      (pcase (assoc-default (ipmsg-message-command msg) ipmsg-commands)
        (`BR_ENTRY (ipmsg-send proc [ANSENTRY] "kosh-emacs"))
        ))))

(defvar ipmsg-server-process nil)

;;;###autoload
(defun ipmsg-server-start ()
  (interactive)
  (unless ipmsg-server-process
    (setq ipmsg-server-process
          (make-network-process
           :name "ipmsg"
           ;;:broadcast t
           :service 2425
           :server t
           :family 'ipv4
           :type 'datagram
           :coding '(raw-text . raw-text)
           :filter #'ipmsg--server-filter)))
  (message "Start %s...done" ipmsg-server-process))

(defun ipmsg-quit ()
  (interactive)
  (ipmsg-send "ipmsg" [BR_EXIT] "")
  (if (process-live-p ipmsg-server-process)
      (delete-process ipmsg-server-process)))

(provide 'ipmsg)

;;; ipmsg.el ends here
