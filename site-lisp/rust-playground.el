;;; rust-playground.el --- Rust Playground api client  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  KOBAYASHI Shigeru

;; Author: KOBAYASHI Shigeru <shigeru.kb@gmail.com>
;; URL: https://github.com/kosh04/rust-playground.el
;; Keywords: programming, tools, rust
;; Version: 0.1-beta
;; Package-Requires: ((emacs "24.4") (let-alist "1.0.5"))
;; Created: 2018-01-29

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; - https://play.rust-lang.org/
;; - https://github.com/integer32llc/rust-playground/

;; $ rust-play exec
;;   [-channel=stable]
;;   [-mode=debug]
;;   [-crate-type=bin]
;;   [-tests=false]
;;   FILENAME

;; $ rust-play compile
;;   [-channel=stable|beta|nightly]
;;   [-crate-type=bin|lib]
;;   [-assembly-flavor=demangle|mangle]
;;   [-hide-assembler-directives=hide|show]
;;   [-mode=debug|release]
;;   [-target=asm|llvm-ir|mir|wasm]
;;   [-tests=?]
;;   FILENAME

;;; Example:
;; (rust-playground-execute :code (f-read "~/Dropbox/snippet/rust/rng-sample.rs"))
;; (rust-playground-compile :code (f-read "~/Dropbox/snippet/rust/hello.rs") :target "llvm-ir")
;; (rust-playground-compile :code (f-read "~/Dropbox/snippet/rust/hello.rs") :target "mir")

;;; Todo:
;; - /format
;; - share code (gist)

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'let-alist))
(require 'json)
(require 'request)
(require 'f)
(require 's)

(defvar rust-playground-buffer-name
  "*Rust Playground*")

(defsubst rust-playground--dump (data)
  (cl-labels ((prn (obj) (princ obj) (terpri)))
    (let-alist data
      (when (s-presence .stderr)
        (prn "## Standard Error")
        (prn .stderr))
      (when (s-presence .stdout)
        (prn "## Standard Output")
        (prn .stdout))
      (when (s-presence .code)
        (prn "## Result")
        (prn .code)))))


(defsubst rust-playground--send (url data callback)
  (request url
           :type "POST"
           :headers '(("Content-Type" . "application/json"))
           :data (json-encode data)
           :parser 'json-read
           :success (cl-function
                     (lambda (&key data &allow-other-keys)
                       (with-output-to-temp-buffer rust-playground-buffer-name
                         (funcall callback data))))))

(cl-defun rust-playground-execute (&key
                 (code "")
                 (channel (or "stable" "beta" "nightly"))
                 (mode (or "debug" "release"))
                 (ctate-type (or "bin")) ; "lib" ?
                 (tests nil))
  ;;(cl-assert (s-presence code))
  (rust-playground--send "https://play.rust-lang.org/execute"
         `(("channel" . ,channel)
           ("mode" . ,mode)
           ("crateType" . ,ctate-type)
           ("tests" . ,(if tests t json-false))
           ("code" . ,code))
         #'rust-playground--dump))

;;;###autoload
(defun rust-playground-execute-file (path)
  (interactive "fExecute rust file: ")
  (rust-playground-execute :code (f-read path)))

;;;###autoload
(cl-defun rust-playground-compile (&key
                 (channel (or "stable" "beta" "nightly"))
                 (code "")
                 (crate-type "bin") ; or "lib" ?
                 (assembly-flavor (or "att" "intel"))
                 (demangle-assembly (or "demangle" "mangle"))
                 (hide-assembler-directives (or "hide" "show"))
                 (mode (or "debug" "release"))
                 (target (or "asm" "llvm-ir" "mir" "wasm"))
                 (tests nil))
  ;;(cl-assert (s-presence code))
  (rust-playground--send "https://play.rust-lang.org/compile"
         `(("channel" . ,channel)
           ("mode" . ,mode)
           ("crateType" . ,crate-type)
           ("tests" . ,(if tests t json-false))
           ("code" . ,code)
           ("target" . ,target)
           ("assemblyFlavor" . ,assembly-flavor)
           ("demangleAssembly" . ,demangle-assembly)
           ("hideAssemblerDirectives" . ,hide-assembler-directives))
         #'rust-playground--dump))

;; (defun rust-playground-format (code))

(provide 'rust-playground)
;;; rust-playground.el ends here
