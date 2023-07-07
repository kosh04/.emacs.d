# スタートアップの順序

- https://www.gnu.org/software/emacs/manual/html_node/elisp/Startup-Summary.html#Startup-Summary
- https://ayatakesi.github.io/lispref/28.1/html/Startup-Summary.html#Startup-Summary
- 

1. Add `load-path` subdirs.el
2.
3.
4. Set `before-init-time`
5. Parse `command-line-args`
6. Load early-init.el
7. Call `package-activate-all` if `package-enable-at-startup` or (not `-q`,`-Q`,`--batch`)
8. Initialize window system
9. Run `before-init-hook`
10. Create graphical frame
11. Initialize initial frame's face. メニューバーとツールバーのセットアップ
12. Run? `custom-reevaluate-setting` `custom-delayed-init-variables`
13. Load site-start
14. Load init.el
15. Load default.el
16. Load `abbrev-file-name`
17. Set `after-init-time`
18. Run `after-init-hook`
19. Initialize `*scratch*` by `initial-major-mode`
20. Run `tty-setup-hook`
21. Display init message unless `inhibit-startup-echo-area-message`
22. Process rest `command-line-args` options
23. exit if `--batch`
24. Initialize `*scratch*` by `(substitute-command-keys initial-scratch-message)`
25. Visit `initial-buffer-choice`
26. Run `emacs-startup-hook`
27. Call `frame-notice-user-settings`
28. Run `window-setup-hook`
29. Display startup screen unless `inhibit-startup-screen`
30. `server-start` if `--daemon`
31.

XXX: Run `after-pdump-load-hook` at end of the Emacs startup process (since emacs-29)


## 参考リンク

- [Qiita](https://qiita.com/tadsan/items/4f5743de21b7aa06ca35 "Emacsを起動する")
