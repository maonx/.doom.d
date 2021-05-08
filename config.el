;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Maonx"
      user-mail-address "maonx@qq.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)
(if (string-match "\\`Win10-Office" (system-name))
        (setq doom-theme 'doom-one-light))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/emacs/org/")

(setq org-agenda-files '("~/emacs/org/inbox.org"
                         "~/emacs/org/todo.org"))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
;; (setq display-line-numbers-type t)
(setq display-line-numbers-type 'relative)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(if (eq system-type 'windwos-nt)
    (setq system-time-locale "C")
  (format-time-string "%Y-%m-%d %a")
  )

;; (setq-default fill-column 80)

;; 编码设置 begin
;; (set-language-environment 'Chinese-GB)
(set-language-environment 'utf-8)
;; default-buffer-file-coding-system变量在emacs23.2之后已被废弃，使用buffer-file-coding-system代替
(set-default buffer-file-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
;; 影响导出文件名乱码问题
(setq-default pathname-coding-system 'utf-8)
(setq file-name-coding-system 'utf-8)

;; 另外建议按下面的先后顺序来设置中文编码识别方式。
;; 重要提示:写在最后一行的，实际上最优先使用; 最前面一行，反而放到最后才识别。
;; utf-16le-with-signature 相当于 Windows 下的 Unicode 编码，这里也可写成
;; utf-16 (utf-16 实际上还细分为 utf-16le, utf-16be, utf-16le-with-signature等多种)
(prefer-coding-system 'cp950)
(prefer-coding-system 'gb2312)
(prefer-coding-system 'cp936)
(prefer-coding-system 'gb18030)

;(prefer-coding-system 'utf-16le-with-signature)
(prefer-coding-system 'utf-16)

;; 新建文件使用utf-8-unix方式
;; 如果不写下面两句，只写
;; (prefer-coding-system 'utf-8)
;; 这一句的话，新建文件以utf-8编码，行末结束符平台相关
(prefer-coding-system 'utf-8-dos)
(prefer-coding-system 'utf-8-unix)
;; 编码设置 end

(defun self-font()
  (interactive)
  (set-frame-font (format "%s:pixelsize=%d" "Monaco" 14) t)
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font) charset
                      (font-spec :family "Hiragino Sans GB W3" :size 16))))
(self-font)

(setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("org" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/")
                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))


(global-evil-matchit-mode 1)

;; Disable word-wrap
(setq word-wrap 1)

;;Exit insert mode by pressing f and then d quickly
(key-chord-mode 1)
(setq key-chord-two-keys-delay 0.3)
(key-chord-define-global "fd" 'evil-normal-state)

;; Disable evil-snipe-s for keybind s
(after! evil-snipe
  (evil-snipe-mode -1))

(setq fancy-splash-image "~/.doom.d/King_Boo_MMWii.png")

;; Disable word-wrap
(setq word-wrap 1)

(after! org
  (setq org-capture-templates nil)

  (setq org-log-done 'time)

  (setq org-todo-keywords
        '((sequence
           "TODO(t)"
           "DELAY(D!)"
           "|"
           "DONE(d)"
           "CANCELL(c@/!)"
           )))
  (add-to-list 'org-capture-templates
               '("i" "Inbox" entry
                 (file "~/emacs/org/inbox.org")
                 "* %U - %^{Title} %^G\n %?\n" :empty-lines 1))
  (add-to-list 'org-capture-templates
               '("t" "Tasks" entry
                 (file "~/emacs/org/todo.org" )
                 "* TODO %^{Title} %^G\nOPENED: %U\n%?\n" :empty-lines 1))
  ;; (add-to-list 'org-capture-templates
  ;;              '("l" "Life" entry
  ;;                (file+olp+datetree "~/org/life.org" "Life")
  ;;                "* %U - %^{Title}\n %?\n" :empty-lines 1))
  (add-to-list 'org-capture-templates
               '("w" "Troubleshooting" entry
                 (file+headline "~/emacs/org/blog/post.org" "故障排除")
                 "** %^{Title}\n %?\n" :empty-lines 0))

  ;; Populates only the EXPORT_FILE_NAME property in the inserted headline.
  (with-eval-after-load 'org-capture
    (defun org-hugo-new-subtree-post-capture-template ()
      "Returns `org-capture' template string for new Hugo post.
See `org-capture-templates' for more information."
      (let* ((title (read-from-minibuffer "Post Title: ")) ;Prompt to enter the post title
             (fname (org-hugo-slug title)))
        (mapconcat #'identity
                   `(
                     ,(concat "* TODO " title)
                     ":PROPERTIES:"
                     ,(concat ":EXPORT_FILE_NAME: " fname)
                     ":END:"
                     "%?\n")          ;Place the cursor here finally
                   "\n")))

    (add-to-list 'org-capture-templates
                 '("h"                ;`org-capture' binding + h
                   "Hugo post"
                   entry
                   ;; It is assumed that below file is present in `org-directory'
                   ;; and that it has a "Blog Ideas" heading. It can even be a
                   ;; symlink pointing to the actual location of all-posts.org!
                   (file "./blog/post.org" )
                   (function org-hugo-new-subtree-post-capture-template))))
  )


;; ox-hugo
(use-package! ox-hugo
  :after ox
  :config
  (org-hugo-auto-export-mode)
  ;; 解决ox-hugo 转换 md 时代码块有多余缩进
  (setq org-src-preserve-indentation nil)
  )

(use-package! rime
  :config
  (setq default-input-method "rime"
        rime-user-data-dir (expand-file-name "~/.config/rime")
        rime-show-candidate 'posframe
        )
  (global-set-key (kbd "M-;") 'toggle-input-method)
  (global-set-key (kbd "C-;") 'toggle-input-method)

  (add-hook! (org-mode
              markdown-mode
              beancount-mode)
    (activate-input-method default-input-method))
)

(use-package! beancount
  :defer t
  :bind
  ;; ("C-M-b" . (lambda ()
  ;;              (interactive)
  ;;              (find-file "~/Dropbox/beancount/main.bean")))
  :mode
  ("\\.bean\\(?:count\\)?\\'" . beancount-mode)
  ;; :config
  ;; (setq beancount-accounts-files
  ;;       (directory-files "~/Dropbox/beancount/accounts/"
                         ;; 'full
                         ;; (rx ".bean" eos)))
)

(fset 'delete-empty-lines (kbd "M-x flush-lines RET ^\s-*$ RET"))
