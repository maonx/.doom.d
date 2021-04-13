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
(setq doom-theme 'doom-one-light)
(if (string-match "\\`Win10" (system-name))
        (setq doom-theme 'doom-one))

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

;; Set Chinese input methond
(use-package pyim
  :ensure nil
  :demand t
  :config
  ;; 激活 basedict 拼音词库，五笔用户请继续阅读 README
  (pyim-basedict-enable)

  (setq default-input-method "pyim")

  ;; 我使用小鹤双拼
  (setq pyim-default-scheme 'xiaohe-shuangpin)

  ;; 设置 pyim 探针设置，这是 pyim 高级功能设置，可以实现 *无痛* 中英文切换 :-)
  ;; 我自己使用的中英文动态切换规则是：
  ;; 1. 光标只有在注释里面时，才可以输入中文。
  ;; 2. 光标前是汉字字符时，才能输入中文。
  ;; 3. 使用 M-j 快捷键，强制将光标前的拼音字符串转换为中文。
  (setq-default pyim-english-input-switch-functions
                '(pyim-probe-dynamic-english
                  pyim-probe-isearch-mode
                  pyim-probe-program-mode
                  pyim-probe-org-structure-template))

  (setq-default pyim-punctuation-half-width-functions
                '(pyim-probe-punctuation-line-beginning
                  pyim-probe-punctuation-after-punctuation))

  ;; 开启拼音搜索功能
  (pyim-isearch-mode 1)
  ;; 设置模糊音
  (setq pyim-fuzzy-pinyin-alist '(("en" "eng")
                                  ("in" "ing")
                                  ("z" "zh")
                                  ("s" "sh")
                                  ("c" "ch")
                                  ))

  ;; 使用 popup-el 来绘制选词框, 如果用 emacs26, 建议设置
  ;; 为 'posframe, 速度很快并且菜单不会变形，不过需要用户
  ;; 手动安装 posframe 包。
  (setq pyim-page-tooltip 'posframe)
  ;; (setq pyim-page-tooltip 'popup)

  ;; 选词框显示5个候选词
  (setq pyim-page-length 5)

  :bind
  (("M-;" . pyim-convert-string-at-point) ;;与 pyim-probe-dynamic-english 配合
   ("C-;" . pyim-delete-word-from-personal-buffer)))

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
