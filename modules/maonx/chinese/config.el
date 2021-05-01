;;; ~/.doom.d/+chinese.el -*- lexical-binding: t; -*-

;; fork from https://github.com/cnsunyour/.doom.d

;;
;;; Hack
;;;
(define-advice org-html-paragraph (:filter-args (args) chinese-a)
  "Join consecutive Chinese lines into a single long line without
unwanted space when exporting org-mode to html."
  (++chinese--org-paragraph args))

(define-advice org-hugo-paragraph (:filter-args (args) chinese-a)
  "Join consecutive Chinese lines into a single long line without
unwanted space when exporting org-mode to hugo markdown."
  (++chinese--org-paragraph args))

(defun ++chinese--org-paragraph (args)
  (cl-destructuring-bind (paragraph content info) args
    (let* ((origin-contents
            (replace-regexp-in-string
             "<[Bb][Rr][[:blank:]]*/>"
             ""
             content))
           (origin-contents
            (replace-regexp-in-string
             "\\([[:multibyte:]]\\)[[:blank:]]*\n[[:blank:]]*\\([[:multibyte:]]\\)"
             "\\1\\2"
             origin-contents))
           (fixed-contents
            (replace-regexp-in-string
             "\\([^[:blank:]]\\)[[:blank:]]*\n[[:blank:]]*\\([^[:blank:]]\\)"
             "\\1 \\2"
             origin-contents)))
      (list paragraph fixed-contents info))))


(use-package! rime
  :bind
  ("M-;" . #'+rime-convert-string-at-point)
  (:map rime-active-mode-map
   ("C-M-S-j" . #'rime-inline-ascii)
   ("C-M-S-s-j" . #'rime-inline-ascii))
  (:map rime-mode-map
   ("C-M-S-s-j" . #'rime-force-enable)
   ("C-." . #'rime-send-keybinding)
   ("S-SPC" . #'rime-send-keybinding)
   ("S-<delete>" . #'rime-send-keybinding)
   ("C-`" . #'rime-send-keybinding)
   ("C-~" . #'rime-send-keybinding)
   ("C-S-`" . #'rime-send-keybinding))
  ;; :hook
  ;; ((after-init kill-emacs) . (lambda ()
  ;;                              (ignore-errors (rime-sync))))
  :config
  (setq default-input-method "rime"
        rime-user-data-dir (expand-file-name "~/.config/rime")
        rime-show-candidate 'message
        rime-inline-ascii-trigger 'shift-l)

  (add-hook! (org-mode
              markdown-mode
              beancount-mode)
    (activate-input-method default-input-method))

  (defun +rime-force-enable ()
    "[ENHANCED] Force into Chinese input state.

If current input method is not `rime', active it first. If it is
currently in the `evil' non-editable state, then switch to
`evil-insert-state'."
    (interactive)
    (let ((input-method "rime"))
      (unless (string= current-input-method input-method)
        (activate-input-method input-method))
      (when (rime-predicate-evil-mode-p)
        (if (= (1+ (point)) (line-end-position))
            (evil-append 1)
          (evil-insert 1)))
      (rime-force-enable)))

  (defun +rime-convert-string-at-point ()
    "Convert the string at point to Chinese using the current input scheme.

First call `+rime-force-enable' to active the input method, and
then search back from the current cursor for available string (if
a string is selected, use it) as the input code, call the current
input scheme to convert to Chinese."
    (interactive)
    (+rime-force-enable)
    (let ((string (if mark-active
                      (buffer-substring-no-properties
                       (region-beginning) (region-end))
                    (buffer-substring-no-properties
                     (point) (max (line-beginning-position) (- (point) 80)))))
          code
          length)
      (cond ((string-match "\\([a-z]+\\|[[:punct:]]\\)[[:blank:]]*$" string)
             (setq code (replace-regexp-in-string
                         "^[-']" ""
                         (match-string 0 string)))
             (setq length (length code))
             (setq code (replace-regexp-in-string " +" "" code))
             (if mark-active
                 (delete-region (region-beginning) (region-end))
               (when (> length 0)
                 (delete-char (- 0 length))))
             (when (> length 0)
               (setq unread-command-events
                     (append (listify-key-sequence code)
                             unread-command-events))))
            (t (message "`+rime-convert-string-at-point' did nothing.")))))

  (unless (fboundp 'rime--posframe-display-content)
    (error "Function `rime--posframe-display-content' is not available."))
  (define-advice rime--posframe-display-content (:filter-args (args) resolve-posframe-issue-a)
    "给 `rime--posframe-display-content' 传入的字符串加一个全角空
格，以解决 `posframe' 偶尔吃字的问题。"
    (cl-destructuring-bind (content) args
      (let ((newresult (if (string-blank-p content)
                           content
                         (concat content "　"))))
        (list newresult))))

  (load! "+rime-predicates"))


;; Support pinyin in Ivy
;; Input prefix ';' to match pinyin
;; Refer to  https://github.com/abo-abo/swiper/issues/919 and
;; https://github.com/pengpengxp/swiper/wiki/ivy-support-chinese-pinyin
(use-package! pinyinlib
  :commands (pinyinlib-build-regexp-string)
  :init
  (with-no-warnings
    (defun ivy--regex-pinyin (str)
      "The regex builder wrapper to support pinyin."
      (or (pinyin-to-utf8 str)
          (and (fboundp '+ivy-prescient-non-fuzzy)
               (+ivy-prescient-non-fuzzy str))
          (ivy--regex-plus str)))

    (defun my-pinyinlib-build-regexp-string (str)
      "Build a pinyin regexp sequence from STR."
      (cond ((equal str ".*") ".*")
            (t (pinyinlib-build-regexp-string str t))))

    (defun my-pinyin-regexp-helper (str)
      "Construct pinyin regexp for STR."
      (cond ((equal str " ") ".*")
            ((equal str "") nil)
            (t str)))

    (defun pinyin-to-utf8 (str)
      "Convert STR to UTF-8."
      (cond ((equal 0 (length str)) nil)
            ((equal (substring str 0 1) ";")
             (mapconcat
              #'my-pinyinlib-build-regexp-string
              (remove nil (mapcar
                           #'my-pinyin-regexp-helper
                           (split-string
                            (replace-regexp-in-string ";" "" str)
                            "")))
              ""))
            (t nil)))

    (mapcar
     (lambda (item)
       (let ((key (car item))
             (value (cdr item)))
         (when (member value '(+ivy-prescient-non-fuzzy
                               ivy--regex-plus))
           (setf (alist-get key ivy-re-builders-alist)
                 #'ivy--regex-pinyin))))
     ivy-re-builders-alist)))