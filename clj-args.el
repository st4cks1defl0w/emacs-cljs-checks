  (setq-default vector-pattern "\\[[^.]*?\\]" )
  (setq-default map-destructuring-pattern "{:keys \\[[^.]*?}")
  (setq-default fn-pattern "(.*)*")

  (defun extract-destructured-args (str)
    (setq-local defunced (replace-regexp-in-string fn-pattern ":arg-side-args-emacs" str))
    (setq-local saved-match (string-match vector-pattern defunced))
    (setq-local saved-match-str (match-string 0 defunced))
    (if (null saved-match-str)
      (substring saved-match-str 1 -1)
      ""))

  (defun strip-forms (str)
    (replace-regexp-in-string vector-pattern ":arg-side-args-emacs"
     (replace-regexp-in-string map-destructuring-pattern ":arg-side-args-emacs"
      (replace-regexp-in-string fn-pattern ":arg-side-args-emacs" str))))

  (defun remove-odd (list)
    (loop for i in list
          for idx from 0
          unless (oddp idx)
          collect i))

;;todo place destrctured args into their correct positions
  (defun arg->arg-and-label (arg)
    (when (not (string= ":arg-side-args-emacs" arg))
        (concat "\n\"___" arg " \"" " " arg)))

  (defun str-vector->labeled-args (str-vector sexp-type)
(message sexp-type)
    (setq-local trimmed-vector (substring str-vector 1 -1))
    (setq-local flattened-arguments (split-string (strip-forms trimmed-vector)))
    (setq-local flattened-destructured-arguments (split-string (extract-destructured-args trimmed-vector)))
    (let ((arg-list (if (string= sexp-type "def")
                     flattened-arguments
                     (remove-odd flattened-arguments))))
    (concat "(println \"***args are \" " (mapconcat 'arg->arg-and-label
                                                    (append
                                                     arg-list
                                                     flattened-destructured-arguments) " ") ")\n")))

  (defun arg-side-debug (&optional n)
    (interactive "P")
    (setq-local insert-at (point))
    (sp-beginning-of-sexp)
    (setq-local sexp-type (evil-yank-characters (point) (+ 3 (point))))
    (down-list)
    (sp-backward-up-sexp)
    (let* ((args (evil-cp-yank-sexp 1)))
      (goto-char insert-at)
      (insert-string (str-vector->labeled-args
                      args sexp-type))))

(defun debug-spit-symbol (&optional n)
  (interactive "P")
  (let* ((yanked (evil-paste-before 0)))
  (insert-string (concat "\n(println \">>>>spitting data "
                         yanked ":\" "
                         yanked ")\n"))))

  (dolist (m '(clojure-mode))
    (spacemacs/set-leader-keys-for-major-mode m
      "j" 'cider-project-reset
      "J" 'cider-dev
      "sj" 'cider-connect-sibling-cljs
      "sa" 'cider-default-connect
      "sC" 'cider-replicate-connection
      "hc" 'clojure-cheatsheet))

  (dolist (m '(clojure-mode clojurescript-mode))
    (spacemacs/set-leader-keys-for-major-mode m
      "da" 'arg-side-debug
      "ds" 'debug-spit-symbol
      "gk" 'cider-find-keyword))
