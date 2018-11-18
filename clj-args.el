(defun strip-functions (str)
  (replace-regexp-in-string "([^.]*)\\|(.*)" "arg" str))

(defun strip-destructuring (str)
  (replace-regexp-in-string "{:keys \\[\\|\\]}" "" str))

(defun remove-odd (list)
  (loop for i in list for idx from 0 unless (oddp idx) collect i))

(defun arg->arg-and-label (arg)
  (concat "\n\"___" arg " \"" " " arg))

(defun str-vector->labeled-args (str-vector sexp-type)
  (setq-local after-functions (strip-functions str-vector))
  (setq-local after-destr (strip-destructuring after-functions))
  (setq-local vector-trimmed (split-string (substring after-destr 1 -1)))
  (let ((arg-list (if (string= sexp-type "def") vector-trimmed
                    (remove-odd vector-trimmed))))
    (concat "(println \"***args are \" "
            (mapconcat 'arg->arg-and-label arg-list " ") ")\n")))

(defun arg-side-debug
    (&optional
     n)
  (interactive "P")
  (setq-local insert-at (point))
  (sp-beginning-of-sexp)
  (setq-local sexp-type (evil-yank-characters (point)
                                              (+ 3 (point))))
  (down-list)
  (sp-backward-up-sexp)
  (let* ((args (evil-cp-yank-sexp 1)))
    (goto-char insert-at)
    (insert-string (str-vector->labeled-args args sexp-type))))
