  (setq vector-pattern "\\[[^.]*?\\]" )
  (setq map-destructuring-pattern "{:keys \\[[^.]*?}")
  (setq fn-pattern "(.*)*")

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
        (concat "\n\">>>" arg " \"" " " arg)))

  (defun str-vector->labeled-args (str-vector sexp-type)
(message sexp-type)
    (setq-local trimmed-vector (substring str-vector 1 -1))
    (setq-local flattened-arguments (split-string (strip-forms trimmed-vector)))
    (setq-local flattened-destructured-arguments (split-string (extract-destructured-args trimmed-vector)))
    (let ((arg-list (if (string= sexp-type "def")
                     flattened-arguments
                     (remove-odd flattened-arguments))))
    (concat "(println \">>>args are \" " (mapconcat 'arg->arg-and-label
                                                    (append
                                                     arg-list
                                                     flattened-destructured-arguments) " ") ")\n")))


  (defun emacs-cljs-checks-args (&optional n)
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

(defun emacs-cljs-checks-print-symbol (&optional n)
  (interactive "P")
  (let* ((yanked (evil-paste-before 0)))
  (insert-string (concat "\n(println \">>> "
                         yanked ":\" "
                         yanked ")\n"))))

;; ;;;;START HELM-AG.EL - A HACK TO PASS CUSTOM AG INPUT

;; (defun helm-do-ag--helm (&optional custom-input)
;;   (let ((search-dir (if (not (helm-ag--windows-p))
;;                         helm-ag--default-directory
;;                       (if (helm-do-ag--target-one-directory-p helm-ag--default-target)
;;                           (car helm-ag--default-target)
;;                         helm-ag--default-directory))))
;;     (helm-attrset 'name (helm-ag--helm-header search-dir)
;;                   helm-source-do-ag)
;;     (helm :sources '(helm-source-do-ag) :buffer "*helm-ag*" :keymap helm-do-ag-map
;;           :input (or  custom-input
;;                       (helm-ag--marked-input t)
;;                      (helm-ag--insert-thing-at-point helm-ag-insert-at-point))
;;           :history 'helm-ag--helm-history)))

;; ;;;###autoload
;; (defun helm-do-ag-this-file ()
;;   (interactive)
;;   (helm-aif (buffer-file-name)
;;       (helm-do-ag default-directory (list it))
;;     (error "Error: This buffer is not visited file.")))

;; ;;;###autoload
;; (defun helm-do-ag (&optional basedir targets custom-input)
;;   (interactive)
;;   (require 'helm-mode)
;;   (helm-ag--init-state)
;;   (let* ((helm-ag--default-directory (or basedir default-directory))
;;          (helm-ag--default-target (cond (targets targets)
;;                                         ((and (helm-ag--windows-p) basedir) (list basedir))
;;                                         (t
;;                                          (when (and (not basedir) (not helm-ag--buffer-search))
;;                                            (helm-read-file-name
;;                                             "Search in file(s): "
;;                                             :default default-directory
;;                                             :marked-candidates t :must-match t)))))
;;          (helm-do-ag--extensions (when helm-ag--default-target
;;                                    (helm-ag--do-ag-searched-extensions)))
;;          (one-directory-p (helm-do-ag--target-one-directory-p
;;                            helm-ag--default-target)))
;;     (helm-ag--set-do-ag-option)
;;     (helm-ag--set-command-features)
;;     (helm-ag--save-current-context)
;;     (helm-attrset 'search-this-file
;;                   (and (= (length helm-ag--default-target) 1)
;;                        (not (file-directory-p (car helm-ag--default-target)))
;;                        (car helm-ag--default-target))
;;                   helm-source-do-ag)
;;     (if (or (helm-ag--windows-p) (not one-directory-p)) ;; Path argument must be specified on Windows
;;         (helm-do-ag--helm custom-input)
;;       (let* ((helm-ag--default-directory
;;               (file-name-as-directory (car helm-ag--default-target)))
;;              (helm-ag--default-target nil))
;;         (helm-do-ag--helm custom-input)))))

;; (defun helm-ag--project-root ()
;;   (cl-loop for dir in '(".git/" ".hg/" ".svn/" ".git")
;;            when (locate-dominating-file default-directory dir)
;;            return it))


;; ;;;###autoload
;; (defun helm-do-ag (&optional basedir targets custom-input)
;;   (interactive)
;;   (require 'helm-mode)
;;   (helm-ag--init-state)
;;   (let* ((helm-ag--default-directory (or basedir default-directory))
;;          (helm-ag--default-target (cond (targets targets)
;;                                         ((and (helm-ag--windows-p) basedir) (list basedir))
;;                                         (t
;;                                          (when (and (not basedir) (not helm-ag--buffer-search))
;;                                            (helm-read-file-name
;;                                             "Search in file(s): "
;;                                             :default default-directory
;;                                             :marked-candidates t :must-match t)))))
;;          (helm-do-ag--extensions (when helm-ag--default-target
;;                                    (helm-ag--do-ag-searched-extensions)))
;;          (one-directory-p (helm-do-ag--target-one-directory-p
;;                            helm-ag--default-target)))
;;     (helm-ag--set-do-ag-option)
;;     (helm-ag--set-command-features)
;;     (helm-ag--save-current-context)
;;     (helm-attrset 'search-this-file
;;                   (and (= (length helm-ag--default-target) 1)
;;                        (not (file-directory-p (car helm-ag--default-target)))
;;                        (car helm-ag--default-target))
;;                   helm-source-do-ag)
;;     (if (or (helm-ag--windows-p) (not one-directory-p)) ;; Path argument must be specified on Windows
;;         (helm-do-ag--helm custom-input)
;;       (let* ((helm-ag--default-directory
;;               (file-name-as-directory (car helm-ag--default-target)))
;;              (helm-ag--default-target nil))
;;         (helm-do-ag--helm custom-input)))))

;; ;;;;END HELM-AG.EL;;;;


(defun emacs-cljs-checks-usages (&optional n)
  (interactive "P")
  (let* ((starting-point (point))
         (yanked-sexp (evil-cp-yank-sexp 1)))
    (evil-goto-first-line)
    (evil-forward-WORD-begin)
    (setq yanked-ns (string-remove-suffix ".core"
                                          (string-remove-prefix "transportal." (evil-cp-yank-sexp 1))))
    (goto-char starting-point)
    (insert-string
     (helm-do-ag (projectile-project-root) nil (concat "[^\.]" yanked-ns "/" yanked-sexp " ")))))

  (dolist (m '(clojure-mode clojurescript-mode))
    (spacemacs/set-leader-keys-for-major-mode m
      "cu" 'emacs-cljs-checks-usages
      "ca" 'emacs-cljs-checks-args
      "cs" 'emacs-cljs-checks-print-symbol))
