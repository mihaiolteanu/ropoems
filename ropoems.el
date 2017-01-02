(setq ropoems-db "~/projects/ropoems/db/")
(setq ropoems-current-poet "")

(defun ropoems-poets ()
  (directory-files ropoems-db nil "^[^.].*$"))

(defun ropoems-poet-poems (folder)
  (directory-files (concat ropoems-db folder) nil "^[^.].*$"))

(setq helm-ropoems-poets-source
      '((name . "helm ropoems poets")
        (candidates . ropoems-poets)
        (action . (lambda (candidate)
                    (setq ropoems-current-poet candidate)
                    (helm :sources '(helm-ropoems-poet-poems-source))))))

(setq helm-ropoems-poet-poems-source
      '((name . "helm ropoems poet poems")
        (candidates . (lambda ()
                        (ropoems-poet-poems ropoems-current-poet)))
        (action . (lambda (candidate)
                    (find-file-read-only
                     (concat (file-name-as-directory
                              (concat ropoems-db ropoems-current-poet))
                             candidate))))))

(defun ropoems-list-poets ()
  (interactive)
  (helm :sources '(helm-ropoems-poets-source)))

(defun ropoems-search-poems ()
  (interactive)
  (helm-do-ag ropoems-db))

(progn
  (define-prefix-command 'ropoems-prefix-map)
  (define-key 'ropoems-prefix-map (kbd "p") 'ropoems-list-poets)
  (define-key 'ropoems-prefix-map (kbd "s") 'ropoems-search-poems)
  )

(global-set-key (kbd "C-c P") ropoems-prefix-map)



(defun helm-ropoems--action-open-poem (candidate)
  (let* ((poem-author-name (car (split-string candidate ":")))
         (poem-name (replace-regexp-in-string "-" " "
                                              (cadr (split-string poem-author-name "/"))))
         (author (replace-regexp-in-string "-" " "
                                           (car (split-string poem-author-name "/"))))
         (poem-buffer (get-buffer-create (format "%s - %s" author poem-name))))
    (with-current-buffer poem-buffer
      (put-text-property 0 (length poem-name) 'face 'info-title-2 poem-name)
      (insert poem-name)
      (newline)
      (put-text-property 0 (length poem-name) 'face 'italic author)
      (insert author)
      (newline)
      (insert-file-contents (concat ropoems-db poem-author-name))
      (newline))
    (switch-to-buffer poem-buffer)))

(defvar helm-ropoems--actions
  (helm-make-actions
   "Open file" #'helm-ropoems--action-open-poem))

(defvar helm-source-ropoems
  (helm-build-async-source "Ropoems"
    :init 'helm-ag--do-ag-set-command
    :candidates-process 'helm-ag--do-ag-candidate-process
    :persistent-action  'helm-ag--persistent-action
    :action helm-ropoems--actions
    :nohighlight t
    :requires-pattern 3
    :candidate-number-limit 9999
    :follow (and helm-follow-mode-persistent 1)))

(defun ropoems-search-poems ()
  (interactive)
  (let* ((helm-ag--default-directory ropoems-db))
    (helm :sources helm-source-ropoems)))


