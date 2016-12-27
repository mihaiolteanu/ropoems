(setq ropoems-db "~/projects/ropoems/db/")
(setq ropoems-current-poet "")

(defun ropoems-poets ()
  (directory-files ropoems-db))

(defun ropoems-poet-poems (folder)
  (directory-files (concat ropoems-db folder)))

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
  (define-key 'ropoems-prefix-map (kbd "<f6>") 'ropoems-list-poets)
  (define-key 'ropoems-prefix-map (kbd "<f7>") 'ropoems-search-poems)
  )

(global-set-key (kbd "<f9>") ropoems-prefix-map)


