;;; ropoems.el --- Search romanian poems using helm

;; Copyright (C) 2016-2017 Mihai Olteanu

;; Author: Mihai Olteanu <mihai_olteanu@fastmail.fm>
;; Created: 2017

;; Keywords: others
;; Homepage: https://github.com/mihaiolteanu/ropoems

(require 'helm-ag)

(setq ropoems-db "~/projects/ropoems/db/")
(setq ropoems-current-poet "")

(defun get--string-from-file (file-path)
  "Return file-path's file content."
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(defun ropoems--get-poem-path (poem author)
  "Get the absolute path of the poem."
  (concat (file-name-as-directory (concat ropoems-db author)) poem))

(defun ropoems--get-poem-verse (poem author)
  "Get the actual poem verses"
  (get--string-from-file (ropoems--get-poem-path poem author)))

(defun ropoems--get-poets ()
  "Get a list of all the poets in the database"
  (directory-files ropoems-db nil "^[^.].*$")) ;Ignore . and .. folders

(defun ropoems--get-poet-poems (poet)
  "Get a list of all the poems for the given poet"
  (directory-files (concat ropoems-db poet) nil "^[^.].*$"))

(setq helm-ropoems-poets-source
      '((name . "helm ropoems poets")
        (candidates . ropoems--get-poets)
        (action . (lambda (candidate)
                    (setq ropoems-current-poet candidate)
                    (helm :sources '(helm-ropoems-poet-poems-source))))))

(setq helm-ropoems-poet-poems-source
      '((name . "helm ropoems poet poems")
        (candidates . (lambda ()
                        (ropoems--get-poet-poems ropoems-current-poet)))
        (action . (lambda (poem)
                    (ropoems--open-poem
                     poem
                     ropoems-current-poet
                     (ropoems--get-poem-verse poem ropoems-current-poet))))))

(defun ropoems-list-poets ()
  "Interactively select a poet and list all its poems"
  (interactive)
  (helm :sources '(helm-ropoems-poets-source)))

(defun ropoems--open-poem (poem author verse)
  "Open a new buffer, if one does not already exist, with the
  given poem and author in ropoems mode. The verse is inserted
  into the new buffer."
  (let ((ropoems-buffer (format "%s - %s" author poem)))
    (if (not (get-buffer ropoems-buffer))  ;Do nothing if the poem is already open.
        (with-current-buffer (get-buffer-create ropoems-buffer)
          (put-text-property 0 (length poem) 'face 'info-title-3 poem)
          (insert poem)
          (newline)
          (put-text-property 0 (length author) 'face 'italic author)
          (insert author)
          (newline)
          (newline)
          (insert verse)
          (ropoems-mode)
          (make-local-variable 'ropoems-poem)
          (setq ropoems-poem poem)
          (make-local-variable 'ropoems-author)
          (setq ropoems-author author)
          (beginning-of-buffer)))
    (switch-to-buffer ropoems-buffer)))

(defun ropoems-author-message ()
  (interactive)
  (insert (ropoems--get-poet-poems
           (replace-regexp-in-string " " "-" ropoems-author))))

;; helm-search for poem contents using helm-ag. (hint: use -g to
;; search for poem/author name) Some of the code is taken from the
;; official helm-ag.el file. The selected poem then opens in a new
;; buffer.
(defun ropoems--open-poem-from-search (candidate)
  "Open a poem in a ropoems buffer as an action of a poem search."
  (let* ((poem-author (car (split-string candidate ":")))
         (poem (cadr (split-string poem-author "/")))
         (author (car (split-string poem-author "/")))
         (verse (ropoems--get-poem-verse poem author)))
    (ropoems--open-poem
     (replace-regexp-in-string "-" " " poem)
     (replace-regexp-in-string "-" " " author)
     verse)))

(defvar helm-ropoems--actions
  (helm-make-actions
   "Open file" #'ropoems--open-poem-from-search))

(defvar helm-source-ropoems
  (helm-build-async-source "Ropoems"
    :init 'helm-ag--do-ag-set-command
    :candidates-process 'helm-ag--do-ag-candidate-process
    :persistent-action  'ropoems--open-poem-from-search
    :action helm-ropoems--actions
    :nohighlight t
    :requires-pattern 3
    :candidate-number-limit 9999
    :follow (and helm-follow-mode-persistent 1)))

(defun ropoems-search-poems ()
  "Interactively search for romanian poems using the helm-ag"
  (interactive)
  (let* ((helm-ag--default-directory ropoems-db))
    (helm :sources helm-source-ropoems)))

;; ropoems major mode - used to navigate the buffer
(define-derived-mode ropoems-mode special-mode "ropoems"
  "Major mode for viewing and browsing romanian poems"
  (setq buffer-read-only t))

(define-key ropoems-mode-map (kbd "q") 'kill-this-buffer)
(define-key ropoems-mode-map (kbd "n") 'next-line)
(define-key ropoems-mode-map (kbd "p") 'previous-line)

;; Keybindings
(progn
  (define-prefix-command 'ropoems-prefix-map)
  (define-key 'ropoems-prefix-map (kbd "p") 'ropoems-list-poets)
  (define-key 'ropoems-prefix-map (kbd "s") 'ropoems-search-poems))

(global-set-key (kbd "C-c P") ropoems-prefix-map)


(provide 'ropoems)
