;;; ropoems.el --- Search romanian poems using helm

;; Copyright (C) 2016-2017 Mihai Olteanu


;; Author: Mihai Olteanu <mihai_olteanu@fastmail.fm>
;; Created: 2017

;; Keywords: others
;; Homepage: https://github.com/mihaiolteanu/ropoems

(require 'helm-ag)

(setq ropoems-db "~/projects/ropoems/db/")
(setq ropoems-current-poet "")

(defun get-string-from-file (file-path)
  "Return file-path's file content."
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

;; helm-search from a list of poets. After the poet is selected,
;; helm-search from a list of his poems. The selected poem then opens
;; in a new buffer.
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
        (action . (lambda (poem)
                    (let ((verse (get-string-from-file (concat (file-name-as-directory
                                                                (concat ropoems-db ropoems-current-poet))
                                                               poem))))
                      (ropoems--open-poem poem ropoems-current-poet verse))))))

(defun ropoems-list-poets ()
  (interactive)
  (helm :sources '(helm-ropoems-poets-source)))

(defun ropoems--open-poem (poem author verse)
  "Open a new buffer, if one does not already exist, with the
  given poem and author in ropoems mode. The verse is inserted
  into the new buffer."
  (let ((buffer-name (format "%s - %s" author poem)))
    (if (not (get-buffer buffer-name))  ;Do nothing if the poem is already open.
        (with-current-buffer (get-buffer-create buffer-name)
          (put-text-property 0 (length poem) 'face 'info-title-3 poem)
          (insert poem)
          (newline)
          (put-text-property 0 (length author) 'face 'italic author)
          (insert author)
          (newline)
          (newline)
          (insert verse)
          (setq-local buffer-poem poem)
          (setq-local buffer-author author)
          (ropoems-mode)
          (beginning-of-buffer)))
    (switch-to-buffer buffer-name)))

(defun get-string-from-file (filePath)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

;; helm-search for poem contents using helm-ag. (hint: use -g to
;; search for poem/author name) Some of the code is taken from the
;; official helm-ag.el file. The selected poem then opens in a new
;; buffer.
(defun ropoems--open-poem-from-search (candidate)
  (let* ((poem-author (car (split-string candidate ":")))
         (poem (replace-regexp-in-string "-" " "
                                         (cadr (split-string poem-author "/"))))
         (author (replace-regexp-in-string "-" " "
                                           (car (split-string poem-author "/"))))
         (verse (with-temp-buffer
                  (insert-file-contents (concat ropoems-db poem-author))
                  (buffer-string))))
    (ropoems--open-poem poem author verse)
    (switch-to-buffer poem-buffer)))

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
