;;; forecastapp-sprints.el -- forecast.app integration with emacs. -*- lexical-binding: t; -*-
;; Copyright (C) 2020  Bruno Dias

;; Author: Bruno Dias <dias.h.bruno@gmail.com>
;; Url: https://github.com/diasbruno/forecastapp.el
;; Package-requires: ((emacs "25.1") (tablist "1.0") (navigel "0.7.0") (s "1.12.0"))
;; Version: 0.0.1

;;; Commentary:

;; List all sprints of a project.

;;; Code:

(require 'navigel)
(require 'forecastapp-net)

(defalias 'assocdr #'alist-get)

(defun forecastapp--setup-sprint-hook ()
  "Setup the the mode with some extra key bindings."
  nil)

(navigel-method forecastapp-sprints navigel-buffer-name (sprint)
  "sprints")

(navigel-method forecastapp-sprints navigel-open (sprints target)
  (setf navigel-init-done-hook #'forecastapp--setup-sprint-hook)
  (case target
    (init (cl-call-next-method))
    (t
     ;; TODO(dias): by hitting RET on a sprint,
     ;; maybe we load the tasks associated.
     (print sprints))))

(navigel-method forecastapp-sprints navigel-entity-to-columns (sprint)
  (vector (assocdr 'name sprint)
          (assocdr 'start_date sprint)
          (assocdr 'end_date sprint)))

(navigel-method forecastapp-sprints navigel-children (sprints callback)
  "Call CALLBACK with the files in DIRECTORY as parameter."
  (funcall callback sprints))

(navigel-method forecastapp-sprints navigel-tablist-format (sprint)
  (vector (list "Name" 20 t)
          (list "Start Date" 10 nil :right-align t)
          (list "End Date" 10 nil :right-align t)))

(navigel-method forecastapp-sprints navigel-delete (sprint &optional callback)
  (funcall callback))

(provide 'forecastapp-sprints)
;;; forecastapp-sprints.el ends here
