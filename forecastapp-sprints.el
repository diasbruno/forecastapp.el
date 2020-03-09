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
(require 'forecastapp)
(require 'forecastapp-utils)
(require 'forecastapp-net)
(provide 'forecastapp-sprint-view)

(defun forecastapp--setup-sprint-hook ()
  "Setup the the mode with some extra key bindings."
  (define-key (current-local-map) "v"
    (lambda ()
      (interactive)
      (let ((project *forecastapp--current-project*)
            (sprint (navigel-entity-at-point)))
        (if (not (null project))
            (progn
              (setf *forecastapp--current-sprint* sprint)
              (forecastapp-open-sprint))
          (error "Project not selected"))))))

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

(defun forecastapp-sprints ()
  "List all sprints of a project."
  (interactive)
  (let ((project *forecastapp--current-project*))
      (if (not (null project))
        (forecast--get-project-sprints
         (assocdr 'id project)
         nil
         (cl-function
          (lambda (&key data &allow-other-keys)
            (let ((navigel-app 'forecastapp-sprints))
              (navigel-open data 'init))))
         (cl-function
          (lambda (&rest args &key error-thrown &allow-other-keys)
            (print x))))
      (error "Project not defined"))))

(provide 'forecastapp-sprints)
;;; forecastapp-sprints.el ends here
