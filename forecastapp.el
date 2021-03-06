;;; forecastapp.el -- forecast.app integration with emacs. -*- lexical-binding: t; -*-
;; Copyright (C) 2020  Bruno Dias

;; Author: Bruno Dias <dias.h.bruno@gmail.com>
;; Url: https://github.com/diasbruno/forecastapp.el
;; Package-requires: ((emacs "25.1") (tablist "1.0") (navigel "0.7.0") (s "1.12.0"))
;; Version: 0.0.1

;;; Commentary:

;; Start point to navigate through the Forecast API.

;;; Code:

(require 'navigel)
(require 'forecastapp-utils)
(require 'forecastapp-net)
(require 'forecastapp-project-view)
(require 'forecastapp-sprints)

(defvar *forecastapp--current-project* nil
  "Defined when expanding a project.")

(defvar *forecastapp--current-sprint* nil
  "Defined when expanding a sprint.")

(defun forecastapp--setup-hooks ()
  "Setup all necessary hooks."
  (print (current-local-map))
  (define-key (current-local-map) "v"
    (lambda ()
      (interactive)
      (let ((project (navigel-entity-at-point)))
        (setf *forecastapp--current-project* project)
        (forecastapp-open-project project)))))

(navigel-method forecastapp-projects navigel-children (projects callback)
  "Call CALLBACK with the files in DIRECTORY as parameter."
  (funcall callback projects))

(navigel-method forecastapp-projects navigel-tablist-format (project)
  (vector (list "Name" 20 t)
          (list "Start Date" 10 nil :right-align t)
          (list "End Date" 10 nil :right-align t)))

(navigel-method forecastapp-projects navigel-buffer-name (project)
  "list")

(navigel-method forecastapp-projects navigel-entity-to-columns (project)
  (vector (assocdr 'name project)
          (assocdr 'start_date project)
          (assocdr 'end_date project)))

(navigel-method forecastapp-projects navigel-open (project target)
  (setf navigel-init-done-hook #'forecastapp--setup-hooks)
  (case target
    (init (cl-call-next-method))
    (t (progn
         (setf *forecastapp--current-project* project)
         (forecastapp-sprints)))))

(navigel-method forecastapp-projects navigel-delete (project &optional callback)
  (funcall callback))

(defun forecastapp ()
  "Initialize and list all projects."
  (interactive)
  (forecast--get-projects
     nil
     (cl-function
      (lambda (&key data &allow-other-keys)
        (let ((navigel-app 'forecastapp-projects))
          (navigel-open data 'init))))
     (cl-function
      (lambda (&rest args &key error-thrown &allow-other-keys)
        (print x)))))

(provide 'forecastapp)
;;; forecastapp.el ends here
