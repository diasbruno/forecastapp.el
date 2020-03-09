;;; forecastapp-project-view.el -- forecast.app integration with emacs. -*- lexical-binding: t; -*-
;; Copyright (C) 2020  Bruno Dias

;; Author: Bruno Dias <dias.h.bruno@gmail.com>
;; Url: https://github.com/diasbruno/forecastapp.el
;; Package-requires: ((emacs "25.1") (tablist "1.0") (navigel "0.7.0") (s "1.12.0"))
;; Version: 0.0.1

;;; Commentary:

;; Presents a view of the selected project.

;;; Code:

(require 'forecastapp-utils)
(require 'forecastapp-net)

(defun forecastapp--project-buffer-name (name)
  "Make the project buffer name with project NAME."
  (concat "forecastapp-project: " name))

(defun forecastapp-open-project (project)
  "Open a PROJECT view."
  (forecast--get-project
   (assocdr 'id project)
   nil
   (cl-function
    (lambda (&key data &allow-other-keys)
      (let* ((pj-name (assocdr 'name data))
             (bf-name (forecastapp--project-buffer-name pj-name))
             (bf (get-buffer-create bf-name)))
        (with-buffer bf
          (progn
            (kill-region (point-min) (point-max))
            (insert (concat "Project: " pj-name))
            (insert (concat " (" (assocdr 'stage data) ")"
                            "\n"))
            (insert (concat "Description: "
                            (assocdr 'description data)
                            "\n"))
            (insert (concat "Start date: "
                            (assocdr 'start_date data)
                            "\n"))
            (insert (concat "End date: "
                            (assocdr 'end_date data)
                            "\n"))
            (toggle-read-only)
            (pop-to-buffer bf))))))
   (cl-function
    (lambda (&rest args &key error-thrown &allow-other-keys)
      (print x)))))

(provide 'forecastapp-project-view)
;;; forecastapp-project-view.el ends here
