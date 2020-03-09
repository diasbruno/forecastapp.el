;;; forecastapp-sprint-view.el -- forecast.app integration with emacs. -*- lexical-binding: t; -*-
;; Copyright (C) 2020  Bruno Dias

;; Author: Bruno Dias <dias.h.bruno@gmail.com>
;; Url: https://github.com/diasbruno/forecastapp.el
;; Package-requires: ((emacs "25.1") (tablist "1.0") (navigel "0.7.0") (s "1.12.0"))
;; Version: 0.0.1

;;; Commentary:

;; Presents a view of the selected sprint.

;;; Code:

(require 'forecastapp-utils)
(require 'forecastapp-net)

(defun forecastapp--sprint-buffer-name (name)
  "Make the sprint buffer name with sprint NAME."
  (concat "forecastapp-sprint: " name))

(defun forecastapp-open-sprint ()
  "Open a SPRINT view."
  (let ((project *forecastapp--current-project*)
        (sprint *forecastapp--current-sprint*))
    (forecast--get-project-sprint
     (assocdr 'id project)
     (assocdr 'id sprint)
     nil
     (cl-function
      (lambda (&key data &allow-other-keys)
        (print data)
        (let* ((pj-name (assocdr 'name data))
               (bf-name (forecastapp--sprint-buffer-name pj-name))
               (bf (get-buffer-create bf-name)))
          (with-buffer bf
            (progn
              (kill-region (point-min) (point-max))
              (insert (concat "Sprint: " pj-name
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
        (print x))))))

(provide 'forecastapp-sprint-view)
;;; forecastapp-sprint-view.el ends here
