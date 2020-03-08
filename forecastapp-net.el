;;; forecastapp-net.el -- forecastapp-net.app integration with emacs.  -*- lexical-binding: t; -*-
;; Copyright (C) 2020  Bruno Dias

;; Author: Bruno Dias <dias.h.bruno@gmail.com>
;; Url: https://github.com/diasbruno/forecastapp.el
;; Package-requires: ((emacs "25.1") (tablist "1.0") (navigel "0.7.0") (s "1.12.0"))
;; Version: 0.0.1

;;; Commentary:

;; All endpoints available by the Forecast API.

;;; Code:

(require 's)
(require 'dash)
(require 'request)

(defvar *forecast-api-key* nil
  "Your forecast api key.")

(defvar *forecast-api-version* "v1"
  "Change the version of the api.")

;; TODO(dias): needs tests.
(defvar *forecast-host*
  "https://api.forecast.it/api/"
  "Forecast.app's host.")

(defvar *forecast-base-url*
  (concat *forecast-host* *forecast-api-version*)
  "Forecast.app's base url.")

(defun find-api-arguments (uri)
  "Find the arguments of an API URI."
  (let* ((index 0)
         (results nil))
    (while (string-match "{\\(\\w+\\)}" uri index)
      (push (substring uri (match-beginning 1) (match-end 1))
            results)
      (setf index (match-end 1)))
    results))

(defun stringify (value)
  "Convert VALUE of basic type into string."
  (typecase value
    (integer (int-to-string value))
    (t value)))

(defun uri-template (label)
  "Build uri template for LABEL."
  (concat "{" label "}"))

(defun url-builder (url args syms)
  "Build from URL with ARGS and SYMS."
  (let ((url-built url))
    (loop for attr in args
          for value in syms
          do (setf url-built (s-replace (uri-template attr)
                                        (stringify value)
                                        url-built)))
    url-built))

(defmacro build-endpoint (name method uri)
  "Create the endpoint of NAME for URI and METHOD."
  (let* ((endpoint (concat *forecast-base-url* uri))
         (realname (concat "forecast--" (symbol-name name)))
         (args (find-api-arguments endpoint))
         (sym-args (mapcar #'make-symbol args)))
    `(defun ,(intern realname) (,@sym-args data on-succ on-err)
       (let ((url (url-builder ,endpoint
                               (list ,@args)
                               (list ,@sym-args))))
         (request url
           :parser 'json-read
           :type ,(symbol-name method)
           :data data
           :error on-err
           :success on-succ)
         nil))))

;; allocations
(build-endpoint get-allocations GET "/allocations")
(build-endpoint get-allocation GET "/allocations/{allocationId}")
(build-endpoint create-allocation POST "/allocations")
(build-endpoint update-allocation PUT "/allocations/{allocationId}")
(build-endpoint delete-allocation DELETE "/allocations/{allocationId}")
;; tasks
(build-endpoint get-tasks GET "/tasks")
(build-endpoint get-project-tasks GET "/projects/{projectId}/tasks")
(build-endpoint get-task GET "/tasks/{taskId}")
(build-endpoint get-comments-of-task GET "/tasks/{taskId}/comments")
(build-endpoint create-task POST "/tasks")
(build-endpoint update-task PUT "/tasks/{taskId}")
(build-endpoint delete-task DELETE "/tasks/{taskId}")
(build-endpoint get-deleted-tasks GET "/tasks/deleted")
;; projects
(build-endpoint get-projects GET "/projects")
(build-endpoint get-project GET "/projects/{projectId}")
(build-endpoint create-project POST "/projects")
(build-endpoint update-project PUT "/projects/{projectId}")
(build-endpoint delete-project DELETE "/projects/{projectId}")
(build-endpoint get-project-status GET "/projects/{projectId}/statuses")
;; milestones
(build-endpoint get-project-milestones GET "/projects/{projectId}/milestones")
(build-endpoint get-project-milestone GET "/projects/{projectId}/milestones/{milestoneId}")
(build-endpoint create-project-milestone POST "/projects/{projectId}/milestones")
(build-endpoint update-project-milestone PUT "/projects/{projectId}/milestones/{milestoneId}")
(build-endpoint delete-project-milestone DELETE "/projects/{projectId}/milestones/{milestoneId}")
;; sprints
(build-endpoint get-project-sprints GET "/projects/{projectId}/sprints")
(build-endpoint get-project-sprint GET "/projects/{projectId}/sprints/{sprintId}")
(build-endpoint create-project-sprint POST "/projects/{projectId}/sprints")
(build-endpoint update-project-sprint PUT "/projects/{projectId}/sprints/{sprintId}")
(build-endpoint delete-project-sprint DELETE "/projects/{projectId}/sprints/{sprintId}")
;; clients
(build-endpoint get-clients GET "/clients")
(build-endpoint get-client GET "/clients/{clientId}")
(build-endpoint create-client POST "/clients")
(build-endpoint update-client PUT "/clients/{clientId}")
(build-endpoint delete-client DELETE "/clients/{clientId}")

(provide 'forecastapp-net)
;;; forecastapp-net.el ends here
