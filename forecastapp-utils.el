;;; forecastapp-utils.el -- forecast.app integration with emacs. -*- lexical-binding: t; -*-
;; Copyright (C) 2020  Bruno Dias

;; Author: Bruno Dias <dias.h.bruno@gmail.com>
;; Url: https://github.com/diasbruno/forecastapp.el
;; Package-requires: ((emacs "25.1") (tablist "1.0") (navigel "0.7.0") (s "1.12.0"))
;; Version: 0.0.1

;;; Commentary:

;; Some utils to help.

;;; Code:

(defalias 'assocdr #'alist-get)

(provide 'forecastapp-utils)
;;; forecastapp-utils.el ends here
