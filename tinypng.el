;;; tinypng.el --- Compress PNG and JPEG with TinyPNG.com  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Xu Chunyang

;; Author: Xu Chunyang <mail@xuchunyang.me>
;; Homepage: https://github.com/xuchunyang/tinypng.el
;; Created: 2019-06-10T11:46:05+08:00
;; Package-Requires: ((emacs "25.1"))
;; Version: 0
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Compress PNG and JEPG via https://tinypng.com/ API.

;;; Code:

(require 'json)
(require 'url)

(defvar tinypng-api-key
  (let ((plist (car (auth-source-search :max 1 :host "api.tinify.com"))))
    (let ((v (plist-get plist :secret)))
      (if (functionp v) (funcall v) v)))
  "Your API key.")

(defun tinypng (from to)
  "Compress .png or jpeg file FROM and save the compressed file as TO."
  (interactive "fCompress .png or .jpeg: \nFSave to: ")
  (with-current-buffer
      (let ((url-request-method "POST")
            (url-request-extra-headers
             `(("Authorization" .
                ,(format "Basic %s" (base64-encode-string (concat "api:" tinypng-api-key))))))              
            (url-request-data (with-temp-buffer
                                (set-buffer-multibyte nil)
                                (insert-file-contents-literally from)
                                (buffer-string))))
        (url-retrieve-synchronously "https://api.tinify.com/shrink"))
    (set-buffer-multibyte nil)
    (goto-char (point-min))
    (re-search-forward "^\r?\n")
    (display-buffer (current-buffer))
    (let-alist (json-read)
      (if .error
          (error "%s: %s" .error .message)
        (url-copy-file .output.url to t)
        (message "%s (%s) -> %s (%s) %s"
                 from (file-size-human-readable .input.size 'iec)
                 to (file-size-human-readable .output.size 'iec)
                 (format "-%.0f%%" (* 100 (- 1 .output.ratio))))))))

(provide 'tinypng)
;;; tinypng.el ends here
