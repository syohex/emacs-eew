;;; eew.el --- Japanese Earthquake Early Warning for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2014 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-eew
;; Version: 0.01
;; Package-Requires: ((cl-lib "0.5"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'cl-lib)

(defvar eew--last-date nil)

(defconst eew--api-url-base
  "http://api.p2pquake.net/userquake")

(defsubst eew--date-format (&optional date)
  (format-time-string "date=%m/%d" date))

(defun eew--api-url ()
  (concat eew--api-url-base "?" (eew--date-format)))

(defun eew--construct-data (time code detail-str)
  (let ((details (split-string detail-str "/" t)))
    (list :time time :code code
          :date (cl-first details)
          :intensity (cl-second details)
          :tsunami (string-to-number (cl-third details))
          :info-type (string-to-number (cl-fourth details))
          :focus (cl-fifth details)
          :depth (cl-sixth details)
          :magnitude (cl-seventh details)
          :has-correction (string-to-number (cl-eighth details))
          :latitude (cl-ninth details)
          :longitude (cl-tenth details))))

(defun eew--parse-response (proc-buf)
  (with-current-buffer proc-buf
    (decode-coding-region (point-min) (point-max) 'shift_jis-dos)
    (goto-char (point-min))
    (let (results)
      (while (not (eobp))
        (let* ((line (buffer-substring-no-properties
                      (line-beginning-position) (line-end-position)))
               (elements (split-string line "," t))
               (code (cl-second elements)))
          (when (string= code "QUA")
            (push (eew--construct-data
                   (cl-first elements) code (cl-third elements))
                  results))
          (forward-line 1)))
      results)))

(defun eew--show-information (info)
  (message "地震速報: %s頃, %sで震度 %sの地震が発生しました"
           (plist-get info :date)
           (plist-get info :focus)
           (plist-get info :intensity)))

(defun eew--get-information ()
  (let* ((url (eew--api-url))
         (show-force current-prefix-arg)
         (proc-buf (get-buffer-create " *eww-api-get*"))
         (coding-system-for-read 'binary)
         (coding-system-for-write 'binary)
         (proc (start-process "eew-api-get" proc-buf "curl" "--silent" url)))
    (set-process-sentinel
     proc
     (lambda (proc _event)
       (when (eq (process-status proc) 'exit)
         (if (not (= (process-exit-status proc) 0))
             (message "Error: Can't get API response")
           (let ((latest (car (eew--parse-response proc-buf))))
             (when (or show-force (not (equal eew--last-date latest)))
               (eew--show-information latest))
             (setq eew--last-date latest)
             (kill-buffer proc-buf))))))))

;;;###autoload
(defun eew ()
  (interactive)
  (eew--get-information))

(provide 'eew)

;;; eew.el ends here
