;;; org-autoexport.el --- Auto-export org file on save -*- lexical-binding: t; -*-

;; Author: Glenn Hutchings <zondo42@gmail.com>
;; Maintainer: Glenn Hutchings <zondo42@gmail.com>
;; URL: https://git.sr.ht/~zondo/org-autoexport
;; Version: 1.1
;; Keywords: org, wp
;; Package-Requires: ((emacs "29.1") (org "9.6"))
;; 
;; This file is not part of GNU Emacs.

;; Copyright (c) 2025, Glenn Hutchings
;; 
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:
;; 
;; 1. Redistributions of source code must retain the above copyright notice, this
;;    list of conditions and the following disclaimer.
;; 
;; 2. Redistributions in binary form must reproduce the above copyright notice,
;;    this list of conditions and the following disclaimer in the documentation
;;    and/or other materials provided with the distribution.
;; 
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
;; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
;; CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
;; OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; Commentary:

;; It is common to want to export org files to one or more other formats
;; every time you save your changes.  This package this allows you to do so
;; using #+auto_export: options in the org file.

;;; History:

;; Version 1.0 (22 Aug 2024):
;;    First release
;; 
;; Version 1.1 (30 Nov 2024):
;;    Deactivate mark before exporting
;;    Support the EXPORT_FILE_NAME file property

;;; Code:

(require 'org)
(require 'ox)

(defconst org-autoexport-backend-suffix-map
  '(("gfm" . "md")
    ("latex" . "tex"))
  "Mapping of export backend name to file suffix.

Most of the time, the name and suffix are the same.  This variable lists
the special cases where they are different.")

(defconst org-autoexport-function-template-map
  '(("pandoc" . "org-pandoc-export-to-%s"))
  "Mapping of export backend name to export function template.

These are special cases that use explicit export functions instead of
backends.  Each template must contain a placeholder for the export
format name.")

(defun org-autoexport-get-backend-info ()
  "Get a list of auto-export backend information from the current file.

This is the list of items declared by #+auto_export:
keywords in the org file.  Each item is in the format

    NAME [FORMAT]

where NAME is the backend name and the optional FORMAT is the output
format required.  If FORMAT is not specified, it's assumed that the
format is the same as the backend name."
  (cdar (org-collect-keywords '("AUTO_EXPORT"))))

(defun org-autoexport-get-backend (backend-name)
  "Return the export backend used to autoexport using BACKEND-NAME."
  (org-export-get-backend (intern backend-name)))

(defun org-autoexport-get-suffix (backend-name)
  "Return the file suffix used to autoexport using BACKEND-NAME.

Default is the name of the backend itself, unless a special case
is found in `org-autoexport-backend-suffix-map'."
  (alist-get backend-name org-autoexport-backend-suffix-map backend-name nil 'equal))

(defun org-autoexport-get-filename ()
  "Return the export filename used for auto-export.

Use the EXPORT_FILE_NAME file property if set; otherwise the
basename of the current buffer's filename."
  (let ((propname (org-collect-keywords '("EXPORT_FILE_NAME")))
        (bufname (buffer-file-name)))
    (cond (propname
           (file-name-base (cadar propname)))
          (bufname
           (file-name-base bufname))
          (t
           (error "Buffer has no associated filename or EXPORT_FILE_NAME property")))))

(defun org-autoexport-get-function-template (backend-name)
  "Return the function template used to autoexport using BACKEND-NAME."
  (alist-get backend-name org-autoexport-function-template-map nil nil 'equal))

;;;###autoload
(defun org-autoexport-do-export ()
  "Export the current org file to one or more backends if required.

The backends are listed in the #+auto_export: directives.  If a backend
or export function is unknown, a warning is written to the *Warnings* buffer.

Buffer restrictions are ignored when autoexporting."
  (interactive)
  (save-restriction
    (save-mark-and-excursion
      (widen)
      (let (backend suffix path msg name func fmt tmpl)
        (dolist (backend-info (org-autoexport-get-backend-info))
          (setq name (car (string-split backend-info)))
          (setq fmt (cadr (string-split backend-info)))
          (setq tmpl (org-autoexport-get-function-template name))
          (cond (tmpl
                 ;; Special-case export function.
                 (if (not fmt)
                     (warn "Export to %s requires a format argument" name)
                   (setq func (format tmpl fmt))
                   (if (intern-soft func)
                       (funcall (intern func))
                     (warn "No %s export function called '%s'" name func))))
                (t
                 ;; Standard backend using org-export-to-file.
                 (if fmt
                     (warn "Export to %s does not require a format argument" name)
                   (setq suffix (org-autoexport-get-suffix name))
                   (setq backend (org-autoexport-get-backend name))
                   (setq path (concat (org-autoexport-get-filename) "." suffix))
                   (if (not backend)
                       (warn "No export backend for '%s'" name)
                     (setq msg (format "Exporting %s to '%s'" name path))
                     (message "%s..." msg)
                     (org-export-to-file backend path nil)
                     (message "%s...done" msg))))))))))

;;;###autoload
(define-minor-mode org-autoexport-mode
  "Automatically export Org mode files with #+auto_export options."
  :lighter " AutoExp"

  (if org-autoexport-mode
      (add-hook 'after-save-hook #'org-autoexport-do-export nil 'local)
    (remove-hook 'after-save-hook #'org-autoexport-do-export 'local)))

(provide 'org-autoexport)

;;; org-autoexport.el ends here
