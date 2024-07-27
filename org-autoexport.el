;; org-autoexport.el --- auto-export org file on save -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Glenn Hutchings
;;
;; Author: Glenn Hutchings <zondo42@gmail.com>
;; Maintainer: Glenn Hutchings <zondo42@gmail.com>
;; Created: July 27, 2024
;; Modified: TODO
;; Version: 0.1
;; Keywords: tools
;; Homepage: TODO
;; Package-Requires: ((emacs "28.1") (org "9.6"))
;;
;; This file is not part of GNU Emacs.
;;
;;; TODO: copyright notice
;;
;;; Commentary:
;;
;;  TODO: write commentary
;;
;;; Code:

;;;; Customizable stuff.

(defgroup org-autoexport nil
  "autoexport org files on when saving."
  :prefix "org-autoexport-"
  :group 'org)

(defcustom org-autoexport-export-if-exists nil
  "Auto-export only if the exported file already exists.

If this is set, you must start the autoexport process by first
creating the exported file in the same directory as the org
file (e.g., by exporting it interactively).")

(defun org-autoexport--do-export ()
  "Export current org file if conditions are right."
  ;; TODO: implement
  )
