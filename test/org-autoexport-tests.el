;;; -*- lexical-binding: t; -*-

(require 'buttercup)
(require 'org-autoexport)

(describe "org-autoexport"
  (before-all
   (find-file "README.org"))

  (it "finds the backend names in README.org"
      (expect (org-autoexport-get-backends) :to-equal '("gfm" "html")))

  (it "finds the correct filename"
      (expect (org-autoexport-get-filename) :to-equal "README"))

  (it "finds the correct suffix"
      (expect (org-autoexport-get-suffix "html") :to-equal "html")
      (expect (org-autoexport-get-suffix "gfm") :to-equal "md")
      (expect (org-autoexport-get-suffix "latex") :to-equal "tex"))

  (it "finds the backend from its name"
      (expect (org-autoexport-get-backend "html") :not :to-be nil)
      (expect (org-autoexport-get-backend "latex") :not :to-be nil)
      (expect (org-autoexport-get-backend "nosuch") :to-be nil)))
