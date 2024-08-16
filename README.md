- [Introduction](#orga8fd57f)
- [Usage](#orgb50eed5)
- [Implementation](#org23acfba)
- [License](#org5d0ea09)



<a id="orga8fd57f"></a>

# Introduction

This emacs package allows you to synchronize exported versions of your org files, by auto-exporting them every time you save. My original reason for this was because I prefer to write docs in org mode, but some git repo sites don't support org mode as a format for their `README` files (looking at you, [sourcehut](https://sr.ht/)). But I don't want to have to remember to export after each change.

This being emacs, there are several ways to do it. One is simply to add a local `after-save-hook` function (here's one that auto-exports to [github-flavoured markdown](https://github.github.com/gfm/) on save):

```
# Local Variables:
# eval: (add-hook 'after-save-hook #'org-gfm-export-to-markdown nil 'local)
# End:
```

Which works, but I find it ugly. It means copy-pasting hard-to-remember code at the end of each org file, instead of declaring the export in the same way as other org declarations. Instead, I want to be able to do something similar to what the [org-auto-tangle](https://github.com/yilkalargaw/org-auto-tangle) packages does for tangling:

```
#+auto_export: gfm
```

That way, I can have the auto-export control for a bunch of org files as part of a common setup file declared by `#+setupfile:`, so I don't have to repeat myself.


<a id="orgb50eed5"></a>

# Usage

Simply require the package in you emacs init and hook it into org-mode.

```elisp
(require 'org-autoexport)
(add-hook 'org-mode-hook 'org-autoexport-mode)
```

or you can use `use-package`:

```elisp
(use-package org-autoexport
  ;; this line is necessary only if you cloned the repo in your site-lisp directory
  :load-path "site-lisp/org-autoexport/"
  :defer t
  :hook (org-mode . org-autoexport-mode))
```

If the minor mode is on, it will try to automatically export your org files if they contain any `#+auto_export:` options.

Auto-export will fail if it doesn't recognize the requested export backend, and you'll get a warning to that effect. In that case you will need to install and load


<a id="org23acfba"></a>

# Implementation

Each of the `#+auto_export:` statements declares an org export *backend* that does the export. With that in mind, here's an outline of the export algorithm:

1.  Get the export backend names from the `#+auto_export:` statements in the current file.

2.  For each backend, do this:
    -   Find the suffix of the file to export to. For most backends, that's just the name of the backend. But there are special cases: for [github-flavoured markdown](https://github.github.com/gfm/) the backend is `'gfm` but the suffix is `md`.
    
    -   Create the export filename by concatenating the current file's prefix with the export suffix.
    
    -   Get the export backend object from its string representation.
    
    -   If the backend is found, do the export. Otherwise, warn the user.

3.  Er&#x2026; that's it.

Here's a function to get the backend names, using `org-collect-keywords`:

```elisp
(defun org-autoexport-get-backends ()
  "Get a list of backend names to auto-export from the current file.

This is the list of backend names declared by #+auto_export:
keywords in the org file."
  (delete "AUTO_EXPORT" (car (org-collect-keywords '("AUTO_EXPORT")))))
```

We can test that on the current file:

```elisp
(org-autoexport-get-backends)
```

Here's the result:

    ("gfm" "html")

To map the backend names to the right suffix, we'll use an alist which lists the special cases:

```elisp
(defconst org-autoexport-backend-suffix-map
  '(("gfm" . "md")
    ("latex" . "tex"))
  "Mapping of export backend name to file suffix.

Most of the time, the name and suffix are the same.  This
variable lists the special cases where they are different.")
```

And a function which uses this, defaulting to the backend name:

```elisp
(defun org-autoexport-get-suffix (backend-name)
  "Return the file suffix used to autoexport using BACKEND-NAME.

Default is the name of the backend itself, unless a special case
is found in `org-autoexport-backend-suffix-map'."
  (alist-get backend-name org-autoexport-backend-suffix-map backend-name nil 'equal))
```

Let's test it:

```elisp
(let (suffix (result ""))
  (dolist (backend-name (org-autoexport-get-backends) result)
    (setq suffix (org-autoexport-get-suffix backend-name))
    (setq result (concat result (format "Backend '%s' -> '%s'\n" backend-name suffix)))))
```

The value of `result` is:

    Backend 'gfm' -> 'md'
    Backend 'html' -> 'html'

Next we need a function to look up the backend object given its name, defaulting to `nil` if not found:

```elisp
(defun org-autoexport-get-backend (backend-name)
  "Return the export backend used to autoexport using BACKEND-NAME."
  (org-export-get-backend (intern backend-name)))
```

Does it work?

```elisp
(let ((result "") found)
  (dolist (name '("gfm" "html" "md" "latex" "docx") result)
    (cond ((org-autoexport-get-backend name)
           (setq found "found"))
          (t
           (setq found "not found")))
    (setq result (concat result (format "Backend '%s' %s\n" name found)))))
```

The value of `result` is:

    Backend 'gfm' found
    Backend 'html' found
    Backend 'md' found
    Backend 'latex' found
    Backend 'docx' not found

Here's the function which puts it all together, and does the exporting:

```elisp
(defun org-autoexport-do-export ()
  "Export the current org file to one or more backends if required.

The backends are listed in the #+auto_export: directives.  If a backend
is unknown, a warning is written to the *Warnings* buffer."

  (let (backend suffix filename)
    (unless (buffer-file-name)
      (error "Buffer has no associated filename"))
    (dolist (backend-name (org-autoexport-get-backends))
      (setq suffix (org-autoexport-get-suffix backend-name))
      (setq backend (org-autoexport-get-backend backend-name))
      (setq filename (concat (file-name-base (buffer-file-name)) "." suffix))
      (cond (backend
             (org-export-to-file backend filename))
            (t
             (warn "No export backend for '%s'" backend-name))))))
```

Next we need a minor autoexport mode, which (if enabled) does the exporting. The idea here is to have this turned on in `org-mode-hook`.

```elisp
(define-minor-mode org-autoexport-mode
  "Automatically export Org mode files with #+auto_export options."
  :lighter " Exp"

  (if org-autoexport-mode
      (add-hook 'after-save-hook #'org-autoexport-do-export nil 'local)
    (remove-hook 'after-save-hook #'org-autoexport-do-export 'local)))
```

And that's it. If you're looking at the org mode source of this file, you'll see that the source code is tangled directly from it.


<a id="org5d0ea09"></a>

# License

This package is licensed under the the 2-Clause BSD License:

```text
Copyright (c) 2024, Glenn Hutchings

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
```
