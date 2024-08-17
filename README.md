# Introduction

This emacs package allows you to synchronize exported versions of your org files, by auto-exporting them every time you save. My original reason for this was because I prefer to write docs in org mode, but some git repo sites don't support org mode as a format for their `README` files (looking at you, [sourcehut](https://sr.ht/)). But I don't want to have to remember to export after each change.

This being emacs, there are several ways to do it. One is simply to add a local `after-save-hook` function (here's one that auto-exports to [github-flavoured markdown](https://github.github.com/gfm/) on save):

```
# Local Variables:
# eval: (add-hook 'after-save-hook #'org-gfm-export-to-markdown nil 'local)
# End:
```

Which works, but I find it ugly. It means copy-pasting hard-to-remember code at the end of each org file, instead of declaring the export in the same way as other org declarations. More seriously, it doesn't take narrowing into account: if the file is saved while narrowed, only the narrowed part of the buffer will be exported.

Instead, I want to be able to do something similar to what the [org-auto-tangle](https://github.com/yilkalargaw/org-auto-tangle) packages does for tangling:

```
#+auto_export: gfm
```

That way, I can have the auto-export control for a bunch of org files as part of a common setup file declared by `#+setupfile:`, so I don't have to repeat myself.


# Usage

The package defines a minor mode called `org-autoexport-mode`, which (if enabled) performs the export after saving the file. Simply require the package in you emacs init and hook it into org-mode:

```elisp
(require 'org-autoexport)
(add-hook 'org-mode-hook 'org-autoexport-mode)
```

Alternatively, you can use `use-package`:

```elisp
(use-package org-autoexport
  ;; this line is necessary only if you cloned the repo in your site-lisp directory
  :load-path "site-lisp/org-autoexport/"
  :defer t
  :hook (org-mode . org-autoexport-mode))
```

You can install directly from the source repo if you have [vc-use-package](https://github.com/slotThe/vc-use-package) installed (this package isn't yet available from MELPA):

```elisp
(use-package org-autoexport
  :defer t
  :vc (:fetcher sourcehut :repo zondo/org-autoexport)
  :hook (org-mode . org-autoexport-mode))
```

If the minor mode is on, it will try to automatically export your org files if they contain any `#+auto_export:` options. You can also invoke the export interactively, via the `org-autoexport-do-export()` function.

Auto-export will fail if it the requested export backend can't be found, and you'll get a popup warning buffer to that effect. In that case you will need to install and load the export backend first (for example, to get the `gfm` export mentioned above, you will need to load the [ox-gfm](https://github.com/larstvei/ox-gfm) package).


# Contributing

You can find the source repo on [sourcehut](https://git.sr.ht/~zondo/org-autoexport). Report any problems [here](https://todo.sr.ht/~zondo/org-autoexport).


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
