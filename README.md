- [Introduction](#intro)
- [Usage](#usage)
- [Installation](#install)
- [Contributing](#contrib)
- [License](#license)

[![img](https://melpa.org/packages/org-autoexport-badge.svg)](https://melpa.org/#/org-autoexport)


<a id="intro"></a>

# Introduction

This emacs package allows you to synchronize exported versions of your [org](https://orgmode.org/) files, by auto-exporting them every time you save.

My original reason for writing it was because I prefer to write docs in org mode, but some forge sites don't support org mode as a format for their `README` files (looking at you, [sourcehut](https://lists.sr.ht/~sircmpwn/sr.ht-discuss/%3Cfe7aa296-9c90-463d-b4e6-50eeb7e57428%40localhost%3E)) and insist on [Markdown](https://www.adamhyde.net/whats-wrong-with-markdown/). But I don't want to have to remember to export after each change.

This being emacs, there are several ways to do it. One is simply to add a local `after-save-hook` function (here's one that auto-exports to [github-flavoured](https://github.github.com/gfm/) Markdown on save):

```
# Local Variables:
# eval: (add-hook 'after-save-hook #'org-gfm-export-to-markdown nil 'local)
# End:
```

This works, but I find it ugly. It means copy-pasting hard-to-remember code at the end of each org file, instead of declaring the export in the same way as other org declarations. More seriously, it doesn't take narrowing or the active region into account: if the file is saved while its buffer is narrowed, or a region is active, only that part of the buffer will be exported.

Instead I want to be able to do something similar to what the [org-auto-tangle](https://github.com/yilkalargaw/org-auto-tangle) package does for tangling, and be able to say:

```
#+auto_export: gfm
```

That way, I can have the auto-export control for a bunch of org files as part of a common setup file declared by `#+setupfile:`, so I don't have to repeat myself.


<a id="usage"></a>

# Usage

The package defines a minor mode called `org-autoexport-mode`, which (if enabled) will try to export to other formats after saving the current org file. It will perform one export for each `#+auto_export:` option it finds. You can also invoke the export process interactively, via the `org-autoexport-do-export()` function.

By default the exported filename is based on the org filename. You can use the `EXPORT_FILE_NAME` file property to override this.

Auto-export will fail if the requested export backend can't be found, and you'll get a popup warning buffer to that effect. In that case you will need to install and load the export backend first (for example, to get the `gfm` export mentioned above, you will need to load the [ox-gfm](https://github.com/larstvei/ox-gfm) package).

If want to suppress export for particular files (e.g., files included in other files via `#+setupfile:`) you can turn autoexport mode off for those files via local variables:

```
# Local Variables:
# org-autoexport-mode: nil
# End:
```


<a id="install"></a>

# Installation

Simply require the package in your emacs init file and hook it into org mode:

```elisp
(require 'org-autoexport)
(add-hook 'org-mode-hook 'org-autoexport-mode)
```

Alternatively, you can install from MELPA via `use-package`:

```elisp
(use-package org-autoexport
  :defer t
  :hook (org-mode . org-autoexport-mode))
```

Autoexport mode adds an `AutoExp` indicator to the mode line to show that it's active. You can change or suppress that using the [diminish](https://www.gnu.org/software/emacs/manual/html_node/use-package/Diminish.html) option of `use-package`.


<a id="contrib"></a>

# Contributing

You can find the source repo on [sourcehut](https://git.sr.ht/~zondo/org-autoexport). Report any problems or suggestions [here](https://todo.sr.ht/~zondo/org-autoexport).


<a id="license"></a>

# License

This package is licensed under the the [2-Clause BSD License](https://opensource.org/license/bsd-2-clause).
