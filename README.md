## Ian's use-package Emacs configuration

<p align="center">
  <img src="https://upload.wikimedia.org/wikipedia/commons/thumb/0/08/EmacsIcon.svg/120px-EmacsIcon.svg.png" />
</p>

### Thanks for dropping by!
This is my personal Emacs configuration (on GNU Emacs 26.2). It's unopinionated and was created for general use in mind.

This Emacs config starts up **super fast** with an average init time of 0.4 to 0.6 seconds.

### Installation
Back up your `~/.emacs.d/` first (if you have one):

```
mv ~/.emacs.d ~/.emacs.d.bak
```

Git clone my configuration to your new `~/.emacs.d/` :
```
git clone https://github.com/ianpan870102/.use-package.emacs.d.git ~/.emacs.d
```

### "Rolling" Release
I will constantly push new commits since *a real Emacser* is never completely satisfied with his/her setup.

## Packages that I use:

#### Package Manger
- use-package

#### Vim Editing habits (Optional depending on your preferences)
- evil

#### Git integration
- magit

#### Better Emacs defaults
- ido-vertical-mode
- markdown-mode
- exec-path-from-shell
- highlight-operators
- highlight-numbers
- highlight-escape-sequences

#### Lighweight IDE-like features
- company (autocompletion)
- flycheck
- format-all (code formatting)
- yasnippets
- highlight-symbol (highlight things at cursor point)
- dashboard (welcome screen)

### Language Server Protocal (LSP) clients
- eglot (for C and Python)
- lsp-mode, lsp-java, and company-lsp (for Java)

#### Org mode
- org-bullets

#### Terminal habits
- ranger

#### Theme
- doom-themes (doom-tomorrow-night: one of my favorite themes)
- zenburn-theme (low contrast and easy on the eyes)
