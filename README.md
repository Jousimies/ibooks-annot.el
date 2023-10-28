# ibooks-annot.el

iBooks-Annot.el 用于提取 Apple Books 中书籍的高亮信息，并将它们保存到 Denote 笔记中。

目前支持 epub 和 PDF 文件 Highlights 的提取。

## Install

``` shell
git submodule add --depth=1 https://github.com/Jousimies/ibooks-annot.el.git
```
## Configuration
``` emacs-lisp
(use-package ibooks-annot
    :load-path "packages/ibooks-annot.el/"
    :bind ("s-n e" . ibooks-annot/choose-book-and-save-to-file)
    :config
    (setq pdfannots-script "/path-to-pdfannots/pdfannots.py -f json")
    (setq ibooks-annot/book-note-directory "/path-to-your-note-directory/))
```

## PDF

需要安装 [pdfannots](https://github.com/0xabu/pdfannots/tree/main/pdfannots/)

``` emacs-lisp
(setq pdfannots-script "/path-to-pdfannots/pdfannots.py -f json")
```
