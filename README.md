# ibooks-annot.el

iBooks-Annot.el 用于提取 Apple Books 中书籍的高亮信息，并将它们保存到 Denote 笔记中。

目前支持了 epub 和 PDF 文件内容的提取。

## Install

``` shell
git submodule add --depth=1 https://github.com/Jousimies/ibooks-annot.el.git
```

``` emacs-lisp
(use-package ibooks-annot
    :load-path "packages/ibooks-annot.el/"
    :bind ("s-n e" . ibooks-annot/choose-book-and-save-to-file)
    :config
    (setq ibooks-annot/book-note-directory (expand-file-name "denote/books" my-galaxy)))
```

## PDF

需要安装 [pdfannots](https://github.com/0xabu/pdfannots/tree/main/pdfannots/)
