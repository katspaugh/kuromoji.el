kuromoji.el
===========

Plugin for Emacs that fontifies Japanese text using Kuromoji tokenizer
and part-of-speech tagger.

*Right now it abuses the http://atilika.org/ server, but should
 support local installations as well.*

![screenshot](http://i.imgur.com/fJBIMZf.png)

### Installation

    (require 'kuromoji)

### Usage

Enter some Japanese text into a buffer.

    M-x kuromoji-start

### Todo

 * Display readings as furigana.
 * On-the-fly highlighting.
