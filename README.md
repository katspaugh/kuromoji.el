kuromoji.el
===========

Plugin for Emacs that fontifies Japanese text using Kuromoji tokenizer
and part-of-speech tagger.

![screenshot](http://i.imgur.com/PLWVewW.png)

### TODO

 * Right now it uses the http://atilika.org/ server, but should support
local installations as well.
 * Display readings as furigana.
 * On-the-fly highlighting.

### Installation

    (require 'kuromoji)

### Usage

Enter some Japanese text into a buffer.

    M-x kuromoji-start
