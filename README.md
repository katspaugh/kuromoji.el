kuromoji.el
===========

A package for Emacs that fontifies Japanese text using Kuromoji
tokenizer and part-of-speech tagger.

![screenshot](http://i.imgur.com/fJBIMZf.png)

### Installation

[Download Kuromoji](https://github.com/atilika/kuromoji/downloads) and
put the jar file to some directory. Install Java if you haven't already.

Put this into your Emacs init file:

    (require 'kuromoji)
    (setq kuromoji-jar-path "/path/to/kuromoji-0.7.7.jar")

### Usage

Enter some Japanese text into a buffer. Run the analyzer:

    M-x kuromoji-start

To stop the Kuromoji process, either kill the `*Kuromoji Process*` buffer, or:

    M-x kuromoji-stop
