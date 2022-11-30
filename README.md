# MiniPythonParser

This is a project for my Mizzou Principles of Programming Languages class.

The goal is to write a parser for a small subset of python.

## Instructions
To use this project you must have [stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/) installed. 

To run the test suite, do `stack test`. 

To run the parser on MacOS or Linux, do `stack run < input.py`. \
If parsing is successful this command will generate a parse tree at `parseTree/tree.tex`. To view it as a `.pdf` you can use a compiler like `latexmk` or paste it into [overleaf.com](https://overleaf.com)

The parse tree is formatted with each of the start symbol's direct children drawn as seperate trees. This is so that many line programs can be represented. If a single expression is too long or too deeply nested it may not all fit on the page.
