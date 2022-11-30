# MiniPythonParser

This is a project for Mizzou Principles of Programming Languages Fall 2022 that parses a subset of Python.

## Instructions
To use this project you must have [stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/) installed. 

Build the project with `stack build` and run the parser with `stack run < input.py`.

If parsing is successful, this command will generate a parse tree at `parseTree/tree.tex`. To view it as a `.pdf` you can use a compiler like `latexmk` or paste it into [overleaf.com](https://overleaf.com). Each direct child of the start symbol is drawn on its own page to allow for a complete representation of large portions of code without sacrificing readability. 

To run the test suite, do `stack test`. 

## Demo
View my demo video [here](https://youtu.be/S37bwAOr3hk)
