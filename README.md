# Generate Pandoc files from Coq (v2md)

The idea is just to generate pandoc files from Coq (.v) files.

Build & Install

````
stack build
stack install
stack
````

Using it
````
v2md Preface.v -o Preface.md
````

Other generators
- https://github.com/mzp/condoc
