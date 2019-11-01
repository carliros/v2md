# Generate Pandoc files from Coq (v2md)

The idea is just to generate pandoc files from Coq (.v) files.

## Build & Install

````
stack build
stack install
````

## Using it
````
v2md Preface.v -o Preface.md
````

## TODO
- Get rid of `Data.String.Conversions` and all `cs` uses. Better to use
  `Data.Text` directly?


## Other generators
- https://github.com/mzp/condoc
