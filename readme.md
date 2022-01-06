# OCaml project (I4IRIF11)
Working directory for the 4IR OCaml project. This project was carried out in the fall of 2021 as part of the fundamental computer science course and consists of the implementation of a Ford Fulkerson algorithm in Ocaml (functional programming).  

## Project progress 
* Minimal acceptable project: **done** (run `make demo`)
* Medium project (Money sharing): **done** (run `make share`)
* Better project: 
  * Max-flow min-cost: **done** (see `find_path` in `fordfulkerson.ml`)
  * Enhance the medium project: **done** (CSV parsing + addition of repartition possibilities, cf. [better_project.md](better_project.md))

## Medium project
We have chosen to implement the money sharing project. The implementation has been carried out and the demonstrator takes as input a processing file: 
```
p alice 50.0
p bob 30.0
p carol 15.0
p dan 5.0
```
Each row represents each person (starts with a p), with their name and then the amount the person paid. The program uses Ford Fulkerson's algorithm to determine cost balancing: 
```
dan owes 5.00 to bob
dan owes 15.00 to alice
carol owes 10.00 to alice
```
The program also displays some useful information to verify the correct functioning of the program (in simple cases). 

## Better Project
See [better_project.md](better_project.md).

## Quick start
### Fordfulkerson demo (Minimal acceptable project)
```
git clone https://github.com/Manah7/projet_ocaml
cd projet_ocaml
make image
eog image.svg
```

### Money sharing demo (Medium project)
```
git clone https://github.com/Manah7/projet_ocaml
cd projet_ocaml
make share
eog image.svg
```

### CSV parsing & weighted value (Better project)
```
git clone https://github.com/Manah7/projet_ocaml
cd projet_ocaml
make csvtest
```

## Makefile
```
make build  # build the project

make image   # build and run a demo, saved as image.svg
make share   # build and run money sharing demo using FF. algo.
make csvtest # build and run CSV parsing demo

make clean 
make float  # Used for debug
```

## About
Contact & more: [Manah](https://manah.fr), [Enjmateo](https://github.com/Enjmateo)
