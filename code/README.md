# CS6820 Project: Sampling Random Spanning Trees
## Mark Moeller (mdm367), Goktug Saatcioglu (gs725)

This is our implementation for our CS6820 project as discussed in the Implementation section of the submitted paper.

### Overview
  
The program has been written in OCaml and uses the [Core](https://opensource.janestreet.com/core/) and [Core_bench](https://github.com/janestreet/core_bench) libraries. The program structure, where everything is located under the folder `src`, is as follows:

* [`bin`](./src/bin/) contains the entry point for the program and implements the main logic including command line argument parsing.
* [`parser`](./src/parser/) contains the parsing logic that reads a file and converts the contents of that file into a adjacency list form of the graph (see below for input file format).
* [`random_tree`](./src/random_tree/) implements Wilson's two algorithms for sampling a random spanning tree from a graph, the module exposes two functions:
  1. `with_root` which takes an integer and the graph and returns a spanning tree, and
  2. `without_root` which takes a graph and returns a spanning tree. 
* [`kruskal`](./src/kruskal/) implements Kruskal's minimum spanning tree algorithm along with naive sampling of spanning tree using edge-randomized Kruskal's algorithm, the module exposes four functions:
  1. `det_kruskal` which takes a graph and returns a minimum spanning tree by sorting the edges by cost,
  2. `simp_rand_kruskal` which takes a graph and returns a spanning tree by first sorting the edges by cost and then taking a random permutation of the sorted edges,
  3. `prop_rand_kruskal` which takes a graph and return a spanning tree by picking a permutation of the edges by taking into account the probability distribution created by the edge weights, and
  4. `gen_all_sts` wich takes a graph and returns the set of all spanning trees along with their weights.
* [`tester`](./src/tester/) implements our testing suite and exposes a single function anmed `test` which takes in a file name and runs a series of tests.

### Build Instructions

This program was written and tested with OCaml `4.11.0` and while the OCaml language claims backwards compatibility, it is best to run the program under this version of OCaml.

Instructions on installing OCaml on various systems is given [here](https://ocaml.org/docs/install.html). You should also install the OCaml package manager OPAM with your OCaml install.

The easiest way to satisfy all OCaml-related installation requirements is to install the OCaml package manager OPAM and then execute the following commands
  
```bash
opam switch 4.11.0
opam install -y ocamlbuild
eval $(opam config env) 
```

You will also need to install some libraries to get everything working. You can do this as follows.

```bash
opam install dune core core_bench
```

To compile the program run

```Makefile
make tree_sampler
```

which will produce a file named `./tree_sampler`.

### Running the Program

After building the program you will get the executable `tree_sampler`. You can execute the program by running the executable along with the following command line arguments

* `-fname [filename/string]` - required field, specify the file to read from;
* `kruskal-proportional-random` - optional flag, take the input file and run the naive proportional Kruskal algorithm,
* `-kruskal-simple-random` - optional flag, take the input file and run the naive simple random Kruskal algorithm,
* `-kruskal` - optional flag, take the input file and run Kruskal's minimum spanning tree algorithm,
* `-test [iters/int]` - optional field, take the input file and run the test suite where each test is run `iters` many times,
* `-wilson-no-root` - optional flag, take the input file and run Wilson's unrooted algorithm,
* `-wilson-root [root/int]` - optional field, take the input file and run Wilson's rooted algorithm with root `root`.
* `-help`, `--help`     - prints help messages.

Note that out of all of the optional fields and flags, you must specify one of them and can only specify only one.

The graph to be analyzed by the program is inputted to the program as a file. This file should be formatted as follows.

```
n
m
i1,j1,p1
...
im,jm,pm
```

Here we specified a graph that has n vertices and m edges. After the first two lines, the program expects to read m triples (read lines) of the form `i,j,p` which specifies an edge `(i,j)` with weight `p`. No input validation is done on this file so you must manually make sure that you are following the required formats.

Now, for example, we can run Wilson's rooted algorithm with root `2` using `tree_Sampler` on the file `test1` by doing

```bash
./gossip -fname test1 -wilson-root 2
```

This will cause the program to create a new output file named `test1_wilson_root2_out`. The output file will have on each line an edge of the spanning tree specified in the format `(i,j)`.

### Running the Test Suite

We can also run the test suite on `test1` where each test is executed `1000` times by running the command

```bash
./gossip -fname test1 -test 1000
```

This will run every function described in the Overview section for `1000` times and collect both distribution and running time data. The average running time will be printed on the screen after the program finishes execution while the experimental distribution, i.e., which spanning trees were sampled and how frequently, will be located in a file named `test1_test_distributions`. Furthermore, some auixliary files with the suffix `_stats` will be generated which give a more detailed look at the running time of each sampling method. You may ignore these for now but they could be useful for future analysis if we ever run more complicated tests.
