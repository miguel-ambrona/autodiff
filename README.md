
# AutoDiff

## Installation

*1*. Install [Opam](https://opam.ocaml.org/).

 * In Ubuntu,

~~~~~
apt-get install -y ocaml ocaml-native-compilers opam m4 camlp4-extra
~~~~~

 * In OS X, use homebrew,

~~~~~
brew install opam
~~~~~

*2*. Install the necessary dependencies (substitute CLONED_DIR by the directory where you
     cloned the repository):

~~~~~
opam pin add autodiff CLONED_DIR -n
opam install autodiff --deps-only
~~~~~

*3*. Run `make` to compile the tool, which will create two executables:

  * test.native: can be used to check the universality of distinguisher, for example, by executing `./test.native examples/check-universality/attacks/F-5.ind`

  * search.native: can be used to automatically search for attacks. It takes two arguments, the first is either 1 or 2, referring to the heuristic to be used. The second is the input file describing the primitive to be analyzed.

*4*. To reproduce our tests:

  * Verifying attacks: Go to folder /examples/check-universality and run `python3 run_tests.py`.

  * Searching for attacks: Go to folder /examples/attack-search and run `python3 run_tests.py`.

  * Finding collisions to hash functions: From the root folder, run `./test.native examples/collisions/unification.txt`.