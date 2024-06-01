# Word counter
This program counts the number of occurrences of each unique word in a given text file.
It outputs the number of occurrences as a list of occurrences and words, sorted first by descending number of occurrences and second by lexicographic order on the words.

A word in this context is a sequence of Unicode characters separated by any type of whitespace.
This means that the program is not opinionated about what words can be.
In particular, punctuation marks and special characters are part of words, such that the program counts e.g. "What" and "What?" separately.

## Running via Docker
If you don't want to compile and run the program on your own computer, you can run it in a Docker container.
To do this you will need to have Docker installed on your machine.

You can either download a ready-to-use Docker image from the release section of the repository or build your own image (see below).
If you have downloaded a Docker image from the release section, you can load it by running
```
docker load -i word-counter-docker-image.tar.gz
```

To use the program, you will need to give the Docker container access to a directory on your machine so it can read files.
You can give the container access to the current working directory and make the program read a file with e.g.:
```
docker run -v .:/outside word-counter /outside/test-data/given-text.txt
```
This should output:
```
2: do
2: that
1: go
1: so
1: thing
1: well
1: you
```

#### Building a Docker image
To build your own Docker image, simply run the following command inside the repository directory:
```
docker build -t word-counter .
```
This will take a while, but you should eventually end up with a Docker image containing the program.

If you need to share your Docker image, you can export it using
```
docker save -o word-counter-docker-image.tar word-counter
```
You can then load the image using the instructions above (even if it is first compressed using e.g. gzip).

## Compilation
The program is implemented in Haskell.

To compile the program you will need the following:
* [The Glasgow Haskell compiler (ghc)](https://www.haskell.org/ghc/) (tested with version 9.4.8)
* [The Cabal build system (cabal)](https://www.haskell.org/cabal/) (tested with version 3.10.3.0)

If you are on a Linux system, you can probably install these through the package manager of your distribution.
Otherwise, you will have to manually install them following the instructions on the pages linked above.
If you are on Windows, you may additionally want a Cygwin installation to get a Unix-like environment.

If the environment is correctly set up, you can compile the program with:
```sh
cabal build
```

## Running with Cabal
You can run the program by providing it with the name of a text file to count words in.
For example, you can run the test example with Cabal:
```sh
cabal run coding-task -- test-data/given-text.txt
```
outputs:
```
2: do
2: that
1: go
1: so
1: thing
1: well
1: you
```

## Testing
The program has a test suite using HSpec and QuickCheck.
You can run the test suite using Cabal:
```sh
cabal test --test-show-details=direct
```

## Assumptions
The program makes some assumptions to simplify its design:
- Files are validly Unicode-encoded
- Files are small enough to fit in memory
- Words can consist of anything, even non-letter forms such as symbols and punctuation
- Texts have less than 2^29 occurrences of each word
- We know nothing about the distribution of words in the text up front

## Implementation
The program is implemented in Haskell with Cabal.
It uses the `hashtables` library for fast hash tables and the `text` library for Unicode text processing.

The program works in the following stages:
- Attempt to read the given file, exiting with an error if this is not possible (or if no file is given)
- Normalize all letter cases in the text
- Split the text into words
- Iterate through the words and update a hash table mapping each word to the number of times it occurs
- Convert the hash table to a list of key-value pairs (i.e. word-count pairs)
- Sort the list by descending count, then by lexicographic ordering on words
- Format the list
- Print the list

If we had more information about the words in the text, we might be able to tweak the approach for better performance.

The program assumes that words can consist of any type of characters (except whitespace).
This assumption was made for portability, since the Haskell libraries for advanced Unicode processing require extra libraries to be available at runtime.
The program assumes that each word occurs fewer than 2^29 times, which allows it to use `Int` instead of `Integer` for performance.

## Performance
The program works in a pipeline consisting of the following steps:
- Case conversion: O(n)
- Word splitting: O(n)
- Counting: each update is in O(1) amortized, so the counting is in O(n)
- Converting hash table to list of key-value pairs: O(n)
- Sorting: O(n log n)
- Output formatting: O(n)

In total, the program runs in O(n log n) time.
