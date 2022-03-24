# Usage

agregat INPUT [-f FILTER]

# Building
## Prerequisites
This Program is written in Haskell. To compile it, make sure you fulfill
following prerequisites:

- Glasgow Haskell Compiler is installed.
- The cabal command line utility is installed.
- You have issued a "cabal update" or the required libraries are installed
  through the operating system.

## Get the code
Download the code either as a tarball through the GitHub interface or 
clone it by ssh.

git clone git@github.com:zral96/agregat.git

# Compiling
Change into the directory of the project and issue the following command
to get an executable version of the program:

cd agregat
cabal build
