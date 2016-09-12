# rule34-paheal-downloader
A program to download all the images of a given tag from https://rule34.paheal.net

To use, place the program in the folder which you wish to download images to, and invoke the program.

I will hopefully later add support for specifying a directory to download to.

If you don't provide any command line arguments the program will then prompt you for a tag to download from.
Tags must not have spaces in, instead they should have underscores. You can specify a tag on the command line using -t or --tag.

Example: ./rule34downloader --tag "cute_anime_girl"

The -h or --help flag is also supported, if you're having problems.

It's not very fast currently due to the website enforcing a one second per request rule. I will be adding async IO to speed this up a little.

## Installation:

#### Clone the repository
`git clone https://github.com/ZedPea/rule34-paheal-downloader.git`

#### Install dependencies
You will need a couple of dependencies. Ensure you have the haskell platform including cabal installed.
This should be installed from your repositories if available. 

`cabal install concurrent-extra tagsoup http`

Note that cabal is a bit finnicky, if it fails on any of the above modules, try running cabal install failed_module on its own, so if tagsoup failed to install, rerun

`cabal install tagsoup`

#### Compile
Enter the directory which contains the r34downloader.hs file and run

`ghc r34downloader.hs`

Note that some intermediate compile files will be left around, r34downloader.hi and r34downloader.o. You can delete these if you wish.
