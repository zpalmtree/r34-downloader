# rule34-paheal-downloader
A program to download all the images of a given tag from https://rule34.paheal.net [Warning - Not Safe For Work]

To use, place the program in the folder which you wish to download images to, and invoke the program.

If you don't provide any command line arguments the program will then prompt you for a tag to download from.

The website has a one request a second limit, thus users with fast internet connections won't max out their bandwidth.

The program does support async IO, so a slow network connection will affect the speed as little as possible, a download should being roughly every second.

## Flags:

#### -t / --tag
Specify a tag to download on the command line.

Usage: `./r34downloader --tag=cute_anime_girl

This will download all images with the tag cute_anime_girl.

#### -f / --first
Specify the amount of images to download. Input should be a positive number.

Usage: `./r34download -t=cute_anime_girl --first=10

This will download the first 10 images from the tag cute_anime_girl, if it exists.

#### -? / --help
Prints a help message.

Usage: `./r34download --help`

This will output help on how to use the program.

#### -d / --directory
Lets you select the directory to download the images to. The directory must exist.
If the directory does not exist, the program will fall back to the current working directory.

Usage: `./r34downloader -t=cute_anime_girl --directory=/media/Pictures

This will download all images with the tag cute_anime_girl to the directory /media/Pictures, if it exists.

If the directory does not exist, it will fall back to the current directory.

#### -s / --search
Lets you search for a tag if you don't know if it exists/the full name.

Usage: `./r34downloader --search=cute

If a tag "cute_anime_girl" existed, it would be printed to the console, along with any other tags beginning with "cute".
Tag search is not case sensitive, so "cute", would also find tags beginning with "Cute", for example.

## Installation:

#### Clone the repository
`git clone https://github.com/ZedPea/rule34-paheal-downloader.git`

#### Install dependencies
You will need a couple of dependencies. Ensure you have the haskell platform including cabal installed.
This should be installed from your repositories if available. 

`cabal install concurrent-extra tagsoup http async`

Note that cabal is a bit finnicky, if it fails on any of the above modules, try running cabal install failed_module on its own, so if tagsoup for example failed to install, rerun

`cabal install tagsoup`

#### Compile
Enter the directory which contains the r34downloader.hs file and run

`ghc r34downloader.hs`

Note that some intermediate compile files will be left around, r34downloader.hi, r34downloader.o, Search.hi, Search.o, Utilities.h, and Utilities.o. You can delete these if you wish.

#### Running the program
Run

`./r34downloader`

in the directory you wish to run the program. See the flags section to customize how you use the program.

#### Windows installation
The setup is the same on windows, I have tried it using a cywgin terminal and it works fine, I have not tried using windows cmd, but I have no reason to believe it would be any different.

Get the haskell platform here: https://www.haskell.org/platform/#windows
