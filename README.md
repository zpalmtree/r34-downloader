# rule34-paheal-downloader
A program to download all the images of a given tag from https://rule34.paheal.net [Warning - Not Safe For Work]

Comes with both a GUI and a CMD version.

The website has a one request a second limit, thus users with fast internet connections won't max out their bandwidth.

## Installation:

#### Clone the repository
`git clone https://github.com/ZedPea/rule34-paheal-downloader.git`

#### Compile
`cd src`

`cabal install`

This will install the programs in ~/.cabal/bin

If you add this line to your ~/.profile:
`PATH=$PATH:~/.cabal/bin`
you will be able to run the programs from any bash shell.

Otherwise, you will have to invoke them with ~/.cabal/bin/r34Downloader

#### Running the program
Run

`r34Downloader` or `r34DownloaderGUI`

See the flags section to customize how you use the CMD version of the program.

## Flags:

#### -t / --tag
Specify a tag to download on the command line.

Usage: `r34Downloader --tag=cute_anime_girl`

This will download all images with the tag cute_anime_girl.

#### -f / --first
Specify the amount of images to download. Input should be a positive number.

Usage: `r34Downloader -t=cute_anime_girl --first=10`

This will download the first 10 images from the tag cute_anime_girl, if it exists.

#### -? / --help
Prints a help message.

Usage: `r34Downloader --help`

This will output help on how to use the program.

#### -d / --directory
Lets you select the directory to download the images to. The directory must exist.
If the directory does not exist, the program will fall back to the current working directory.

Usage: `r34Downloader -t=cute_anime_girl --directory=/media/Pictures`

This will download all images with the tag cute_anime_girl to the directory /media/Pictures, if it exists.

If the directory does not exist, it will fall back to the current directory.

#### -s / --search
Lets you search for a tag if you don't know if it exists/the full name.

Usage: `r34Downloader --search=cute`

If a tag "cute_anime_girl" existed, it would be printed to the console, along with any other tags beginning with "cute".
Tag search is not case sensitive, so "cute", would also find tags beginning with "Cute", for example.


#### Windows installation
The setup is the same on windows, I have tried it using a cywgin terminal and it works fine, I have not tried using windows cmd, but I have no reason to believe it would be any different.

GUI setup has not been tested and probably won't work. It will be looked at soon.

Get the haskell platform here: https://www.haskell.org/platform/#windows
