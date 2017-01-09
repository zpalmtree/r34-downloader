# rule34-paheal-downloader
A program to download all the images of a given tag from https://rule34.paheal.net [Warning - Not Safe For Work]

Comes with both a GUI and a CMD version.

The website has a one request a second limit, thus users with fast internet connections won't max out their bandwidth.

## Installation:

#### Clone the repository
`git clone https://github.com/ZedPea/rule34-paheal-downloader.git`

or download the folder as a zip and unzip it.

#### Compile
Move to the directory you downloaded the repo to.

`cd rule34-paheal-downloader/src`

`cabal install`

This will install the programs in ~/.cabal/bin

If you add this line to your ~/.profile:
`PATH=$PATH:~/.cabal/bin`
you will be able to run the programs from any bash shell.

Otherwise, you will have to invoke them with ~/.cabal/bin/r34Downloader

#### Running the program
Run

`r34Downloader` or `r34DownloaderGUI`

The GUI version should be pretty self explanatory. Enter a tag, search for it,
select your chosen tag from the results, then choose the directory to download
to, and download it.

See the flags section to customize how you use the CMD version of the program.
It will prompt you for a tag if you don't provide any arguments.

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

Getting the program to work on windows will probably be difficult. You will have to install gtk3 which I believe doesn't compile on windows with cabal due to a cairo issue. Once you get that sorted out I assume it will work. I'l try to have a look at it at some point so I can provide a list of commands to get it working.
