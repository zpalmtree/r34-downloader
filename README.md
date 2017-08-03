# rule34-paheal-downloader
A program to download all the images of a given tag from https://rule34.paheal.net [Warning - Not Safe For Work]

Comes with both a GUI and a CMD version.

The website has a one request a second limit, thus users with fast internet connections won't max out their bandwidth.

![Screenshot of program](r34downloader.png?raw=true "Screenshot of program")

GUI is being remade with QT Quick / QML. The current UI looks nicer, and will hopefully build on windows without too much issue.

Currently only search is functional. Use the master branch right now.

## Installation:

#### Install prerequistes
You need stack installed, along with the qt quick controls and qt quick controls2.

##### Debian based:
`sudo apt-get install haskell-stack qml-module-qtquick-controls qml-module-qtquick-controls2`

##### Arch based:
`sudo pacman -S stack qt5-quickcontrols qt5-quickcontrols2`

Commands should be similar for other distributions.

#### Clone the repository
`git clone https://github.com/ZedPea/rule34-paheal-downloader.git`

`cd rule34-paheal-downloader`

Alternatively, download the repo as a zip, unzip it, and move into the directory.

#### Compile
`stack install`

If you get an error about your version of stack being too old, run 

`stack update --git` 

and then run 

`~/.local/bin/stack install`, assuming that is where stack installed itself to.

Alternatively, source a version of stack-1.0.0 or higher, and use that instead.

This is due to older version of stack being unable to parse GHC 8 output.

##### Windows [Currently broken]:
Get stack here: https://www.stackage.org/stack/windows-x86_64-installer

Run the stack executable with the default options.

Get the qt quick controls here: http://download.qt.io/official_releases/online_installers/qt-unified-windows-x86-online.exe

For the qt setup, skip the account creation, and deselect everything but MinGW under the most recent version of Qt. Note down the version number, you'll need to edit some commands with the correct version later.

Finish the install.

Download the repo as a zip, unzip it, and move into the directory, then open a terminal there.

Navigating to the directory in explorer, holding shift, and right clicking
"Open command window here" lets you easily get a terminal in the right spot.

`set PATH=%PATH%;C:\qt\5.9.1\mingw53_32\bin\`

It might not be 5.9.1, depends on what version you installed.

`stack install --extra-include-dirs=C:\Qt\5.9.1\mingw53_32\include\ --extra-lib-dirs=C:\Qt\5.9.1\mingw53_32\lib\`

Again, might not be 5.9.1.

This is as far as I've got before it dies with an error about realgcc.exe. I think it's because the gcc isn't taking input from the standard input for some reason. Not sure though.

Obviously, this is quite length and error-prone. Once i've finished rewriting the GUI I'll try and make both linux and windows binaries so you don't have to bother with all this.

#### Run

Either add ~/.local/bin to your path and run the executable of your choice:

`r34Downloader`
`r34DownloaderGUI`

Or, run

`stack exec r34Downloader`
`stack exec r34DownloaderGUI`

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

#### --disableasync
Disables asynchronous download.
