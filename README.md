# r34-downloader
A program to download all the images of a given tag from https://rule34.paheal.net [Warning - Not Safe For Work]

The website has a one request a second limit, thus users with fast internet connections won't max out their bandwidth.

![Screenshot of program](r34downloader.png?raw=true "Screenshot of program")

Like the program? [Buy me a coffee :)](https://www.buymeacoffee.com/zpalmtree)

## Installation (Windows):

#### Precompiled
The easiest way is to get the installer [here.](https://github.com/zpalmtree/r34-downloader/releases)

Read on if you want to build from source.

#### From source
Get stack here and install it with the default options: https://www.stackage.org/stack/windows-i386-installer

You must download the 32 bit version, 64 bit won't work with hsqml.

Get the qt quick controls here: http://download.qt.io/official_releases/online_installers/qt-unified-windows-x86-online.exe

For the qt setup, skip the account creation, and deselect everything but MinGW under the most recent version of Qt. 

Note the version number.

![Screenshot of Qt installation](qt-install.png?raw=true "Screenshot of Qt installation")

Finish the install.

Download the repo as a zip, unzip it, and move into the directory, then open a terminal there.

Navigating to the directory in explorer, holding shift, and right clicking
"Open command window here" lets you easily get a terminal in the right spot.

`set PATH=%PATH%;C:\Qt\5.13.0\mingw73_32\bin`

It might not be 5.13.0 or 73_32, depends on what version you installed. If you're not sure, just open the Qt folder and follow the paths down.

If you get an error about moc not being available, you probably didn't set the path or install QT correctly.

`stack install`

#### Running
If you used the installer, it should prompt you to run the program on exit.

It also will have created a shortcut in your start menu.

If you built from source, run `stack exec r34Downloader` from a command prompt.

## Installation (Linux/Mac?):
I don't have a mac to test, but if you can aquire the qt quick required files, I assume it will work.

#### Install prerequistes
You need stack installed, along with the qt quick controls and qt quick controls2.

Look for the package that provides QtQuick/QML in your distro.

##### Debian based:
`sudo apt-get install haskell-stack qtdeclarative5-dev`

##### Arch based:
`sudo pacman -S stack qt5-quickcontrols qt5-quickcontrols2`

Commands should be similar for other distributions.

#### Clone the repository
`git clone https://github.com/zpalmtree/r34-downloader.git`

`cd r34-downloader`

Alternatively, download the repo as a zip, unzip it, and move into the directory.

#### Compile
`stack install`

#### Running
Either add ~/.local/bin to your path and run the executable:

`r34Downloader`

Or, run

`stack exec r34Downloader`

## Bugs
Please let me know if you find a bug, and I'll be happy to try and fix it, by opening an [issue](https://github.com/zpalmtree/r34-downloader/issues/new)


## Creating an MSI

Check out the [build folder](build)
