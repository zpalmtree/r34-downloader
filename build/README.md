# Generating an msi on windows

## Get prerequisites

Install the wix toolset from here: http://wixtoolset.org/releases/

Or otherwise aquire the "Heat", "Candle", and "Light" executables.

## Aquire QT Files and .exe

Firstly, follow the instructions for compliling from source on windows, in the readme.

However, for the final step, instead of running 

`stack install`

instead, run

`stack install --flag rule34-paheal-downloader:localqml`

You need to distribute the QT files and dll's along with the .exe for it to successfully run.

You can find the files you will need and where they need to be located in the [tree.txt](tree.txt) file.

You need to place the files in tree.txt, inside a folder called InstallationFiles, which should be located in the same folder as the make.bat.

All these files will have been installed on your computer, provided you followed the building on source windows instructions. 

They should live in somewhere like: C:/Program Files (x86)/R34Downloader/R34Downloader/

## Make installer

If you used the wix toolset installer it should add the %WIX% variable to your path, so the make file works.

You can manually edit the make file to point to your binaries if they're in a different spot.

Ensure you have removed all .qmlc files before running make.bat. They are created when you run the program, but are not needed to install it.

Run make.bat.

Once you have successfully ran the make file and generated an .msi, you can safely delete all the other files, and distribute just the .msi.
