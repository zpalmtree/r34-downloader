@echo off

echo Gathering files...

"%WIX%\bin\heat" dir "InstallationFiles" -o "HarvestedFiles.wxs" -t "exclude.xsl" -scm -sfrag -srd -sreg -ag -cg "ComponentGroupId" -dr INSTALLDIR -nologo

if errorlevel 1 goto die

echo Compiling to .wixobj files...

"%WIX%\bin\candle" -ext WixUIExtension -ext WixUtilExtension "R34Downloader-installer.wxs" "HarvestedFiles.wxs" -nologo

if errorlevel 1 goto die

echo Linking to .msi file...

"%WIX%\bin\light" -b "InstallationFiles" -ext WixUIExtension -ext WixUtilExtension "R34Downloader-installer.wixobj" "HarvestedFiles.wixobj" -o "R34Downloader-installer.msi" -nologo

:die
@pause
