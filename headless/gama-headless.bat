
cls
setLocal EnableDelayedExpansion
set inputFile=""
set outputFile="" 
set memory=2048m
set workDir=%TEMP%\.work%RANDOM%
SETLOCAL enabledelayedexpansion

:TOP

IF (%1) == () GOTO NEXT_CODE
	if %1 EQU -m ( 
		set  comm=%1
		set  next=%2
		set memory=!next!
		SHIFT
		GOTO DECALE
	)

	set param=%param% %1
	GOTO DECALE
:DECALE
SHIFT
GOTO TOP

:NEXT_CODE

set FILEPATH=C:\Users\Gama\Documents\GAMA\plugins\org.eclipse.equinox.launcher_1.5.300.v20190213-1655.jar


call C:\Users\GAMA\Documents\GAMA\jdk\bin\java  -cp %FILEPATH% -Xms512m -Xmx%memory%  -Djava.awt.headless=true org.eclipse.core.launcher.Main  -application msi.gama.headless.id4 -data "%workDir%" !param! 
