# Common Lisp SDL2 tutorials
Adapted from http://lazyfoo.net/tutorials/SDL/


## Installing the SDL2 Libraries

  The tutorial requires that SDL2, SDL2 Image, and SDL2 TTF are installed before
  running the examples.

### Mac OS X

Mac OS X requires each library to be downloaded and copied to
$HOME/Library/Frameworks or /Library/Frameworks.

 Library Name | Download Link
 ------------ | -------------
 SDL2         | https://www.libsdl.org/download-2.0.php
 SDL2 Image   | https://www.libsdl.org/projects/SDL_image
 SDL2 TTF     | https://www.libsdl.org/projects/SDL_ttf

Running the tutorials will cause Catalina to report that the framework cannot be
opened because the developer cannot be verified. The workaround for this problem
is the following:

1. Navigate to System Preferences -> Security & Privacy -> General on the Mac

2. Run repl.sh

3. When the dialog appears about the developer not being verified, click the
"Cancel" button.

4. Examine the Security & Privacy screen for a button called "Allow Anyway" for
the framework. Click the "Allow Anyway" button with the identified framework.

5. The verification dialog will now appear a second time, but there will be an
option to "Open" the framework. Click "Open".

6. Repeat steps 3 through 5 for the different frameworks.

### Linux

The SDL2 libraries can be installed on a Debian based system with the command
below.

	apt install libsdl2-2.0 libsdl2-image-2.0 libsdl2-ttf-2.0


## Running the Examples

A repl.sh script is provided that loads SDL2-TUTORIAL with quicklisp and then
starts up a swank thread on port 4005 for repl connections.

	1. ./repl.sh
	2. (sdl2-tutorial:main #'sdl2-tutorial-1:main)
