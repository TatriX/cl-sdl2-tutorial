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
