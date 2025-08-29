#[

= Installation

#set raw(lang:"os")
#show raw.where(lang:"os") : it => {
  text(fill:blue,it)
}

== General Instructions

+ This may take a while, so make sure that you have enough time on your hands.

+ Make sure that your device has enough charge to last you the entire installation process.

+ Make sure that you have a strong and stable internet connection.

+ Make sure that any antivirus(es) that you have on your device is fully turned off during the installation process. You can turn it back on immediately afterwards.

+ Make sure to follow the following instructions *IN ORDER*.\ Make sure to *COMPLETE EACH STEP* fully *BEFORE* moving on to the *NEXT STEP*.

== Choose your Operating System

=== Linux

+ *Install Haskell*\

  + Read the general instructions very carefully, and ensure that you have complied with all the requirements properly.

  + Close all open windows and running processes other than wherever you are reading this.

  + Open the directory `Haskell/installation/Linux` in your text editor.\ (We have more support for Visual Studio Code, but any text editor should do)

  + Type in the commands in the `installHaskell` file into the terminal.

  + This may take a while.

  + You will know installation is complete at the point when it says `Press any key to exit`.

  + Restart (shut down and open again) your device.

+ *Install HaskellSupport*\

  + Read the general instructions very carefully, and ensure that you have complied with all the requirements properly.

  + Close all open windows and running processes other than wherever you are reading this.

  + Open the directory `Haskell/installation/Linux` in your text editor.\ (We have more support for Visual Studio Code, but any text editor should do)

  + Type in the commands in the `installHaskellSupport` file in the terminal.

  + This may take a while.

  + You will know installation is complete at the point when it says `Press any key to Exit`.

  + Restart (shut down and open again) your device.

=== MacOS

+ *Install Haskell*\

  + Read the general instructions very carefully, and ensure that you have complied with all the requirements properly.
  
  + Close all open windows and running processes other than wherever you are reading this.
    
  + Open the folder `Haskell` in Finder .
  
  + Open the folder `installation` in Finder.
  
  + Right click on the folder `MacOS` in Finder, and select `Open in Terminal`.
  
  + Type in `chmod +x installHaskell.command` in the terminal.
  
  + Close the terminal window.
  
  + Open the folder `MacOS` in Finder.
  
  + Double-click on `installHaskell.command`.
  
  + This may take a while.
  
  + You will know installation is complete at the point when it says `Press any key to exit`.
  
  + Restart (shut down and open again) your device.

+ *Install Visual Studio Code*\
    Get it #text(fill:blue,(emph(underline(link("https://code.visualstudio.com/download",[here]))))).

+ *Install HaskellSupport*\.
  
  + Read the general instructions very carefully, and ensure that you have complied with all the requirements properly.
  
  + Close all open windows and running processes other than wherever you are reading this.
    
  + Open the folder `Haskell` in Finder .
  
  + Open the folder `installation` in Finder.
  
  + Right click on the folder `MacOS` in Finder, and select `Open in Terminal`.
  
  + Type in `chmod +x installHaskellSupport.command` in the terminal.
  
  + Close the terminal window.
  
  + Open the folder `MacOS` in Finder.
  
  + Double-click on `installHaskellSupport.command`.
  
  + This may take a while.
  
  + You will know installation is complete if a new window pops up asking whether you trust authors. Click on "Trust".
  
  + Restart (shut down and open again) your device.

=== Windows

+ *Install Haskell*\.
  
  + Read the general instructions very carefully, and ensure that you have complied with all the requirements properly.
  
  + Close all open windows and running processes other than wherever you are reading this.
  
  + Open the folder `Haskell` in File Explorer .
  
  + Open the folder `installation` in File Explorer.
  
  + Open the folder `Windows` in File Explorer.
  
  + Double-click on `installHaskell`.
  
  + This may take a while.
  
  + You will know installation is complete at the point when it says `Press any key to exit`.
  
  + Restart (shut down and open again) your device.

+ *Install Visual Studio Code*\
  Get it #text(fill:blue,(emph(underline(link("https://code.visualstudio.com/download",[here]))))).

+ *Install HaskellSupport*\.
  
  + Read the general instructions very carefully, and ensure that you have complied with all the requirements properly.
  
  + Close all open windows and running processes other than wherever you are reading this.
  
  + Open the folder `Haskell` in File Explorer.
  
  + Open the folder `installation` in File Explorer.
  
  + Open the folder `Windows` in File Explorer.
  
  + Double-click on `installHaskellSupport`.
  
  + This may take a while.
  
  + You will know installation is complete if a new window pops up asking whether you trust authors. Click on "Trust".
  
  + Restart (shut down and open again) your device.

]

= Running Haskell

Open VS Code. A window "Welcome" should be open right now. If you close that tab, then a tab with `helloWorld` written should pop up.

If you right-click on `True`, a drop-down menu should appear, in which you should select "Run Code".

You have launched GHCi. After some time, you should see the symbol `>>> `appear.

Type in `helloWorld` after the `>>> `.

It should reply `True`.

#set raw(lang:"os")
#show raw.where(lang:"os") : it => {
  text(fill:blue,it)
}

= Fixing Errors

If you see squiggly red, yellow, or blue lines under your text, that means there is an error, warning, or suggestion respectively.

To explore your options to remedy the issue, put your text cursor at the text and click `Ctrl`+`.` .

You have opened the QuickFix menu.

You can now choose a suitable option.

= Autocomplete

Just like texting with your friends, VS Code also gives you useful auto-complete options while you are writing. 

To navigate the auto-complete options menu, hold down the `Ctrl` key while navigating using the #text(fill:blue,$arrow.t$) and #text(fill:blue,$arrow.b$) keys.

To accept a particular auto-complete suggestion, use `Ctrl`+`Enter`.