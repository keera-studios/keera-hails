This repo contains sample reactive programs that show data received from a
Nintendo Wiimote using a Gtk GUI. All the programs are very simple; you
are encouraged to open the haskell code and try to understand it. If the meaning
is not obvious, you should open a bug report in keera-hails.

* In ![Minimal.hs](/Minimal.hs) there are two kinds of Reactive Values:
    * Wiimote Accelerometer RVs, polled regularly.
    * GUI RVs, representing the texts in the entries.

* ![MinimalFRP.hs](/MinimalFRP.hs) contains an additional pair of RVs for each
  field, and uses the Functional Reactive Programming DSL
[Yampa](http://github.com/ivanperez-keera/Yampa) to integrate the values of the
acceleration over time using Signal Functions.

* ![WMGui.hs](/WMGui.hs)
is a larger example featuring a glade gtk3 UI to show the state of a wiimote.
All the buttons, plus accelerometer data, are supported.

In all cases, simple, declarative, reactive relations/rules are used to keep
all Reactive Values in sync.

# Compilation

Apart from installing all the dependencies, you should compile
the program with the flags -threaded and -rtsopts, and run it
with +RTS -V0.

Once the program is launched, press 1+2 (or sync near the battery slot)
on your wiimote to connect to the computer.

You should expect to see the following (respectively, WMGui.hs and Minimal.hs):

![WMGUI demo](https://github.com/keera-studios/hails-reactive-wiimote-demo/raw/master/screenshots/hwgui.png)
![Wiimote demo](https://github.com/keera-studios/hails-reactive-wiimote-demo/raw/master/screenshots/gui.png)

# See also

- http://github.com/keera-studios/keera-hails
- http://github.com/keera-studios/keera-hails-reactive-polling
- http://github.com/keera-studios/keera-hails-reactive-yampa
- http://github.com/keera-studios/hails-reactive-gtk
- http://github.com/keera-studios/hails-reactivevalues
- http://github.com/ivanperez-keera/hcwiid
- http://github.com/ivanperez-keera/Yampa
