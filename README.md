This is a sample reactive program that shows the data received from
a Nintendo Wiimote's accelerometer using a Gtk GUI.

The program is very simple, there are two kinds of Reactive Values:
* Wiimote Accelerometer RVs, polled regularly.
* GUI RVs, representing the texts in the entries.

Rules are used to keep all in sync. See Minimal for more details.

# Compilation

Apart from installing all the dependencies, you should compile
the program with the flags -threaded and -rtsopts, and run it
with +RTS -V0.

Once the program launches, press 1+2 (or sync near the battery slot)
on your wiimote to connect to the computer.

You should expect to see the following:

![Wiimote demo](https://github.com/keera-studios/hails-reactive-wiimote-demo/raw/master/screenshots/gui.png)

# See also

- http://github.com/keera-studios/keera-hails
- http://github.com/keera-studios/keera-hails-reactive-polling
- http://github.com/keera-studios/hails-reactive-gtk
- http://github.com/keera-studios/hails-reactivevalues
- http://github.com/ivanperez-keera/hcwiid
