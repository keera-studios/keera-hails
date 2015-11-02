Keera Hails is a toolkit to create reactive applications in Haskell.
It facilitates combining User Interfaces, external devices, networks
connections, files and, optionally, FRP networks.

Keera Hails is modular and extensible. It is cross platform (Windows, Linux,
MacOSX, Android, GHCJS), UI-agnostic (Gtk+, WX, Qt, Android Native toolkit,
HTML DOM), FRP-agnostic and device-agnostic (Wiimote, Webcams, etc).

Keera Hails has been used to create large applications and is used commercially
in production.

The toolkit is divided in three parts:
* Reactive Values: they are typed mutable values with event dispatching and
access properties. They can be modified by lifting functions and applying
lenses to them. They can also be connected so that they stay in sync during
program execution.

  - http://github.com/keera-studios/keera-hails/tree/master/keera-hails-reactivevalues
  - http://github.com/keera-studios/keera-hails/tree/master/keera-hails-reactivelenses

* Reactive bindings: Widget properties/attributes can be seen as
reactive values. So can network sockets, files, application models ('model'
as in MVC) and external resources (polling). Uni-directional, Functional
Reactive Programming signal functions can also be wrapped into a pair
of RVs (see Yampa). The idea is that, at the highest application level
(controller), each layer is wrapped in a reactive container and connected
to others.

  - http://github.com/keera-studios/keera-hails/tree/master/keera-hails-reactive-fs
  - http://github.com/keera-studios/keera-hails/tree/master/keera-hails-reactive-network
  - http://github.com/keera-studios/keera-hails/tree/master/keera-hails-reactive-gtk
  - http://github.com/keera-studios/keera-hails/tree/master/keera-hails-reactive-wx
  - http://github.com/keera-studios/keera-hails/tree/master/keera-hails-reactive-qt
  - http://github.com/keera-studios/keera-hails/tree/master/keera-hails-reactive-htmldom (via GHCJS)
  - http://github.com/keera-studios/keera-hails/tree/master/keera-hails-reactive-yampa
  - http://github.com/keera-studios/keera-hails/tree/master/keera-hails-reactive-polling

  An experimental Android backend that uses Android's Native UI toolkit
  is also available. Please, contact Keera Studios if you wish to
  use it.

* Hails: a series of tools and libraries to create MVC programs using
  a reactive interface. The application architecture takes the approach of
  "convention over configuration", which means that certain modules will be
  expected to have predetermined names. If this is much of a problem, open a
  bug report.

  - http://github.com/keera-studios/keera-hails/tree/master/keera-hails
  - http://github.com/keera-studios/keera-hails/tree/master/keera-hails-mvc-model-lightmodel
  - http://github.com/keera-studios/keera-hails/tree/master/keera-hails-mvc-model-protectedmodels
  - http://github.com/keera-studios/keera-hails/tree/master/keera-hails-mvc-view-gtk
  - http://github.com/keera-studios/keera-hails/tree/master/keera-hails-mvc-view
  - http://github.com/keera-studios/keera-hails/tree/master/keera-hails-mvc-controller
  - http://github.com/keera-studios/keera-hails/tree/master/keera-hails-mvc-environment-gtk
  - http://github.com/keera-studios/keera-hails/tree/master/keera-hails-mvc-solutions-gtk
  - http://github.com/keera-studios/keera-hails/tree/master/keera-hails-mvc-solutions-config
  - http://github.com/keera-studios/keera-hails/tree/master/keera-hails-i18n
  - http://github.com/keera-studios/keera-hails/tree/master/keera-hails-templates

For a introduction to reactive values, see:
* [Bridging the GUI gap with reactive values and relations (Haskell Symposium 2015 paper)](http://dl.acm.org/citation.cfm?id=2804316)
* [Readme for Reactive Value library](http://github.com/keera-studios/hails-reactivevalues)
* [Ivan Perez's 1st PhD report, pages 29 and 40](http://www.cs.nott.ac.uk/~ixp/papers/2014-Perez-1st-year-report.pdf)
(page 40 is a paper of its own; page 29 is the thesis proposal outlining the
core ideas and problems that remain to be solved.)
* [TFP 2014 Pre-proceedings, pages 59-68](http://www.staff.science.uu.nl/~hage0101/preproceedingstfp2014.pdf)
* [Reactive Programming using Reactive Values (blog post)](http://keera.co.uk/blog/2014/05/24/reactive-programming-using-reactive-values/)

For bibtex references to these articles, see Publications in http://www.cs.nott.ac.uk/~ixp/.

Applications:
* http://github.com/keera-studios/keera-posture
* http://github.com/ivanperez-keera/SoOSiM-ui
* http://github.com/keera-studios/keera-hails/tree/master/demos/keera-hails-demos-wiimote
* http://github.com/ivanperez-keera/haskellifi-trayicon
* http://github.com/ivanperez-keera/keera-diamondcard-sms-trayicon

# Credits

I would like to thank the following people for fruitful discussions:
* Henrik Nilsson
* Hamish Mackenzie (for helping me create the first GHCJS backend for Hails,
  and for Gtk2hs).
* Leuite Stegeman (for helping me create the first GHCJS backend for Hails.)
* Arsen Kostenko
* Emilio Gallego
* Paolo Capriotti
* Florent Ballestrieri
* David McGillicuddy
* Philip Holzenspies
* Ian-Woo Kim
* Atze van der Ploeg
* Simon Peyton Jones
* Michał Gajda
* I thank the audiences of the following talks on Reactive programming and
  Hails, for patientily listening to me complain about the state of the world
  and commenting on my work.
  * Haskell Symposium (2015), Vancouver, colocated with ICFP.
  * London (2014), Haskell Meetup
  * Nottingham (2014), FPLAD.
  * TFP (2014)
  * FPLab, Nottingham (2013)
  * CAES Group, UTwente (2013)
  * Babel Research Group, UPM (2010)

(Note: these people do not necessarily support anything I have to say.)

<!--
## About the name

Keera Hails was born from several experiments back when I was an MSc student
and researcher in 2008-2009. Back then, it was clear that it was going to be
called Hails, and I often discussed it with my colleagues by referring to it as
``Haskell on Rails''. I checked that the name wasn't taken, and so Hails was
born. I wrote the first commercial program with this library in 2010, and I've
been using it ever since. Many programs have now been written in Hails
(including Gale, whose name sounds similar, means something related, and was
also not arbitrary).

In 2012, I received a message clients using the library telling me that they
couldn't compile their program anymore. Apparently someone had published a
library called ``Hails'' on Hackage (my hails was on github, but not on
hackage). I asked the authors of that library to change its name, but they
refused, telling me that they were there first.

It's sad that we have to have these disputes in such a small community. We both
think that we are right, and there is no easy way to resolve this matter
without one of us giving something up.

To avoid collisions, I call this library Keera Hails in all papers. Because
there is no risk of confusion, I use the name Hails in this documentation.
-->
