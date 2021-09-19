# Keera Hails
[![Build Status](https://travis-ci.org/keera-studios/keera-hails.svg?branch=master)](https://travis-ci.org/keera-studios/keera-hails)

Keera Hails is a toolkit to create *Reactive Applications in Haskell*.
It facilitates combining User Interfaces, external devices, network
connections, files and, optionally, FRP networks.

Keera Hails is modular and extensible. It is cross platform (Windows, Linux,
MacOSX, iOS, Android, GHCJS), UI-agnostic (Gtk+, WX, Qt, iOS native UIs,
Android Native toolkit, HTML DOM), FRP-agnostic and device-agnostic (Wiimote,
Webcams, etc).

Keera Hails has been used to create large applications and is used commercially
in production.

# Hands-on example

The two key ideas in Keera Hails are *Reactive Values* and *Reactive Rules*.
Reactive values are data holders or action endpoints that will contain, provide
and/or consume data.  Reactive Rules just connect these values so that changes
propagate across.

A very simple example of an RV is the following construction, in which a passive
`IORef` is turned into active Reactive Value.

```haskell
do

  -- Empower IORef with callback installation mechanism. This comes from the
  -- keera-callbacks library.
  --
  -- passiveCBRef :: CBRRef Integer
  passiveCBRef <- newCBRef 0

  -- Turn IO Ref into active reactive value (RV).
  --
  -- RVs are type classes. We use the type of Reactive Fields, which have a
  -- trivial RV implementation.
  let activeCBRefRV :: ReactiveFieldReadWrite IO Integer
      activeCBRefRV = ReactiveFieldReadWrite
                        (writeCBRef           passiveCBRef)
                        (readCBRef            passiveCBRef)
                        (installCallbackCBRef passiveCBRef)
```

We now define an RV that encloses a trivial monadic action:

```haskell
  -- do continues

  -- Define a write-only RV that prints whatever you put in it.
  let printer :: Show a => ReactiveFieldWrite IO a
      printer = wrapMW print
```

You can connect them together in a monadic environment:
```haskell
  -- Connect them using a reactive rule. In a GUI application, this code would
  -- in the controller, and would define connections between the model and
  -- the view.
  --
  -- For bi-directional connections, see (=:=).
  activeCBRefRV =:> printer
```

If you now loop and put data in the `IORef`, it will be passed along
the reactive connection and printed to the output:

```haskell
  forever $ do
    threadDelay 1000000 -- 1 second
    reactiveValueModify activeCBRefRV (+1)
```

Using the same, simple ideas, you can define a RVs for, and connect, the fields
of GUI widgets, for files, for network sockets, etc.

# Project Structure

The toolkit is divided in three parts:
* Reactive Values: they are typed mutable values with event dispatching and
access properties. They can be modified by lifting functions and applying
lenses to them. They can also be connected so that they stay in sync during
program execution.

  - [keera-hails-reactivevalues](keera-hails-reactivevalues/): basic definitions and RV manipulation.
  - [keera-hails-reactivelenses](keera-hails-reactivelenses/): lens application.

* Reactive bindings: Widget properties/attributes can be seen as
reactive values. So can network sockets, files, application models ('model'
as in MVC) and external resources (polling). Uni-directional, Functional
Reactive Programming signal functions can also be wrapped into a pair
of RVs (see Yampa). The idea is that, at the highest application level
(controller), each layer is wrapped in a reactive container and connected
to others.

  - [keera-hails-reactive-fs](keera-hails-reactive-fs/): Files as RVs
  - [keera-hails-reactive-network](keera-hails-reactive-network/): Sockets as RVs
  - [keera-hails-reactive-gtk](keera-hails-reactive-gtk/): Widget attributes and events as RVs
  - [keera-hails-reactive-wx](keera-hails-reactive-wx/): Widget attributes and events as RVs
  - [keera-hails-reactive-qt](keera-hails-reactive-qt/): Widget attributes and events as RVs
  - [keera-hails-reactive-htmldom](keera-hails-reactive-htmldom/) (via GHCJS): HTML DOM element properties as RVs
  - [keera-hails-reactive-yampa](keera-hails-reactive-yampa/): Yampa reactimation loops as a pair of RVs
  - [keera-hails-reactive-polling](keera-hails-reactive-polling/): Strategies to make RVs fire regularly

  Backends for iOS and Android (using each platform's native UI toolkits) are
also available. Samples can be found
[here](https://www.facebook.com/keerastudios/videos/1674596312555888/) and
[here](https://www.facebook.com/keerastudios/photos/a.550237501658447.138031.300854939930039/1675515659130620/?type=3&theater).
Please contact Keera Studios if you wish to use them.

* MVC Architecture

  MVC is easily applied using hails. There are two easy ways of wraping pure
  models into RVs, depending on the level of change detection we need to detect
  and optimise.
  - [keera-hails-mvc-model-lightmodel](keera-hails-mvc-model-lightmodel/): Pure
    models as RVs
  - [keera-hails-mvc-model-protectedmodels](keera-hails-mvc-model-protectedmodels/):
    Pure models as RVs, with advance change detection. Template Haskell can be
	used to generate model access RVs based on record fields that stop
    unnecessary change propagation.
  - [keera-hails-mvc-view](keera-hails-mvc-view/): Interface to the View of any
    program
  - [keera-hails-mvc-controller](keera-hails-mvc-controller/): Simple controller-handling definitions.

  A pair Model-View, often needed by the controller, can be wrapped in an
  *environment*.  The following packages implement a Gtk View and a Gtk
  environment.
  - [keera-hails-mvc-view-gtk](keera-hails-mvc-view-gtk/): Structure to wrap Gtk views.
  - [keera-hails-mvc-environment-gtk](keera-hails-mvc-environment-gtk/): A MVC triplet based on a Gtk View.

  The following package generate default project skeletons that do "the right
  thing" (currently for Gtk+ only). The application takes the approach of
  "convention over configuration": certain modules will be expected to have
  predetermined names. If this is much of a problem, open a bug report.
  - [keera-hails](keera-hails/): Program that generates a project skeleton.

  We have a [separate README](demos/keera-hails-gtk-app/) that shows how to
  build your first app using `keera-hails`.

* Applications can be simplified further. The following packages implement
  Gtk-based choreographies (M-V synchronizations and controller rule templates)
  and address other common features needed in applications.

  - [keera-hails-mvc-solutions-gtk](keera-hails-mvc-solutions-gtk/): Advanced choreographies for Gtk applications.
  - [keera-hails-mvc-solutions-config](keera-hails-mvc-solutions-config/): Handling configuration files cleanly.
  - [keera-hails-i18n](keera-hails-i18n/): Handling internationalization

## Tutorials, papers and publications

* [Building a reactive calculator in Haskell](https://keera.co.uk/2020/05/28/building-a-reactive-calculator-in-haskell-1-5/)
* [The Arpeggigon: Declarative Programming of A Full-Fledged Musical Application (PADL 2017)](http://eprints.nottingham.ac.uk/38657/1/padl2017-techreport.pdf)
* [Bridging the GUI gap with reactive values and relations (Haskell Symposium 2015)](http://dl.acm.org/citation.cfm?id=2804316)
* [Readme for Reactive Value library](http://github.com/keera-studios/hails-reactivevalues)
* [Ivan Perez's 1st PhD report, pages 29 and 40](http://www.cs.nott.ac.uk/~ixp/papers/2014-Perez-1st-year-report.pdf)
(page 40 is a paper of its own; page 29 is the thesis proposal outlining the
core ideas and problems that remain to be solved.)
* [TFP 2014 Pre-proceedings, pages 59-68](http://www.staff.science.uu.nl/~hage0101/preproceedingstfp2014.pdf)
* [Reactive Programming using Reactive Values (blog post)](http://keera.co.uk/blog/2014/05/24/reactive-programming-using-reactive-values/)

For bibtex references to these articles, see Publications in http://www.cs.nott.ac.uk/~ixp/.

## Applications and demos

* http://github.com/keera-studios/keera-posture
* http://github.com/ivanperez-keera/SoOSiM-ui
* [A simple example using RVs for button clicks, sliders, monadic actions and SDL volume](http://github.com/keera-studios/keera-hails/tree/master/demos/keera-hails-demos-soundplay/src/Main.hs#L41-L58)
* http://github.com/keera-studios/keera-hails/tree/master/demos/keera-hails-demos-wiimote
* http://github.com/ivanperez-keera/haskellifi-trayicon
* http://github.com/ivanperez-keera/keera-diamondcard-sms-trayicon

## Credits

I would like to thank the following people for fruitful discussions and collaborations.
* Henrik Nilsson
* Hamish Mackenzie (for helping me create the first GHCJS backend for Hails,
  and for Gtk2hs).
* Leuite Stegeman (for helping me create the first GHCJS backend for Hails).
* Guerric Chupin (for continuing this work and writing Arpeggigon).
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
