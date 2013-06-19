Hails Solutions config
====================================

This package contains a design pattern for reading from/writing to
configuration files.

It provides you with a type synonym of what your Config IO should look like,
and two default operations that work well in (afaik) all platforms.


Installation
====================================

Because the operatiosn handle errors (which are slightly different from pure
haskell exceptions in Gtk), this package depends on MissingK
(https://github.com/keera-studios/MissingK)

Apart from that, all dependencies are standard.

Including the dependencies of MissingK, you'll need:

libraries: glib2.0

    $ apt-get install libglib2.0-dev

programs: alex happy gtk2hs-buildtools

    $ apt-get install happy alex && cabal install gtk2hs-buildtools

haskell packages:

    $ git clone git://github.com/keera-studios/MissingK.git
    $ git clone git://github.com/keera-studios/hails-mvc-solutions-config.git
    $ cd MissingK
    $ cabal install
    $ cd ../hails-mvc-solutions-config
    $ cabal install
