hails-mvc-controller
====================

Aspects purely related to controller conditions (minimal package).

Controllers update models and views, and the conditions/event handlers can
work both ways. Before Keera Hails' reactive values were introduced, I used
directional conditions to keep models and views in sync. Since most update
operations in one direction were almost verbatim copies of update operations
in the other direction, I often decided to create only one and pass the direction
of the update as a parameter.

This package contains that pattern: direction of updates in MVC Controller's
handlers. It is only useful in combination with the rest of Keera Hails suite.

Installation
====================

It has no strange dependencies, so you should be able to install it normally
with cabal. It is not in hackage, so you'll have to clone the repo first.

    $ git clone git@github.com:keera-studios/hails-mvc-controller.git
    $ cd hails-mvc-controller
    $ cabal install

(Note: I prefer to use cabal-dev, and I recommend that you do the same.)
