# Reactive Values

Reactive Values are *time-varying mutable values that can be read/written and
may trigger events when they change*. They can be used to define a *uniform
interface* to hardware ports, widget attributes or model fields, and keep them
all synchronized at all times (making your controllers just a bunch of
synchronization rules).

For instance, in Hails programs, a field of the model that holds a String (for
instance, with the name of the file that the user is editing) may be a
read-write reactive value. The title of a Gtk window may be a Write-only
reactive value.  

So we can always keep them in sync in our programs by writing:

    titleOfMainWindow <:= liftR ("My program - " ++) filenameFieldOfModel

This will update the window title every time the file name changes.
Because read-only values are functors, you can also use applicative style:

    titleOfMainWindow <:= ("My program - " ++) <$> filenameFieldOfModelRO

(where filenameFieldOfModelRO is a read-only RV.) Note that RW RVs are *not*
Functors (the targets of the morphisms would be read-only variables,
even if we originally had an RW, so it does not fulfill the functor laws).

If titleOfMainWindow were instead a RW reactive value, we could have said:

    titleOfMainWindow =:= filenameFieldOfModel

That would also update the file name in the model if the title changed for
some reason (although that is not something we'd like to do in this particular
example, but we might want to sync a text box with a string field).

You can also lift functions in =:= equations by defining them bijective:

      titleOfMainWindow =:= liftRW f filenameFieldOfModel
    where f = BijectiveFunc (s ++) (drop (len s))
          s = "My program -"

We use a definition of Functor that is parametric also on the morphisms of the
source category, which means that you can regard them as functors:

      titleOfMainWindow =:= f <$$> filenameFieldOfModel
    where f = BijectiveFunc (s ++) (drop (len s))
          s = "My program -"

(<$$> is to GFunctor what <$> is to Functor). And if the function being lifted
is equal to its inverse, you can declare it an involution:

      titleOfMainWindow =:= (involution reverse) <$$> filenameFieldOfModel

The above line would keep the fields in sync, but one would be the reversed
representation of the other. Note that reverse here has nothing to do with
inverse functions, it is the normal list reverse operation.

# Installation

This package has no dependencies other than base and contravariant, so you can
install it and use it in your own programs without any of the existing
backends.

# Support

Open a ticket or send me a message if you use this and have a problem.

Also, I'd be happy to know (and publish a list of) programs that use
hails-reactivevalues. Please, let me know if you write something that uses it.
