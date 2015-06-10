Reactive Values are mutable values with change propagation. They can be
composed to create new RVs, and linked together so that changes to one
propagate towards others.  Reactive Values are typed (by the type of the
contents and by their access properties); when they are connected, the type of
the contents must match and the access properties must be compatible.

Protected Models are thread-safe mutable values with change propagation. Every
Protected Model is seen as a collection of Reactive Values. Protected Models
may register changes, giving the possibility of undoing them.

Protected Models are meant to enclose an MVC's application Model so that it is
completely thread-safe, coherent, and change propagation is efficient. Each PM
comes with its own change propagation notification loop.

See https://github.com/keera-studios/keera-hails for more details.
