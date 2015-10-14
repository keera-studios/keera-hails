Keera Hails is a collection of libraries to create MVC and reactive
applications.

This package contains bindings to make Gtk widgets reactive so that they can be
connected to other reactive elements using Reactive Rules.

For more details, see: https://github.com/keera-studios/keera-hails.

The Haskell bindings to Gtk2 export 4560 functions (A little over 5000 symbols
with types included). Of them, there are 2691 define interactive signals,
attributes and/or event-handler installers. They are split as follows:

| Element                         | Amount   |
|:--------------------------------|---------:|
| Signals                         | 228      |
| Event handler installers        | 304      |
| *Total signal/event handlers*   | *532*    |
| Attributes                      | 677      |
| Getters                         | 864      |
| Setters                         | 628      |
| *Total attribute access funcs.* | *2169*   |
| **Total**                       | **2691** |

To simplify the explanation, we consider getters/setters to give access to
underlying attributes, and signals to be equivalent to event-handler
installers.

Keera Hails, and this package in particular, provide both reactive bindings to
specific Attribute-Signal combinations and a general API to connect to any
signal-attribute pair (or, optionally, just a signal or just an attribute).
This makes the most important part of the GTK+ interface (more than half the
full API), available to reactive applications.
