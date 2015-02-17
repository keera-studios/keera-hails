This is an experimental layer for Qt as reactive values.

So far only buttons clicks and line edit texts have been added. For an example,
see:

https://github.com/keera-studios/hails-reactive-qt/blob/master/examples/Example.hs#L54

In which widgets are connected. In particular, given two line edits (text fields),
called l1 and l2, and a button called tb, we connect them with the following code:

    -- Make widgets reactive
    e1 <- lineEditText l1
    e2 <- lineEditText l2
    btn <- buttonClick tb
    
    -- Connection: rule
    e1 =:= (involution reverse <$$> e2)
    e1 <:= (btn `governingR` e2 :: ReactiveFieldRead IO String)

This makes:
- e1 and e2 always represent the same text, reversed from one another.
![Typing in top box](http://ivanperez-keera.github.com/images/screenshots/reactive-qt-type-box1.png "Type in top box")
![Typing in bottom box](http://ivanperez-keera.github.com/images/screenshots/reactive-qt-type-box2.png "Type in bottom box")

- Whenever the button is depressed, the text in e2 is copied (literally)
onto e1. (Consequently, e2 also changes as per rule #1.)
We say that the button click is '''governing''' the line edit text, because
the button determines when changes to the text are propagated.
![Button click](http://ivanperez-keera.github.com/images/screenshots/reactive-qt-button-click.png "Click the button")
![Text transfers up, reverses down](http://ivanperez-keera.github.com/images/screenshots/reactive-qt-button-click2.png "Text transfers up (rule 2), text reverses down (rule 1)")

See also:

http://github.com/keera-studios/keera-hails
http://github.com/keera-studios/hails-reactivevalues
