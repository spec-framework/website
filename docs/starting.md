---
layout: docs
title: Where to start?
prev_section: installation
next_section: initializing
permalink: /docs/starting/
---

The whole framework is based on the class **ComposableModel**. 
This class provides all the methods needed to create and to compose user&nbsp;interfaces.

A Spec model requires three things: 

1. to subclass **ComposableModel**
2. to implement *initializeWidgets*
3. to implement a layout method on class side 

The creation a user&nbsp;interface starts with the creation of a new subclass of **ComposableModel**.
The sub&nbsp;widgets of the user&nbsp;interface are stored as instance variables.

{% highlight smalltalk %}
ComposableModel subclass: #MyUserInterface
	instanceVariableNames: 'theList theButton'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MyExamples'
{% endhighlight %}

As you may have guessed, *theList* will be a list widget and *theButton* will be a button widget.
The instanciation and the customization of the sub widgets is done in the method **initializeWidgets**.