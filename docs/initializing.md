---
layout: docs
title: Instantiating sub&nbsp;widgets
prev_section: installation
next_section: layout
permalink: /docs/initializing/
---

The method `initializeWidgets` is used to instantiate the different widgets that are part of the UI and store them in their respective instance variables.
The configuration and default values of each widget are specified here as well.
This focus in this method is to specify what the widgets will look like and what their self-contained behavior is.
The behavior to update model state, e.g. when pressing a `Save` button, is described in this method as well.
It is explicitly **not** the responsibility of this method to define the interactions **between** the widgets.

In general the `initializeWidgets` method should follow the pattern:

- widgets instantiation
- widgets configuration specification
- specification of order of focus

The last step is not mandatory but **highly** recommended.
Indeed, without this final step keyboard navigation will not work at all.

The code in figure *pattern* shows an example of an `initializeWidgets` method.
It first instantiates a button and a list widget, storing each in an instance variable.
It second configures the button it by setting its label.
Third it specifies the focus order of all the widgets: first the button and then the list.

{% highlight smalltalk %}
initializeWidgets

	theButton := self newButton.
	theList := self newList.

	theButton label: 'I am a button'.
	theList items: #(1 2 3 4 5 6 7 8 9 0).
	
	self focusOrder
		add: theButton;
		add: theList.
{% endhighlight %}

{% alert %}
Specifying this method is **mandatory**, as without it the UI would have no widgets
{% endalert %}

## Widget instantiation

The instantiation of a widget can be done in two ways: through the use of an creation method or through the use of the `instantiate:` method.

Considering the first option, the framework provides unary messages for the creation of all basic widgets.
The format of these messages is `new[Widget]`, for example `newButton` creates a button widget, and `newList` creates a list widget, as we have seen above.
The complete list of available widget creation methods can be found in the class **ComposableModel** in the protocol *widgets*.

Considering the second option, to reuse any composite widgets, i.e. a subclass of **ComposableModel**, the widget needs to be initialized using the `instantiate:` method.
For example, to reuse a **MessageBrowser** widget, the code is `self instantiate: MessageBrowser`.

{% info %}
Do not forger to generate the accessors for your instance variables.
{% endinfo %}