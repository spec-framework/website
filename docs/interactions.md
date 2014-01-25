---
layout: docs
title: Sub&nbsp;widgets interaction
prev_section: open
next_section: dynamic
permalink: /docs/interactions/
---

The only missing part of this user&nbsp;interface is the interaction between the sub&nbsp;widgets.
The interaction is described in the method `initializePresenter`.

By linking the behavior of the different widgets it specifies the overall presentation, i.e. how the overall UI responds to interactions by the user.

Usually this method consists of specifications of actions to perform when a certain event is received by a widget.
From the propagation of those events the whole interaction flow of the UI emerges.

In  **Spec**, the different UI models are contained in reactive variables, and the event mechanism relies on the announcements of these reactive variables to manage the interactions between widgets.

Reactive variables provide a single method `whenChangedDo:` that is used to register a block to perform on change.
In addition to this primitive  `whenChangedDo:` method, the basic widgets provide more specific hooks, e.g. when an item in a list is selected or deselected.

The following example shows how to use one of the registration methods of the list widget to change the label of the button according to the selection in the list.

{% highlight smalltalk %}
theList whenSelectedItemChanged: [ :item | 
	item 
		ifNil: [ theButton text: 'No selected item' ]
		ifNotNil: [ theButton text: 'An item is selected'] ]
{% endhighlight %}

The whole event API of the basic widgets is described on this <a href="{{ site.url }}/docs/api">page</a>.

{% info %}
If a programmer wants his or her widgets to be reused,
they should provide a comprehensive API.
{% endinfo %}

{% info %}
This method is optional. Without it, the different widgets in the UI will simply not respond to changes in each others' state.
{% endinfo %}