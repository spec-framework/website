---
layout: docs
title: Inserting a Morph
prev_section: use-pharo-window
next_section: api
permalink: /docs/insert-morph/
---

*Spec* can interface easily with a Morph.

To insert a morph into a Spec widget, you need to add an instance side method similar to the accessors needed.

This method, instead of returning a Spec model as the other accessor methods, will return a `MorphicGenericAdapter` wrapping the morph you want to insert.

There is a convenience method defined on **Morph** to wrap any morph into a `MorphicGenericAdapter` named `asSpecAdapter`.

The following example shows how to insert a calendar morph into a Spec widget.

{% highlight smalltalk %}
calendar
	| morph |
	
	morph := CalendarMorph on: Date today.
	^ morph asSpecAdapter
{% endhighlight %}

{% alert %}
Inserting a Morph into a Spec widget make your application Morphic specific where Spec is designed to be framework independent.
{% endalert %}