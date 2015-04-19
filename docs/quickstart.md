---
layout: docs
title: Quick start
prev_section: home
next_section: installation
permalink: /docs/quickstart/
---

For the impatient, Spec can be found in any Pharo 3.0 image.
You can download and run one with the following script:

{% highlight bash %}
~ $ curl get.pharo.org/30+vm | bash
~ $ ./pharo-ui Pharo.image

#=> Pharo is now opening
{% endhighlight %}

Here you are, you can now play with Spec.
Where is starts to be really interesting is when you need to define your own user interface, or if you want to quickly prototype one. Then the magic happens and a couple line of code is enough to open a widget.

{% highlight smalltalk %}
ButtonModel new 
	label: 'Browse me';
	action: [ Smalltalk tools browser openOnClass: ButtonModel ];
	openWithSpec.
	
ListModel new 
	items: Smalltalk allClasses;
	openWithSpec.	
	
"And many more widgets are waiting for you!"
{% endhighlight %}
