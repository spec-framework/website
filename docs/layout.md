---
layout: docs
title: Defining the layout
prev_section: initializing
next_section: open
permalink: /docs/layout/
---

This method specifies the layout of the different widgets in the UI.
It also specifies how a widget reacts when the window is resized.

For the same UI multiple layouts can be described, and when the UI is built a specific layout to use can be specified.
If no such specific layout is given, the following lookup mechanism will be used to obtain the layout method:

1. Search on class side, throughout the whole class hierarchy, for a method with the pragma `<spec: #default>`.
2. If multiple such methods exist, the first one found is used.
3. If none such methods exist and if there is exactly one method with the pragma `<spec>`, this method is used.
4. No layout method is found, an error is raised.

This method is on class side because it returns a value that usually is the same for all the instances.
Put differently, usually all the instances of the same user interface have the same layout and hence this can be considered as being a class-side accessor for a class variable.
Note that the lookup for the spec method to use starts on instance side, which allows a UI to have a more specific layout depending on the state of the instance.

The simpliest example of such a method is laying out just one widget.
The example following presents such a layout.
It returns a layout in which just one widget is added: the widget contained in `theList` instance variable.

{% highlight smalltalk %}
    ^ SpecLayout composed
        add: #theList;
        yourself
{% endhighlight %}

<p>The symbol <code>theList</code> refers to an instance side method returning a widget.
This is because as instance variables are private, the layout class needs to use an accessor to obtain it when building the UI.
Note that by default, a widget will take all the space available.</p>

<p>As said above, multiple layouts can be described for the same user interface.
In order to retrieve the correct method to apply, these methods need to be flagged with a pragma.
The pragma can be either <code>&lt;spec: default&gt;</code> for the layout to use by default, or <code>&lt;spec&gt;</code> for the other layouts.</p>

{% alert %}
Specifying this method is **mandatory**, as without it the UI would show no widgets to the user.
{% endalert %}


## Layout Examples

As layouts can become quite complex, this section provides a list of examples of the construction of layouts.
First two examples are given of the use of [rows and columns](#layout_rows_and_column_layout).
This is followed by two examples that explain how to set a [fixed size](#layout_set_size_pixels) for rows and columns.
Next is an example that explains how to specify a widget [proportionally](#layout_percentage).
The last example presents the [expert](#layout_expert) mode in case everything else fails.
To conclude, this section ends with a little [explanation](#layout_specify_layout) of how to specify which layout to use and where to find the complete API.

<a name="layout_rows_and_column_layout"></a>
Often the layout of user interfaces can be described in rows and columns, and **Spec** provides for an easy way to specify such layouts.
The following example shows how to build a row of widgets.

{% highlight smalltalk %}
^ SpecLayout composed
	newRow: [ :row |
		row
			add: #theList;
			add: #theButton
	];
	yourself
{% endhighlight %}

Having the widgets rendered as a column is similar, as shown in this example

{% highlight smalltalk %}
^ SpecLayout composed
	newColumn: [ :column |
		column
			add: #theList;
			add: #theButton
	];
	yourself
{% endhighlight %}


Rows and columns can be combined to build more complex layouts, and splitters between cells can be added.
The example shows how to create a 3 columns layout, containing three buttons in each column.
This example also shows the `addSplitter` message, which adds a splitter between the element added before it and the element added after.

{% highlight smalltalk %}
^ SpecLayout composed
	newRow: [ :row |
		row
			newColumn: [ :column | 
				 column
				 	add: #button1;
					add: #button2;
					add: #button3 
			];
			addSplitter;
			newColumn: [ :column | 
				 column
				 	add: #button4;
					add: #button5;
					add: #button6 
			];
			addSplitter;
			newColumn: [ :column | 
				 column
				 	add: #button7;
					add: #button8;
					add: #button9 
			];
	];
	yourself
{% endhighlight %}

---

<a name="layout_set_size_pixels"></a>
The height of rows as well as the width of columns can be specified, to prevent them to take all the available space.
The first example shows how to specify the height of a row in pixels while the second example is about how to specify the column width.

{% highlight smalltalk %}
^ SpecLayout composed
	newRow: [ :row |
		row
			add: #theList;
			add: #theButton
	] height: 30;
	yourself
{% endhighlight %}

{% highlight smalltalk %} 
^ SpecLayout composed
	newColumn: [ :column |
		column
			add: #theList;
			add: #theButton
	] width: 30;
	yourself
{% endhighlight %}

Note that it is generally considered a bad habit to hardcode the size of the widgets.
Methods are available on *ComposableModel* providing sensible default sizes, like the width of a button.
When specifying custom widget sizes, care should be taken to take in account the current font size.

---

<a name="layout_percentage"></a>
It is also possible to specify the percentage of the container, e.g. the window, that a widget should occupy.
As a result of this, the widget size will change accordingly when the container is resized.
To do so, the proportional position of the four sides of a widget can be specified, as shown in this example.

{% highlight smalltalk %}
^ SpecLayout composed
	add: #theButton top: 0.25 bottom: 0.25 left: 0.25 right: 0.25;
	yourself
{% endhighlight %}

For each edge, the proportion indicates at what percentage of the overall container the edge should be placed.
Zero percent is the container edge, 100 percent is the opposite container edge.
For example, for the top edge, the percentage is counted from the top down: 0 is the top edge, and 1 is the bottom edge.


Also, the argument can be an integer if the offset has to be a fixed number of pixels.
The number of pixels should be positive, as it indicates a distance from the corresponding edge, going to the opposite edge.

---

<a name="layout_expert"></a>
The previous examples should cover most of the cases of layout of widgets.
For the remaining cases there is a last way to specify a widget by specifying its position.

The method `add: origin: corner:` of **SpecLayout** specifies the layout of a widget, percentage-wise from the origin point to the corner point.
These two points represent respectively the top left corner and the bottom right corner of the widget.
The arguments express a percentage of the container, so these __must__ be between *0@0* and *1@1* .

In addition to those points, two offsets can be also be specified, using the method `add: origin: corner: offsetOrigin: offsetCorner:`.
The offsets specify the number of pixels that the origin and the corner should be moved.

Contrary to the previous way to define layouts, while using `add: origin: corner:  offsetOrigin: offsetCorner: ` the offset can be negative.
The offset expresses the number of pixels from the corresponding corner, in the classical computer graphics coordinate system where the origin is in the top left corner.
Note that this approach is similar to the ProportionalLayout of **Morphic**.


The following example shows how to add a widget as a toolbar.
It specifies that the widget in the `toolbar` instance variable should take all the window width, but should be only 30 pixels in height.

{% highlight smalltalk %}
^ SpecLayout composed
	add: #toolbar origin: 0@0 corner: 1@0 offsetOrigin: 0@0 offsetCorner: 0@30;
	yourself
{% endhighlight %}

---

<a name="layout_specify_layout"></a>
All the methods for adding sub widgets can be found in the *commands* and *commands-advanced* protocols of **SpecLayout**.


As explained at the beginning of this page, a UI can have multiple layouts.
So when the layout of a widget that is composed of multiple sub-widgets is defined, and this widget contains multiple layout methods that determine the layout of its sub-widgets, the layout method to use can be specified.

All the methods seen in the previous examples come with a variant used to specify which selector to use for the layout method.
By example, for the `add:` method the variant is  `add:withSpec:`.

For example, consider a widget **MyWidget** defining a first layout method `firstLayout` as the default layout and another layout method called `anotherLayout`.
The following example shows how to add an instance of **MyUserInterface** using its `anotherLayout` layout method.

{% highlight smalltalk %}
^ SpecLayout composed
	add: #MyUserInterface withSpec: #anotherLayout;
	yourself
{% endhighlight %}