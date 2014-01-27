---
layout: docs
title: Dynamic UI
prev_section: interactions
next_section: api
permalink: /docs/dynamic/
---

Having an user interface with a well known number of sub widgets and a static layout is not always sufficient. 
A user interface is often more than just that, for example here are two situations where more is needed: 
First, when the layout of the user interface needs to be changed at runtime to match the execution context of the software.
Second, sub widgets are added or removed at runtime and therefore the programmer needs to be able to parametrize those new sub widgets on the fly.


*Spec* also provides support for such dynamic user interfaces.
In this section we show how to use 
*Spec* in these situations.


To be able to compose dynamic user interfaces at run time, a new method has been introduced.
This method is 
`assign:to:` and take a model instance as a first argument, and a unique symbol as a second argument.
This way composing dynamic model is as simple as composing any other user interface.


First, we talk about making dynamic modifications of the layout of widgets, and second we discuss the dynamic adding and removing of subwidgets.
Third and last we show how the dynamic features can be used to quickly prototype a user interface.


<a name="layout"></a>
## Dynamic modification of the layout <a href="#layout" class="permalink" title="Permalink"><i class='fa fa-link'></i></a>


Changing the layout of widgets at runtime is straightforward, as we will see here.
Such changes basically consist of three steps:


1.  creating the new layout,
2.  setting a flag to prohibit the creation of a new UI element (and instead reuse the existing one),
3.  building the UI again with the newly created layout.


The following code is an example of rebuilding a widget with a new layout.
First, a helper method is used to obtain a 
`SpecLayout` object that determines the new layout.
Second, the 
`needRebuild` flag is set to 
`false` such that the existing UI element is reused.
This leads to the replacement of the content of the current container instead of just instantiating a new UI element.
Third, the rebuilding of the user interface is performed.

{% highlight smalltalk %}
    rebuildWithNewLayout
    	| newLayout |
    
    	newLayout := self newLayoutCreatedDynamically.
    	self needRebuild: false. "tells the interpreter to keep my current UI element"
    	self buildWithSpecLayout: newLayout. "rebuilds me with the new layout"
{% endhighlight %}


One widget can also keep the UI elements of its sub widgets which do not need to be rebuilt.
The message 
`needRebuild: false` needs to be sent to any of those sub widgets.
For example, if a model comprising a 
*button* and a 
*list* just wants to rearrange the position of these UI elements, there is no need to rebuild them, i.e. instantiate new UI elements for them.
To prevent this, the message 
`needRebuild: false` should be send to them, as shown in the following example.


{% highlight smalltalk %}
    rebuildWithNewLayout
    	| newLayout |
    
    	newLayout := self newLayoutCreatedDynamically.
    	self needRebuild: false.
    	theButton needRebuild: false.
    	theList needRebuild: false.
    	self buildWithSpecLayout: newLayout.
{% endhighlight %}


<a name="adding_removing"></a>
## Dynamic adding and removal of subwidgets <a href="#adding_removing" class="permalink" title="Permalink"><i class='fa fa-link'></i></a>


If a user interface needs a varying number of subwidgets, the amount of which cannot be established at compilation time, then another approach is needed.
In this scenario, 
`DynamicComposableModel` is the model that needs to be subclassed, as this class provides support for the required kind of dynamic behavior.
Amongst others, this class adds the method `assign:to:`, which takes a model instance as a first argument, and a unique symbol as a second argument.

This method is used to assign an already instantiated model as sub widget while the method `instantiateModels:` takes a class name as argument.
The fact the method is based on classes prevent to use a dynamically created model as sub widget.

When using 
`DynamicComposableModel`, the instantiation of the sub widgets is a bit different from normal use.
In the 
`instantiateWidgets` method, instead of instantiating each widget separately, 
`instantiateModels:` should be used to instantiate them.
This method takes as argument an array of pairs, where each pair is composed of the unique name of the widget as key, and the name of the widget class as value.
This allows for a widget to be accessed by sending a message whose selector is the widget name to the model.


By example, if a widget named 
`button` is created, then this widget can be accessed by calling 
`self button` as shown in the following example.




{% highlight smalltalk %}
    self instantiateModels: #( button ButtonModel ).
    	self button label: 'Click me'.
{% endhighlight %}


<a name="examples"></a>
## Examples: Prototyping a UI <a href="#examples" class="permalink" title="Permalink"><i class='fa fa-link'></i></a>


Thanks to the capability of 
*Spec* to dynamically instantiate widgets, it is also possible to prototype a user interface from within any workspace.
The following examples show how 
*Spec* can be used to prototype quickly a user interace.


The first example explains how to design by prototyping a user interface.
The second example introduce the composition of dynamic models.


<a name="popup"></a>
## Designing a pop up <a href="#popup" class="permalink" title="Permalink"><i class='fa fa-link'></i></a>


This example shows how to easily and quickly design a popup window asking for an input.


First we create a simple model with two sub widgets, a label and a text field, as shown by the following snippet.




{% highlight smalltalk %}
    view := DynamicComposableModel new
    	instantiateModels: #(label LabelModel text TextInputFieldModel);
    	yourself.
{% endhighlight %}


We can then specify the title and the initial size of the widget, adding the following code:




{% highlight smalltalk %}
    view extent: 300@90;
    	title: 'Choose your project'.
{% endhighlight %}


Then we specify the UI element layout.
It will be only one row with the label and the text field.
The following snippet shows the layout definition.




{% highlight smalltalk %}
    layout := SpecLayout composed
    	newRow: [ :r | r add: #label width: 75; add: #text ];
    	yourself.
{% endhighlight %}


To have a first idea of the resulting UI, we can already open it as follows: 
`view openWithSpecLayout: layout`.
This UI however does not have any sub widget state or behavior so there is not much to see or do at this point.


The next step is to set up the sub widget state and behavior.
We set the text of the label as well as the ghost text of the textfield.
We also specify here that the text field should automatically accept the text on each keystroke, such that it does not show the yellow 'edited' triangle on the top right.
This is shown in the code:




{% highlight smalltalk %}
    view label text: 'Packages:'.
    
    view text
    	autoAccept: true;
    	entryCompletion: nil;
    	ghostText: '.*'.
{% endhighlight %}


Opening the UI again (
`view openWithSpecLayout: layout`) now shows the text of the label and the ghost text of the text field.


As we want the widget to be a popup with a single button 
*Ok*, the toolbar to use should be defined explicitly (the default toolbar has an 
*Ok* button and a 
*Cancel* button).
We also set the toolbar action for when 
*Ok* is clicked: the current text of the text field will be saved in the instance variable 
*regex*.
The following code shows how to do it.




{% highlight smalltalk %}
    toolbar := OkToolbar new
    	okAction: [ regex := view text text ];
    	yourself.
{% endhighlight %}


We can also add a shortcut to the text field on 
*Enter* to simulate the click on 
*Ok*.
The following code illustrates how to set up such a shortcut.




{% highlight smalltalk %}
    view text 
    	bindKeyCombination: Character cr asKeyCombination 
    	toAction: [ toolbar triggerOkAction ].
{% endhighlight %}


This completes the specification of the UI.
As a final step, when opening it we pass it the toolbar and configure it to be centered in the Pharo window and modal.
The final version of the code is:




{% highlight smalltalk %}
    view := DynamicComposableModel new
    	instantiateModels: #(label LabelModel text TextInputFieldModel);
    	extent: 300@90;
    	title: 'Choose your project'
    	yourself.
    	
    view label text: 'Packages:'.
    
    layout := SpecLayout composed
    	newRow: [ :r | r add: #label width: 75; add: #text ];
    	yourself.
    
    view text
    	autoAccept: true;
    	entryCompletion: nil;
    	ghostText: '.*'.
    	
    toolbar := OkToolbar new
    	okAction: [ regex := view text text ];
    	yourself.
    	
    view text 
    	bindKeyCombination: Character cr asKeyCombination 
    	toAction: [ toolbar triggerOkAction ].
    	
    (view openDialogWithSpecLayout: layout)
    	toolbar: toolbar;
    	centered;
    	modalRelativeTo: World.
{% endhighlight %}


The result can be seen in Figure 
[1.1](#fig_popup).


<a name="fig_popup"></a><p class="figure">![fig_popup]({{ site.url }}/figures/Popup.png "Prototype of a popup")</p>

<a name="composing"></a>
## Composing dynamic models <a href="#composing" class="permalink" title="Permalink"><i class='fa fa-link'></i></a>


This exemple shows in three parts how to buid a simple code browser.


First a simple list widget is created displaying all the subclasses of AstractWidgetModel.




{% highlight smalltalk %}
    m := DynamicComposableModel new.
    m instantiateModels: #( list ListModel ).
    m list items: (AbstractWidgetModel allSubclasses sorted: [:a :b | a name < b name ]).
    m layout: (SpecLayout composed
    	add: #list;
    	yourself).
    m openWithSpec.
{% endhighlight %}


Then the list widget is reused to build a viewer widget displaying the protocol methods of the selected class.




{% highlight smalltalk %}
    m2 := DynamicComposableModel new.
    m2 assign: m to: #classes.
    m2 instantiateModels: #( methods ListModel ).
    m list whenSelectedItemChanged: [ :item | 
    	item 
    		ifNil: [ m2 methods: #() ]
    		ifNotNil: [ m2 methods items: ((item selectorsInProtocol: 'protocol') sorted) ] ].
    m2 layout: (SpecLayout composed
    	newRow: [ :r | r add: #classes; add: #methods ];
    	yourself).
    m2 openWithSpec.
{% endhighlight %}


Finally the last widget is defined with the viewer previously created.
In addition, a text zone is edited to show the selected method source code.




{% highlight smalltalk %}
    m3 := DynamicComposableModel new.
    m3 assign: m2 to: #top.
    m3 instantiateModels: #( text TextModel ).
    m2 methods whenSelectedItemChanged: [ :selector | 
    	selector ifNil: [ m3 text text: '' ] ifNotNil: [ m3 text text: (m list selectedItem >> selector ) sourceCode ] ].
    m3 layout: (SpecLayout composed
    	newColumn: [ :c | c add: #top; add: #text ];
    	yourself).
    	
    m3 openWithSpec.
    m3 title: 'Protocol browser'
{% endhighlight %}


The final result looks like the Figure 
[1.2](#ex_browser).

<a name="ex_browser"></a><p class="figure">![ex_browser]({{ site.url }}/figures/Protocol_Browser.png "Prototype of Protocol Browser")</p>