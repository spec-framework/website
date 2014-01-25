---
layout: docs
title: Creating new basic widgets
prev_section: api
next_section: adapters
permalink: /docs/own-model/
---

*Spec* provides for a large amount and wide variety of basic widgets. 
In the rare case that a basic widget is missing, the 
*Spec* framework will need to be extended to add this new widget.
In this section we will explain how to create such a new basic widget.


We will first explain the applicable part of how the widget build process is performed.
This will reveal the different actors in the process and provide a clearer understanding of their responsibilities.
We then present the three steps of widget creation: writing a new model, writing an adapter, and updating or creating an individual UI framework binding.


<a name="one_step" class="hash"></a>
## One step in the building process of a widget <a href="#one_step" class="permalink" title="Permalink"><i class='fa fa-link'></i></a>


The UI building process does not make a distinction between basic and composed widgets.
Hence, at a specific point in the building process of a basic widget the default spec method of the widget is called, just as if it would be a composed widget.
However in this case, instead of providing a layout for multiple widgets that comprise the UI, this method builds an adapter to the underlying UI framework.
Depending of the underlying UI framework that is currently used, this method can provide different kind of adapters, for example an adapter for Morphic, or an adapter for Seaside, etc.


The adapter, when instantiated by the UI model, will in turn instantiate a widget that is specific to the UI framework being used.


For example, when using a List in the Morphic UI, the adaptor will be a MorphicListAdapter and it will contain a PluggableListMorph.
This is this framework-specific widget that will be added to the widget container.


Figure [1.1](#model_adapter_uielement) shows the relationship between those objects.


<a name="model_adapter_uielement"></a><p class="figure">![model_adapter_uielement]({{ site.url }}/figures/Model-Adapter-UIElement.png "Relationship between the model, the adapter, and the UI element")</p>


<a name="the_model" class="hash"></a>
## The Model <a href="#the_model" class="permalink" title="Permalink"><i class='fa fa-link'></i></a>


The new model needs to be a subclass of 
**AbstractWidgetModel** and its name should be composed of the new basic widget concept, e.g. list or button, and of the word 
*Model*.
The responsibility of the model is to store all the state of the widget.
Examples of widget-specific state are:


-  the index of a list
-  the label of a button
-  the action block for when a text is validated in a text field


The state is wrapped in reactive variables and kept in instance variables.
For example, the code following shows how to wrap the state 
`0` in a reactive variable and keep it as an instance variable.
reactive variables are needed because they are later used to propagate state changes and thus create the interaction flow of the user interface, as discussed in the page [Sub&nbsp;widgets interaction]({{ site.url}}/docs/interactions/).

{% highlight smalltalk %}
index := 0 asValueHolder.
{% endhighlight %}


<p>For each instance variable that holds state three methods should be defined: the getter, the setter, and the registration method.
The first two should classified in the protocol named 
<strong>protocol</strong> while the registration method should be in 
<strong>protocol-events</strong>.</p>

<p>For example, the methods to define for the previous example can be implemented as following:</p>


{% highlight smalltalk %}
"in protocol: protocol"
index
	^index value
 
"in protocol: protocol"
index: anInteger
   	index value: anInteger
   
"in protocol: protocol-events"
whenIndexChanged: aBlock
   	index whenChangedDo: aBlock
{% endhighlight %}


<p>The last step to define a new model is to implement a method 
<code>adapterName</code> at the class side.
The method should be in the protocol named 
*spec* and should return a symbol.
The symbol should be composed of the basic concept of the widget, e.g. list or button, and the word <strong>Adapter</strong> like <strong>ListAdapter</strong>.</p>


<p>The communication from the UI model to the adapter is performed using the dependents mechanism.
This mechanism is used to to handle the fact that a same model can have multiple UI elements concurrently displayed.
In fact the message 
<code>change: with:</code> is used to send the message 
<emph>selector</emph> with the arguments 
<emph>aCollection</emph> to the adapter.</p>


{% question %}
Note: For Ben. I still don't understand it. Please explain to me tomorrow.
{% endquestion %}



<a name="the_adapter" class="hash"></a>
## The Adapter <a href="#the_adapter" class="permalink" title="Permalink"><i class='fa fa-link'></i></a>


An adapter must be a subclass of 
**AbstractAdapter**.
The adapter name should be composed of the UI framework name, e.g. Morphic, and the name of the adapter it is implementing, e.g. ListAdapter.
The adapter is an object used to connect a UI framework specific element and the framework independent model.


The only mandatory method for an adapter is 
`defaultSpec` on the class side.
This method has the responsibility to instantiate the corresponding UI element.


The next example shows how **MorphicButtonAdapter** instantiates its UI element.

{% highlight smalltalk %}
defaultSpec
    <spec>

	^ {#PluggableButtonMorph.
 		#color:. Color white.
		#on:getState:action:label:menu:. 	#model. #state. #action. #label. nil.
   		#getEnabledSelector:. 				#enabled.
   		#getMenuSelector:.					#menu:.
   		#hResizing:. 						#spaceFill.
   		#vResizing:. 						#spaceFill.
   		#borderWidth:.						#(model borderWidth).
   		#borderColor:.						#(model borderColor).
   		#askBeforeChanging:.					#(model askBeforeChanging).
   		#setBalloonText:.					#(model help).
   		#dragEnabled:.						#(model dragEnabled).
   		#dropEnabled:.						#(model dropEnabled).	
   		#eventHandler:.						#(EventHandler on:send:to: keyStroke keyStroke:fromMorph: model)}
{% endhighlight %}


<p>Since the adapter is bridging the gap between the element of the UI framework and the model, the adapter also needs to forward the queries from the UI element to the model.
Seen from the other way around: since the model is holding the state, the adapter is used to update the UI element state of the model.</p>


<p>The methods involved in the communication from the model to the adapter as well as the methods involved in the communication from the adapter to the UI model should be in the protocol 
<emph>spec protocol</emph>.
On the other hand the methods involved in the communication from the adapter to the UI element and vice versa should be categorized in the protocol 
<emph>widget API</emph>.</p>


<p>To communicate with the UI element, the adapter methods use the method 
<code>widgetDo:</code>.
This method executes the block provided as argument, which will only happen after the UI element has already been created.</p>


<p>The follwoing example shows how 
<strong>MorphicLabelAdapter</strong> propagates the modification of the emphasis from the adapter to the UI element.</p>



{% highlight smalltalk %}
emphasis: aTextEmphasis

	self widgetDo: [ :w | w emphasis: aTextEmphasis ]
{% endhighlight %}


<a name="the_binding" class="hash"></a>
<h2>The UI Framework binding <a href="#the_binding" class="permalink" title="Permalink"><i class='fa fa-link'></i></a></h2>



<p>The bindings is an object that is used to resolve the name of the adapter at run time.
This allows for the same model to be used with several UI frameworks.</p>


<p>Adding the new adapter to the default binding is quite simple.
It requires to update two methods: 
<code>initializeBindings</code> in 
<strong>SpecAdapterBindings</strong> and 
<code>initializeBindings</code> in the framework specific adapter class, e.g. 
<strong>MorphicAdapterBindings</strong> for Morphic.</p>



{% question %}
For Ben: Give an example here.
{% endquestion %}


Once this is done, the bindings should be re-initialized by running the following snippet of code: 
`SpecInterpreter hardResetBindings`.


For creating a specific binding, the class 
**SpecAdapterBindings** needs to be overriden as well as its method 
`initializeBindings`.
It can then be used during a spec interpretation by setting it as the bindings to use for the 
**SpecInterpreter**.
The next example shows how to do so.

{% highlight smalltalk %}
SpecInterpreter bindings: MyOwnBindingClass new.
{% endhighlight %}


{% info %}
The **SpecInterpreter** bindings are reset after each execution.
{% endinfo %}