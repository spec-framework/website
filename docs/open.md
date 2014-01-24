---
layout: docs
title: Open your UI
prev_section: layout
next_section: 
permalink: /docs/open/
---

The user&nbsp;interface we just implemented is ready to be opened.
To open it in a window, the message `openWithSpec` needs to be sent to an instance of the previously implemented class **MyUserInterface**.

{% highlight smalltalk%}
MyUserInterface new openWithSpec
{% endhighlight %}

A window should appears where you can site the list with ten elements and the button labbelled *I am a button*.

In addition, *Spec* provides support to open a model directly as a dialog window.
For this, the method used to open the user&nbsp;interface must be `openDialogWithSpec`.

{% highlight smalltalk%}
MyUserInterface new openDialogWithSpec
{% endhighlight %}

The dialog window is opened with a toolbar contening a **cancel** button and an **ok** button.
To set the action to perform when ok is pressed, the hook method `initializeDialogWindow:` requires to be implemented.
The following example shows how to open a debugger when ok is pressed.

{% highlight smalltalk%}
initializeDialogWindow: aWindow

	aWindow okAction: [ self halt ].
{% endhighlight %}