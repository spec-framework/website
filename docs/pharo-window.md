---
layout: docs
title: Use Pharo window
prev_section: dynamic
next_section: api
permalink: /docs/use-pharo-window/
---

*Spec* provides a hook for any application to open in the Pharo window.
This allows to simulate a native window application (with the restriction of a single window application).

To do so, you simply need to open your application with the `openWorldWithSpec` message.

By example opening a **MessageBrowser** usually results to a new window into the Pharo window.

{% highlight smalltalk %}
(MessageBrowser
	browseMessages: Object methods
	refreshingBlock: [] 
	named: '') openWithSpec
{% endhighlight %}

This code produces a new window looking like
<a name="fig_spec_pharo_browser"></a><p class="figure">![fig_spec_pharo_browser]({{ site.url }}/figures/SpecMessageBrowser.png "A MessageBrowser into Pharo")</p>

To open a similar **MessageBrowser** using the Pharo window, the following code can be executed

{% highlight smalltalk %}
(MessageBrowser
	browseMessages: Object methods
	refreshingBlock: [] 
	named: '') openWorldWithSpec
{% endhighlight %}

It will resize the Pharo window and change its title as seen in the figure
<a name="fig_world_pharo_browser"></a><p class="figure">![fig_world_pharo_browser]({{ site.url }}/figures/WorldMessageBrowser.png "A MessageBrowser using the Pharo window")</p>