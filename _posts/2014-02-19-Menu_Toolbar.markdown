---
layout: news_item
title:  Menu Toolbar
author: BenjaminVanRyseghem
date:   2014-02-19 00:01:32
categories: improvement menu toolbar
---

Spec provides today a new missing feature: a menu toolbar.

As recently suggested in the mailing list, a menu toolbar is often a requirement to build an application.
In order to provide an easy way to add a menu toolbar, Spec reuse **MenuModel** to generate the toolbar entries.

So now, a **MenuModel** can be used in two cases: to define a toolbar, or to define a contextual menu.

An example can be opened via

    ApplicationWithToolbar new openWithSpec

<a name="fig_ApplicationWithToolbar"></a><p class="figure">![fig_ApplicationWithToolbar]({{ site.url }}/figures/ApplicationWithToolbar.png "An example of application with a toolbar")</p>