---
layout: news_item
title:  Improvements in DynamicComposableModel
author: BenjaminVanRyseghem
date:   2014-01-21 11:52:52
categories: dynamic composition
---

Right now it is easy to prototype a UI in a workspace or to dynamically build a model on demand at run time, but this prototype can not be reused for composition.

Because of this, the prototyping is quite limited.

But the latest Spec commit<sup><a href="https://github.com/SpecForPharo/spec/commit/5d28c1f293bb1654e7395c18a2ea733fbd551d10"><i class='fa fa-github-alt'></i></a></sup> introduces few changes on `DynamicComposableModel` to tackle this issue.

The problem was double: no way to add an instanciated model as a sub widget, and the lookup for the layout was looking for a selector on class side.

The first problem was actually just a method missing since the `DynamicComposableModel` class manage the widgets in a dictionary.

The second problem was slighty more tedious. To work, it requires from the embedded sub widget to have a layout defined. A new instance variable `layout` has been introduce to hold the **SpecLayout** instance used to position the sub widgets. This way, when a layout is requested for the dynamic model, this layout is returned (*regardless of the selector requested*).

These two fixes add support for a better composition in the dynamic context  <i class='fa fa-smile-o fa-2x'></i>.

If you want to try, this [snippet](https://gist.github.com/BenjaminVanRyseghem/cd02f2dc61f350a6495f) gives you an example of how to build such unser interface.