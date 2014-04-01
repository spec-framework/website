---
layout: news_item
title:  Fix splitters
author: BenjaminVanRyseghem
date:   2014-05-01 14:46:32
categories: fix splitters
---

Glad to announce that with the help of [Sean P. DeNigris](http://seandenigris.com) the problem
of splitters in Spec has been tackled.

The problem was revealed when multiple morphs were present after a splitter.
In this configuration the two morphs moved in a really strange way.

The fix<sup><a href="https://github.com/spec-framework/spec/commit/b59dcf9e9cd0ca3bc2850f4b54aaeb394b9517a4"><i class='fa fa-github-alt'></i></a></sup> provides a 
new algorithm to retrieve the morphs which should be attached to each splitters.

The fix should be into Pharo as soon as the [slice](https://pharo.fogbugz.com/default.asp?13163) is integrated.