---
layout: news_item
title:  El Sprint Chileno
author: BenjaminVanRyseghem
date:   2014-01-24 15:26:32
categories: sprint chile
---

After a cool sprint yesterday, a couple of *Spec* issues has been tackled <i class='fa fa-smile-o fa-2x'></i>.

[**Case 12695**](https://pharo.fogbugz.com/default.asp?12695): Fix *InputWidget* example and add a new one. These examples are showing how to set up a window modality. Thanks Samir Saleh and [Sean P. DeNigris](http://seandenigris.com) for this fix.

[**Case 12683**](https://pharo.fogbugz.com/default.asp?12683): The labels background is changing when the containing model is open as world (with `openWorldWithSpec`).

[**Case 12684**](https://pharo.fogbugz.com/default.asp?12684): Renaming **NewValueHolder** into **ReactiveVariable**. But since Pharo 3 is already in beta phase, changing the API was not possible. So instead two hooks were introduced to allow the integration of code from Spec bleeding edge to Pharo 3. For new code (and all code in future versions of Pharo), use the following two hook methods:

- `asReactiveVariable`: use wherever you would have used `asValueHolder`, which will soon be deprecated. For now, it returns a **NewValueHolder**. In the future it will return a (polymorphic) **ReactiveVariable**
- `selectionReactiveVariable`: as above, returns a **SelectionValueHolder** for now. This extension method allows the two different packages to coexist without having direct references from the Spec model.

Thanks a lot [Sean P. DeNigris](http://seandenigris.com) for you help and energy.