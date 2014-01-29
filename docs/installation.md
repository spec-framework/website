---
layout: docs
title: Installation
prev_section: quickstart
next_section: example
permalink: /docs/installation/
---

To fetch the latest *Spec* version, you first need to get a local copy of the repository:

{% highlight bash %}
~ $ cd /tmp
tmp $ git clone https://github.com/SpecForPharo/spec.git
{% endhighlight %}

Then open your image and execute the following script:

{% highlight smalltalk %}
| repo names currentVersions toLoad |

repo := MCFileTreeRepository new directory: '/tmp/spec' asFileReference.

currentVersions := MCWorkingCopy allManagers
	select: [ :e | (e ancestry ancestors) notEmpty ]
	thenCollect: [ :e | e ancestry ancestors first name ].
names := repo packageDescriptionsFromReadableFileNames.
toLoad := names select: [ :e || currentVersion | 
	currentVersion := e first , '-', e second , '.', e third asString.
	(currentVersions includes: currentVersion) not ].

toLoad do: [ :name || version |
	version := repo versionFromFileNamed: name first, '.package'.
	[ version load ]
		on: MCMergeOrLoadWarning
		do: [ :ex | [ ex load  ] on: MCNoChangesException do: [] ] ].
{% endhighlight %}

Your image contains now the latest stable version of *Spec*.