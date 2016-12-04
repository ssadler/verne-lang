# Verne-Lang

Verne-Lang is a high level, lazy, superficially typed, autocomplete aware langauge, developed inside and also targeting the web.

This is the language component of Verne, a work-in-progress social infrastructure for the web.

## Overview

This project has several goals. Overall the aim is to introduce an architecture which will make publishing and ultimately creation of social apps accessible to more people, and in a way that does not require users to sacrifice their privacy. The strategies are:

### Alter application lifecycle

Put another way, unify the lifecycle of code and application data. Rather than code living in versioned, immutable trees, and application state living in non forkable and non mergeable data silos, reference everything from the same immutable tree. This way, an application can be forked at any time, and every state can be addressed as the hash of the code + the hashes of the inputs.

### Develop applications inside the browser

It should be possible to fork a running application at any point and alter it's behaviour. It should also be possbible to create interactive environments for collaborative development inside the browser.

### Make programming easier and more accessible

The Verne Language has a component system, in which components are analgous to objects. Components are typed, and may provide their own autocomplete functionality in order to facilitate the task of programming. In this regard, Verne has some similarities to a spreadsheet application:

* Useful without knowing how to code
* Easy to do something useful
* Highly interactive and generally helpful

This is akin to a tadpole's transition to becoming a frog; it needs to have a surface at water level to crawl up onto.

### Features:

* Simple, LISP-like syntax with extensible literals.
* Powerful built-in and extensible autocomplete.
* Typed component market (think Wordpress).
* Aimed at content publishers, web designers, social app makers, hackers, data engineers.
* Persistent; application state versioned and forkable, can also be exported to GIT.
* Full stack; corresponding serverside application, identity aware, LDAP driven ACL/ACP.
* App for identity management & chat.

