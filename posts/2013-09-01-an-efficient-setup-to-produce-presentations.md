---
title: An efficient setup to produce presentations
author: Mog
---

Recently I had to write several presentations and I wasn't happy with
the tools I was used to ([Beamer](https://en.wikipedia.org/wiki/Beamer_%28LaTeX%29) + [AucTEX](https://www.gnu.org/software/auctex/)).

I read a little bit about what my friends were currently using and
gave a try to [several HTML5/CSS3/js solutions](http://www.bortzmeyer.org/logiciel-presentation-nouveau.html). The one that impressed
me the most was [DZSlides](http://paulrouget.com/dzslides/) by Paul
Rouget, especially for its awesome control panel (called shell in the
DZSlides doc). Even the basic `onstage.html` one, provided with the
vanilla framework, gives you lots of tools to work with: a preview of
the next slide, a timer and notes hidden from the slides to help you
out during the presentation.

The basics of DZSlides are simple: you complete an HTML template with
your slides (specified with regular HTML5 and CSS3), and use a shell
(another html file), to launch a presentation. For example, if your
presentation file is `slides.html` and if you want to use the
`onstage.html` shell, you'd have to browse to the address
`onstage.html#slides.html`.

The main problem with that particular framework from my perspective
was quite embarassing: it required a good habit of producing HTML5 and
CSS3 to be productive, and those are two technologies I barely
touched recently.

Here enters
[Pandoc](http://johnmacfarlane.net/pandoc/demo/example9/producing-slide-shows-with-pandoc.html)
and its ability to turn Markdown into several presentation frameworks
outputs. The page linked provides a quick and sufficient introduction
on the specifics of Pandoc's Markdown conversion. By using Pandoc
together with two specific tools, writing nice presentations turns out
to be quick and comfortable.

The first of those two specific tools is a small Makefile that
provides recompilation by Pandoc at modification of the Markdown
presentation files (here using `.md`) present in the directory:

    MARKDOWN = $(shell ls *.md)
    HTML = $(MARKDOWN:.md=.html)
    ONSTAGED = $(HTML:%=onstage.html\#%)
    
    all: $(HTML)
    
    %.html: %.md
    	pandoc -sf markdown -t dzslides $< -o $@
    	touch onstage.html
    
    view: $(HTML)
    	firefox $(ONSTAGED)
    	while :; do inotifywait -qqe modify $(MARKDOWN); make; done
    
    .PHONY: all view

It requires
[inotify-tools](https://github.com/rvoicilas/inotify-tools/wiki) or
something with similar functionnality.

The second tool is the firefox addon
[AutoReload](https://addons.mozilla.org/en-us/firefox/addon/auto-reload/)
which reloads local files at modification (and it's why there's a
`touch` in the makefile, we need to modify the shell file if we want
it to refresh).

With those two tools combined, the workflow is straightforward: at the
beginning of a session, fire up a terminal and do a

    make view

in the presentation directory.

Afterwards, the presentation page is refreshed after each modification
and is written in an intuitive and efficient language,
e.g. Markdown. Enjoy! The only thing that I'm bothered with at this
point is that it loses navigation info, e.g. at which page of the
presentation you were before the refresh. Any idea on how to handle
that will be appreciated.