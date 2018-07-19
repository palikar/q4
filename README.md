# Browse 4chan completely in ~~parenthesis~~ Emacs Lisp.

**REQUIRES EMACS 25**

~~I am no longer developing this package. It's a fully functional browser but lacks a few features I initially planned because I decided not to waste any more time browsing chans then neccesary :^)~~
In this fork I plan on implementing features that I think are important for any 4chan client.
This is mainly actually being able to post on 4chan, and not just being a read only pleb.
To accomplish this I'll probably use the xwidget features.
After that, added features from 4chanx will be natural conclusion.

A few bullet points:
  * Uses the JSON api, no web scraping pleb shit.
  * No third party emacs dependencies, just a recent build of GNU Emacs (25+).
  * Full thumbnail support
  * Full resolution media support with the 3rd party programs `feh` and `mpv`
  * `wget` integration to download thread photos
  * Comfy navigation features like quote-hopping, URL scraping, viewing replies, and inline quote expansion.
  * Board, thread, and post crosslink support.
  * Send threads and post numbers to your native browser for easy replying
  * Integrates with Evil, Helm, and Ivy when they are available.

TODO:
Being able to reply
etc


### Feedback and bug reports
You can file github issues for any reason you'd like. They don't have to be bug reports, they can be suggestions, complaints, etc. I am not adding new features myself anymore but if I see an issue for it pop up, i will probably be willing to work on it.

### Default Keybinds
If you use evil, these are mostly the same except j/k replace n/p, page refreshing is done with R instead of g, and SPC/DEL is RET/DEL, and l to recenter is not bound to anything right now. Check the source for definitive definitions. As is always the case with emacs, these can be changed.

    SPC         q4/point-to-next-post
    DEL         q4/point-to-previous-post
    B           q4/board-overview
    n           q4/point-to-next-post
    p           q4/point-to-previous-post
    N           scroll-up-line
    P           scroll-down-line
    l           q4/recenter
    q           q4/go-back
    Q           quit-window (and kill)
    ]           q4/quote-hop-backward
    [           q4/pop-mark
    {           q4/unexpand-quotes
    }           q4/expand-quotes
    r           q4/show-replies (splits the window)
    t           q4/toggle-thumbnails
    a           q4/pass-to-feh
    A           q4/wget-threadpics
    i           q4/open-post-image
    o           q4/open-thread
    u           q4/list-urls
    U           q4/view-content-externally
    g           q4/refresh-page
    @           rename-buffer
    <f5>        q4/refresh-page
    <tab>       forward-button
    <backtab>   backward-button
