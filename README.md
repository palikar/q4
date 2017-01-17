# Browse 4chan completely in ~~parenthesis~~ Emacs Lisp.

[![LOOK I CAN USE GIMP](https://raw.githubusercontent.com/desvox/q4/brainfuck/qbanner.png)](#banner)

**REQUIRES EMACS 25**

This is an early stage of development, the internals are in a state of chaos as new features are being added. If you've stumbled on this repo during this stage, or if I was crazy enough to link it to you already, please refer to the header comments in q4.el, as its currently the only documentation or detailed description available. It includes compatibility notes and a usage guide.

If the colors are ugly, they try to set themselves based on whether your theme is dark or light using emacs' own face system. If this fails for you, call the functions q4/set-light-colors or q4/set-dark-colors: they will force the right ones into place.

This repo currently has a few branches; master is kept mostly stable but the others are where volatile development and refactoring are happening. If you just want to use Q4, use master. The other branches exist only to ease tracking of development and might be completely broken at any given time.

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
  
### Changes for Jan 16:
  * Navigational polish: the reply viewing system, bound to r, is now complete, offering easy forward and backward navigation down a reply tree, with a header line to indicate position. Use q to ascend back to a parent node. Some time besides now™ I'll get this same system working to navigate UP a reply tree, as well as downward. Additionally, Evil users now can use shift+J/K to scroll the page up/down linewise instead of the previous linewise cursor movement.
  * Thumbnails now are cached in memory, and are reused instead of redownloaded in the reply navigator.
  * Added detailed usage guide to the header comment, and made the emacs 24 warning a bit friendlier
  * New function q4/go-back, bound to q, will handle buffer, window, and navigational management based on context. Use Q to just slaughter shit unconditionally.
  * Renaming a buffer is bound to @, and renamed buffers require confirmation before killing to make them less prone to being closed accidentally. There are no more keys reserved for evil's macro system in Q4 buffers, hopefully nobody else needed that anyway :^)
  
  
### Feedback and bug reports
You can file github issues for any reason you'd like. They don't have to be bug reports, they can be suggestions, complaints, etc. I also watch the daily programming thread on /g/, if you would like to speak with me on 4chan just drop a mention there and I'll see it sooner or later.

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

