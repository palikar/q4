# Browse 4chan completely in ~~parenthesis~~ Emacs Lisp.

[![LOOK I CAN USE GIMP](https://raw.githubusercontent.com/desvox/q4/brainfuck/qbanner.png)](#banner)

**REQUIRES EMACS 25**

This is an early stage of development, the internals are in a state of chaos as new features are being added. If you've stumbled on this repo during this stage, or if I was crazy enough to link it to you already, please refer to the header comments in q4.el, as its currently the only documentation or detailed description available.

If the colors are ugly, they try to set themselves based on whether your theme is dark or light using emacs' own face system. If this fails for you, call the functions q4/set-light-colors or q4/set-dark-colors: they will force the right ones into place.

This repo currently has two branches, master and the brainfuck branch. Master is kept mostly stable and usable, brainfuck is where I'm doing large amounts of changes that cause regressions across the program that need to be reworked. In that branch I am moving things to a buffer-local metadata system which allows for much more useful and reliable features, like reply threading and tree-sorted browsing (like reddit comments). As the name implies, it may be only somewhat functional at any given time. Currently master is up to date and seems to be working okay aside from the json blip I mentioned above.

A few bullet points:
  * Uses the JSON api, no web scraping pleb shit.
  * No third party dependencies, just a recent build of GNU Emacs (25+).
  * Full thumbnail support
  * Full resolution media support with the 3rd party programs `feh` and `mpv`
  * `wget` integration to download thread photos
  * Comfy navigation features like quote-hopping, URL scraping, viewing replies, and inline quote expansion.
  * Board, thread, and post crosslink support.
  * Integrates with Evil, Helm, and Ivy when they are available.

### Changes for Monday, 01-09: (more since then, too lazy to update readme aaagggg)
  * Crosslink support: jump to other boards, other threads, and posts inside threads
  * TAB and BACKTAB now bound to button-forward and backward
  * Inline quote expansion: use the curly brace keys to expand/collapse text from quoted posts.
  * Helm, Ivy, and Ido completion for the q4/browse-board function.
  * Internal changes not limited to new rendering for HTML <a> tags, changes to the q4/query function for more flexible callbacks, new text properties.

### Default Keybinds
If you use evil, these are mostly the same except j/k replace n/p, page refreshing is done with R instead of g, and SPC/DEL is RET/DEL, and l to recenter is not bound to anything right now. Check the source for definitive definitions. As is always the case with emacs, these can be changed.

    SPC         q4/point-to-next-post
    DEL         q4/point-to-previous-post
    n           q4/point-to-next-post
    p           q4/point-to-previous-post
    N           scroll-up-line
    P           scroll-down-line
    l           q4/recenter
    q           kill-this-buffer
    ]           q4/quote-hop-backward
    [           q4/pop-mark
    {           q4/unexpand-quotes
    }           q4/expand-quotes
    r           q4/show-replies (early implentation, needs new keybinds)
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

