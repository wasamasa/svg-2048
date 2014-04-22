svg-2048
=========

About
-----

A port of the titular HTML5 game utilizing Emacs' SVG
capabilities.  Requires a graphical Emacs instance to run.

Installation
------------

Install via `quelpa <https://github.com/quelpa/quelpa>`_ with ``M-:
(quelpa '(svg-2048 :fetcher github :url
"http://github.com/wasamasa/svg-2048"))`.

Usage
-----

Run ``M-x svg-2048`` for a new game.  Asides from the original
movement keys there's ``g`` for forced redrawing (useful when the
window is resized) and ``n`` to start a new game.  Hit ``q`` to bury
the buffer.

========================= ================================
Key bind                  Function
========================= ================================
``w``, ``k``, ``<up>``    Move tiles up
``a``, ``h``, ``<left>``  Move tiles left
``s``, ``j``, ``<down>``  Move tiles down
``d``, ``l``, ``<right>`` Move tiles right
``g``                     Redraw board
``n``                     New game
``q``                     Bury buffer
========================= ================================

Contributing
------------

If you find bugs, have suggestions or any other problems, feel free to
report an issue on the issue tracker or hit me up on IRC, I'm always on
``#emacs``.  Patches are welcome, too, just fork, work on a separate
branch and open a pull request with it.

Alternatives
------------

`There's <https://github.com/sprang/emacs-2048>`_ `four
<https://github.com/samfiechter/2048-mode>`_ `of
<http://pastebin.com/ZhdB020g>`_ `them
<https://bitbucket.org/zck/2048.el>`_.
They're all text though.
