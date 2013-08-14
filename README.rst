===========================================
 dotemacs: My personal Emacs configuration
===========================================

How do I install it?
====================

Clone the project from ``git://github.com/dholm/dotemacs.git`` and symbolically
link the directory ``.emacs.d`` to ``$HOME/.emacs.d``.

To make all the features work properly in Python mode run ``pip install -r
requirements.txt``.


Bindings
========

With the exception of core Emacs functionality most bindings are made to a set
of predefined maps that in turn are bound to various prefixes by default. That
way bindings become more logical and easier to memorize.

Core bindings
-------------

* execute-extended-command ``[C-x C-m] [C-x m]``
* backward-kill-word ``[C-w]``
* kill-region ``[C-x C-k]``

Maps
----

* user/navigation-map ``[C-c n]``
  Navigation in Emacs buffers and modes.

* user/code-map ``[C-c c]``
  Interaction with the contents of Emacs buffers, such as modifying a region or
  completing code.

* user/documentation-map ``[C-c d]``
  Access to documentation.

* user/vcs-map ``[C-c v]``
  Interaction with version control systems.

* user/utilities-map ``[C-c u]``
  Interaction with Emacs utilities.


.. _InitFile: http://www.emacswiki.org/emacs/InitFile
