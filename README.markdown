dotemacs: My personal Emacs configuration
=========================================

How do I install it?
--------------------

Clone the project from git://github.com/dholm/dotemacs.git and symbolically link
the cloned directory *dotemacs* to *$HOME/.emacs.d*. Alternatively you
can just clone it right into *.emacs.d*. From the repository path
initialize the submodules by executing *git submodule init* followed by
*git submodule update* which will pull in all the submodules. If you are
not using GNU Emacs check out the EmacsWiki page on
[InitFile](http://www.emacswiki.org/emacs/InitFile) for instructions on how to
help your Emacs implementation locate *init.el*.

CEDET needs to be compiled before execution so change into *vendor/cedet* and
run *make*.

After performing the aforementioned steps you should now be able to launch Emacs
and enjoy all the packaged configuration goodies.

To keep the submodules up-to-date remember to occasionally run *git submodule
foreach git pull* and recompile packages that were updated and require it,
such as *vendor/cedet*.


Configured key-bindings
-----------------------
 * (C-h b) Describe all key bindings, this is a standard binding that is highly
   useful.

 * (F3) Toggle the dedicated MultiTerm window
 * (F6) Show line numbers
 * (F7) Compile
 * (F8) Execute GDB (gud-mode)
 * (C-x C-m) Alias to M-x which is easier to reach
 * (C-c C-m) Another alias to M-x
 * (C-w) Kill word backwards
   Overrides the normal kill-region in an attempt to emulate the functionality
   of your standard terminal.
 * (C-x C-k) Kill region
 * (C-c .) Jump to the tag referred to at the cursor
 * (C-c d) Display the code-level documentation for the symbol at the cursor
 * (C-c D) Display all known parts for the datatype at the cursor
 * (C-c c) Smart code completion
 * (C-c t) Switch between the header and the implementation (for languages where
   such a concept exists)
 * (C-c C-w) Spell check the current word
 * (C-x C-f) Lusty file explorer
 * (C-x b) Lusty buffer explorer

 * (M-j) Move one tab left in TabBar
 * (M-k) Move one tab right in TabBar


New functions
-------------
 * *close-all-buffers* Will close all currently open buffers

 
Which external modules are included?
------------------------------------

 * [auto-complete](http://cx4a.org/software/auto-complete/), Auto Complete Mode
   renews an old completion interface and provides an environment that helps
   users concentrate on their own work.
 * [dtrt-indent](http://savannah.nongnu.org/projects/dtrt-indent/), a minor mode
   which attempts to guess the correct indentation based on the loaded source
   file.
 * [Magit](http://zagadka.vm.bytemark.co.uk/magit/), Emacs git integration.
 * [CEDET](http://cedet.sourceforge.net/), Collection of Emacs Development
   Environment Tools.
 * [YASnippet](http://yasnippet.googlecode.com/), Yet Another Snippet extension.
 * [tcom Allows Emacs as Editor for MS Outlook](http://wiki.tcl.tk/9198), by
   utilizing Tcl and its tcom extension it is possible to roundtrip Outlook
   e-mails using Emacs.
 * [lusty-explorer](http://www.emacsmirror.org/package/lusty-explorer.html),
   provides an excellent little file browser buffer for quickly locating files.
 * [XCscope](http://cscope.sourceforge.net/), Darryl Okahatas xcscope.el comes
   with the cscope package.
 * [undo-tree](http://www.dr-qubit.org/emacs.php), visualize and navigate
   Emacs' undo history in a tree.
 * [MultiTerm](http://www.emacswiki.org/MultiTerm), an improved Emacs terminal
   that handles multiple terminal buffers very well.
 * [Zenburn](http://github.com/bbatsov/zenburn-emacs), a direct port of the
   low-contrast Zenburn theme for vim.
 * [TabBarMode](http://www.emacswiki.org/emacs/TabBarMode), provides a minor
   mode which will display open buffers as a tab bar at the top.
 * [SmartTab](http://github.com/genehack/smart-tab), smart tab completion for
   Emacs.
