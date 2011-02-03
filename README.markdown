dotemacs: My personal Emacs configuration
=========================================

How do I install it?
--------------------

Clone the project from git://github.com/dholm/dotemacs.git and symbolically link the cloned directory <i>dotemacs</i> to <i>$HOME/.emacs.d</i>. Alternatively you can just clone it right into <i>.emacs.d</i>. From the repository path initialize the submodules by executing <i>git submodule init</i> followed by <i>git submodule update</i> which will pull in all the submodules.

CEDET needs to be compiled before execution so change into <i>dotemacs/cedet</i> and run <i>make</i>.

After performing the aforementioned steps you should now be able to launch Emacs and enjoy all the packaged configuration goodies.


Which modules are included?
---------------------------

 * [browse-kill-ring](http://www.todesschaf.org/projects/bkr), makes it possible to browse through your kill ring
 * [dtrt-indent](http://savannah.nongnu.org/projects/dtrt-indent/), a minor mode which attempts to guess the correct indentation based on the loaded source file.
 * [Magit](http://zagadka.vm.bytemark.co.uk/magit/), Emacs git integration.
 * [CEDET](http://cedet.sourceforge.net/), Collection of Emacs Development Environment Tools.
