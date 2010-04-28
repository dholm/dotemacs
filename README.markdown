dotemacs: My personal Emacs configuration
=========================================

How do I install it?
--------------------

Clone the project from git://github.com/dholm/dotemacs.git and symbolicallylink <i>.emacs</i> and <i>.emacs.d</i> into your <i>$HOME</i>. From the repository path initialize the submodules by executing <i>git submodule init</i> followed by <i>git submodule update</i> which will pull in all the submodules.

After performing the aforementioned steps you should now be able to launch Emacs and enjoy all the packaged configuration goodies.


Which modules are included?
---------------------------

 * [dtrt-indent](http://savannah.nongnu.org/projects/dtrt-indent/), a minor mode which attempts to guess the correct indentation based on the loaded source file.
 * [Magit](http://zagadka.vm.bytemark.co.uk/magit/), Emacs git integration.
 * [mk-project](http://github.com/mattkeller/mk-project), a per-project settings management framework.
