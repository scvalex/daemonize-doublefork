daemonize-doublefork
====================

> Start background Haskell daemons by double-forking

Deprecated
----------

This package has been deprecated in favour of
[daemons](http://hackage.haskell.org/package/daemons).

What
----

This module provides `startDaemon` and `stopDaemon` to facilitate the
creation of Haskell daemon programs.

The problem is as follows: the user starts a program in their
terminal, but he wants the program to relinquish control of the
terminal immediately, and furthermore, the program (or part of it)
should keep running even after said terminal is closed.  Examples of
programs that behave like this are `nginx` and `emacs --daemon`.

The correct solution is to double-fork a process.  This ensures
that the child process is completed separated from the terminal it
was started on.

See this [page](http://www.enderunix.org/docs/eng/daemon.php) for a
discussion on double-forking.

See the
[Hackage page](http://hackage.haskell.org/package/daemonize-doublefork)
for documentation and examples.

Install
-------

This package is on
[Hackage](http://hackage.haskell.org/package/daemonize-doublefork).
To install it, run:

    cabal update
    cabal install daemonize-doublefork
