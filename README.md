pydefjump
=========

A fast python module def jump plugin in emacs.


Requirements
============

* python package

	epc

* emacs package

	epc

Install
=======

* git clone git@github.com:justdoit0823/pydefjump.git


How to use
==========

Put the pydefjump.el to your emacs load path and then add following emacs lisp code

into your .emacs file.

	(require 'pydefjump)
	(global-set-key (kbd "C-c d") 'jump-to-def)

Then you can use C-c d in any python module buffer to jump.
