pydefjump
=========

A fast python module def jump plugin in emacs.


Requirements
============

* python package

	epc(>=0.0.5)

* emacs package

	epc(>=0.1.1)


Install
=======

* install required python packages

	$ pip install epc


* install this emacs plugin

	you can clone this repository to your emacs directory path, like following:

	$ cd ~/.emacs.d

	$ git clone git@github.com:justdoit0823/pydefjump.git


How to use
==========

Put the pydefjump.el to your emacs load path and then add following emacs lisp code

into your .emacs file.

	(add-to-list 'load-path "~/.emacs.d/pydefjump")
	(require 'pydefjump)
	(add-hook 'python-mode-hook 'jump-python)

Then you can use C-c d in any python module buffer to jump.
