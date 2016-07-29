pydefjump
=========

A fast python function and class definition locating plugin in emacs.


Requirements
============


Python package
--------------

* epc(>=0.0.5)



Emacs package
-------------

* epc(>=0.1.1)



Install
=======


* install required python packages


		$ pip install epc


* install required emacs packages


		M-x el-get install [Ret] epc


* install this plugin

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


Switch python version
---------------------

	run command M-x jump-python-switch


Refresh definition position
---------------------------

	press key C-c r
