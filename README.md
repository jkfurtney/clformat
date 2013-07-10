# clformat

A port of the Common Lisp FORMAT function to Python.

# Installation

  $ python setup.py install

# Usage

```python
>>> from clformat import clformat

>>> clformat("There is a ~a in my string.", "foo")
There is a foo in my string.

>>> clformat("~r cat~:p", 10)
ten cats

>>> clformat("~{~a~^-~}", range(8))
0-1-2-3-4-5-6-7

>>> import time
>>> clformat("The year is ~@r", time.localtime().tm_year)
The year is MMXIII
```

You can check the version like so:
```python
>>> import clformat
>>> clformat.__VERSION__
0.0.1a1
```

# Tests
Tests are in the doctest format. Some tests are generated
automatically by evaluating format expressions in a common lisp
environment; see clformat/tests/format-test.lisp

  $ python -c "import clformat; clformat.test()"

# Status

Usable, most directive are mostly working. The floating point
directives are not fully implemented; the tabular environment
(~< ~>) is not currently implemented. Error reporting could be
improved.

## Format documentation

http://www.lispworks.com/documentation/HyperSpec/Body/22_c.htm

http://www.gigamonkeys.com/book/a-few-format-recipes.html

http://psg.com/~dlamkins/sl/chapter24.html

See also: https://pypi.python.org/pypi/fortranformat

## Submitting Patches, Feature Requests, Etc.

Contributions are welcome! Feel free to submit any patches via a pull
request on Github, or by creating an issue on this repository.

## License

This program is freely available for anyone to use under an MIT license.
Please consult the MIT-LICENSE file.
