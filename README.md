[![Build Status](https://travis-ci.org/10sr/editorconfig-fnmatch-el.svg)](https://travis-ci.org/10sr/editorconfig-fnmatch-el)
[![MELPA](http://melpa.org/packages/editorconfig-fnmatch-badge.svg)](http://melpa.org/#/editorconfig-fnmatch)
[![MELPA Stable](http://stable.melpa.org/packages/editorconfig-fnmatch-badge.svg)](http://stable.melpa.org/#/editorconfig-fnmatch)


editorconfig-fnmatch-el
==========


editorconfig-fnmatch.el provides a fnmatch implementation with a few
extensions.
The main usage of this library is glob pattern matching for
[EditorConfig](http://editorconfig.org), but it can also act solely.



Usage
------

Eval

    (editorconfig-fnmatch-p NAME PATTERN)

to test if NAME match PATTERN, e.g.

    (editorconfig-fnmatch-p "a.js" "*.js")       ; => t
    (editorconfig-fnmatch-p "/dir/a.js" "**.js") ; => t
    (editorconfig-fnmatch-p "d.js" "[abc].js")   ; => nil

PATTERN should be a shell glob pattern, and some zsh-like wildcard matchings can
be used:

|chars        |desc
|-------------|-------------------------------------------------------------
|`*`          |Matches any string of characters, except path separators (/)
|`**`         |Matches any string of characters
|`?`          |Matches any single character
|`[name]`     |Matches any single character in name
|`[^name]`    |Matches any single character not in name
|`{s1,s2,s3}` |Matches any of the strings given (separated by commas)
|`{min..max}` |Matches any number between min and max


License
-------

This software is licensed under GPLv2.
See `LICENSE` for details.


Aknowledgement
--------------

This library is a port from
[editorconfig-core-py] (https://github.com/editorconfig/editorconfig-core-py/blob/master/editorconfig/fnmatch.py)
library.
