[![Build Status](https://travis-ci.org/10sr/editorconfig-fnmatch-el.svg)](https://travis-ci.org/10sr/editorconfig-fnmatch-el)


editorconfig-fnmatch-el
==========


editorconfig-fnmatch.el provides a fnmatch implementation with a few
extensions.
The main usage of this library is matching files for EditorConfig, but it can
also act solely.



Usage
------

    (editorconfig-fnmatch-p NAME PATTERN)

To test if NAME match PATTERN.



Zsh-like wildcard matching can be used in PATTERN:


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
