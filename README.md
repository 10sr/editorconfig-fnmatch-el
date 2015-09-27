fnmatch-el
==========


A zsh-like glob library for Emacs



Usage
------

    (fnmatch-p NAME PATTERN)

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
