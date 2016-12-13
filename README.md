# maxima-read-wxmx
Functions to enable Maxima to parse and load wxmx (wxMaxima) files

wxMaxima stores Maxima code and other files (mostly images)
in a zip archive. This package contains functions to extract
the Maxima code and load it into Maxima.

maxima-read-wxmx uses Quicklisp to load several libraries,
so you will need to have Quicklisp installed.

Functions:

 * get\_wxmx\_content\_xml (file\_name\_wxmx): returns the contents of context.xml from the named wxmx file as a string
 * read\_content\_xml (file\_name\_wxmx): returns the Maxima code from the named wxmx file as a string

Incidental functions:

 * open\_zipfile (file\_name\_zip): returns a zipfile object for the file named file\_name\_zip
 * get\_zipfile\_entry (zipfile, entry\_name): returns the zipfile entry named by entry\_name from zipfile
 * zipfile\_entry\_contents (zipfile\_entry): returns the contents of the zipfile entry as a string

 * read\_xml (file\_name\_xml): returns the XML expression as a Maxima expression
 * extract\_input (expr): returns list of strings from expr which is an expression returned by read\_xml
 * join\_lines (l): concatenate a list of strings, separating with newlines
 * load\_stream (in): reads Maxima input from stream in until the end of the stream
