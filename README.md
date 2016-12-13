# maxima-read-wxmx

SUMMARY
-------

Functions to enable Maxima to parse and load wxmx (wxMaxima) files

wxMaxima stores Maxima code and other files (mostly images)
in a zip archive. This package contains functions to extract
the Maxima code and load it into Maxima.

maxima-read-wxmx uses Quicklisp to load several libraries,
so you will need to have Quicklisp installed.

This package is a work in progress; any or all of these functions are subject to revision.


FUNCTIONS
---------

Intended user interface:

 * load\_wxmx (file\_name\_wxmx): reads Maxima code from wxmx and interprets it with Maxima interpreter
 * parse\_content\_xml (file\_name\_wxmx): returns a list of Maxima expressions parsed (but not evaluated) from wxmx

Incidental functions:

 * open\_zipfile (file\_name\_zip): returns a zipfile object for the file named file\_name\_zip
 * get\_zipfile\_entry (zipfile, entry\_name): returns the zipfile entry named by entry\_name from zipfile
 * zipfile\_entry\_contents (zipfile\_entry): returns the contents of the zipfile entry as a string

 * get\_wxmx\_content\_xml (file\_name\_wxmx): returns the contents of context.xml from the named wxmx file as a string
 * read\_content\_xml (file\_name\_wxmx): returns the Maxima code from the named wxmx file as a string
 * read\_xml (file\_name\_xml): returns the XML expression as a Maxima expression
 * extract\_input (expr): returns list of strings from expr which is an expression returned by read\_xml
 * join\_lines (l): concatenate a list of strings, separating with newlines
 * load\_stream (in): reads Maxima input from stream in until the end of the stream


LIMITATIONS
-----------

maxima-read-wxmx can't handle UTF-8 in the Maxima input (e.g. variables names).
(UTF-8 characters anywhere else in the wxmx are just ignored.)


EXAMPLE
-------

I am working mostly with Clisp. I've tried other Lisps (SBCL, Clozure CL)
once or twice, but after running into trouble with ASDF and/or Quicklisp
I've gone back to Clisp, which has its own idiosyncrasies.

I have Clisp configured to load Quicklisp when it is launched.
Maxima built with Clisp also loads Quicklisp when it is launched.

The maxima-read-wxmx.asd says that maxima-read-wxmx depends on some 
packages that Quicklisp can load. However the first time I try

```
:lisp (asdf:oos 'asdf:load-source-op "maxima-read-wxmx")
```

in a new session, I always get an error when it tries toad XMLS.
I've found that if I just issue the same command again, it succeeds.
Dunno what's up with that.

With maxima-read-wxmx loaded, it should be possible to read wxmx files.
Let's say the file is `foo1.wxmx` which is included in this package.

Just print the content.xml which is contained in the wxmx:
```
print (get_wxmx_content_xml (my_wxmx)) $
```

Read the Maxima input which is contained in content.xml:
```
print (read_content_xml (my_wxmx)) $
```
(The name of that function doesn't suggest its purpose very well;
sorry about that.)

Read the input and parse it into Maxima expressions:
```
grind (parse_content_xml (my_wxmx)) $
```

Read the input, parse it, and feed it into the Maxima interpreter:
```
load_wxmx (my_wxmx);
```

Here's my session when I try all of the stuff above. As I was saying
before, I'm working with Maxima compiled with Clisp and I have Quicklisp
loaded at program launch by clisprc.lisp.

I've already issued `:lisp (asdf:oos ...)` (twice, once for the error and
once for good) which produces voluminous status messages. I wonder how to
quiet that.

```
(%i1) my_wxmx : "foo1.wxmx" $
(%i2) print (get_wxmx_content_xml (my_wxmx)) $
<?xml version="1.0" encoding="UTF-8"?>
<!--   Created by wxMaxima 13.04.2   -->
<!--http://wxmaxima.sourceforge.net-->
<wxMaximaDocument version="1.1" zoom="100">
<cell type="text">
<editor type="text">
<line>Here is some text which opens the discussion. I&apos;ll find out stuff l\
ater.</line>
</editor>
</cell>
<cell type="text">
<editor type="text">
<line>More text here. I wonder if it makes a separate paragraph.</line>
</editor>
</cell>
<cell type="text">
<editor type="text">
<line>First formula. Something about random blurfage.</line>
</editor>
</cell>
<cell type="code">
<input>
<editor type="input">
<line>eqn : x^2 + 17*x + 29 = 0;</line>
</editor>
</input>
<output>
<mth><lbl>(%o1) </lbl><e><r><v>x</v></r><r><n>2</n></r></e><v>+</v><n>17</n><h\
>*</h><v>x</v><v>+</v><n>29</n><v>=</v><n>0</n></mth>
</output>
</cell>
<cell type="code">
<input>
<editor type="input">
<line>solve (eqn, x);</line>
</editor>
</input>
<output>
<mth><lbl>(%o2) </lbl><t>[</t><v>x</v><v>=</v><v>â</v><f><r><q><n>173</n></q\
><v>+</v><n>17</n></r><r><n>2</n></r></f><t>,</t><v>x</v><v>=</v><f><r><q><n>1\
73</n></q><v>â</v><n>17</n></r><r><n>2</n></r></f><t>]</t></mth>
</output>
</cell>
<cell type="code">
<input>
<editor type="input">
<line>subst (first (%), eqn);</line>
</editor>
</input>
<output>
<mth><lbl>(%o3) </lbl><f><r><e><r><p><q><n>173</n></q><v>+</v><n>17</n></p></r\
><r><n>2</n></r></e></r><r><n>4</n></r></f><v>â</v><f><r><n>17</n><h>*</h><p\
><q><n>173</n></q><v>+</v><n>17</n></p></r><r><n>2</n></r></f><v>+</v><n>29</n\
><v>=</v><n>0</n></mth>
</output>
</cell>
<cell type="code">
<input>
<editor type="input">
<line>expand (%);</line>
</editor>
</input>
<output>
<mth><lbl>(%o4) </lbl><n>0</n><v>=</v><n>0</n></mth>
</output>
</cell>
<cell type="text">
<editor type="text">
<line>More text here. I wonder if percent sign stuff works.</line>
</editor>
</cell>
<cell type="code">
<input>
<editor type="input">
<line>abc : &apos;(def + xyz);</line>
</editor>
</input>
<output>
<mth><lbl>(%o5) </lbl><v>xyz</v><v>+</v><v>def</v></mth>
</output>
</cell>
<cell type="code">
<input>
<editor type="input">
<line>bcd : &apos;(sin(x)+sin(x))$</line>
</editor>
</input>
</cell>
<cell type="text">
<editor type="text">
<line>Dollar sign terminator on previous line. Hmm.</line>
</editor>
</cell>
<cell type="code">
<input>
<editor type="input">
<line>ev (bcd);</line>
</editor>
</input>
<output>
<mth><lbl>(%o7) </lbl><n>2</n><h>*</h><fn><fnm>sin</fnm><p><v>x</v></p></fn></\
mth>
</output>
</cell>
<cell type="text">
<editor type="text">
<line>Now bcd is simplified.</line>
</editor>
</cell>
<cell type="code">
<input>
<editor type="input">
<line>print(bcd);</line>
</editor>
</input>
<output>
<mth><n>2</n><h>*</h><fn><fnm>sin</fnm><p><v>x</v></p></fn><v></v></mth>
<mth><lbl>(%o8) </lbl><n>2</n><h>*</h><fn><fnm>sin</fnm><p><v>x</v></p></fn></\
mth>
</output>
</cell>
<cell type="text">
<editor type="text">
<line>bcd was simplified by printing too.</line>
</editor>
</cell>
<cell type="text">
<editor type="text">
<line>All finished.</line>
</editor>
</cell>
<cell type="text">
<editor type="text">
<line>No, really!</line>
</editor>
</cell>
</wxMaximaDocument> 
(%i3) print (read_content_xml (my_wxmx)) $
eqn : x^2 + 17*x + 29 = 0;
solve (eqn, x);
subst (first (%), eqn);
expand (%);
abc : '(def + xyz);
bcd : '(sin(x)+sin(x))$
ev (bcd);
print(bcd);
 
(%i4) grind (parse_content_xml (my_wxmx)) $
[eqn:x^2+17*x+29 = 0,solve(eqn,x),subst(first(%),eqn),expand(%),
 abc:'(xyz+def),bcd:'(2*sin(x)),ev(bcd),print(bcd)]$
(%i5) load_wxmx (my_wxmx);
(%i6)                                2
(%o6)                         x  + 17 x + 29 = 0
(%i7)                          sqrt(173) + 17      sqrt(173) - 17
(%o7)             [x = - --------------, x = --------------]
                               2                   2
(%i8)                                2
               (sqrt(173) + 17)    17 (sqrt(173) + 17)
(%o8)          ----------------- - ------------------- + 29 = 0
                       4                    2
(%i9) (%o9)                                0 = 0
(%i10) (%o10)                             xyz + def
(%i11) 
(%i12) (%o12)                             2 sin(x)
(%i13) 2 sin(x) 
(%o13)                             2 sin(x)
(%i14) (%o14)                               done
(%i15) 
```

