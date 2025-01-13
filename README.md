
# Prolog Server Pages (PSP) and Prolog XML (PLX) 

DISCLAIMER: This is alpha software, which means it is usable and the feature list is 
almost complete but not totally implemented. 
The information provided in this document is provided "AS IS" without warranty of any kind, 
either express or implied, including,  but not limited to, the warranty of noninfringement 
and the implied warranties of merchantability and fitness for a particular purpose. 
Verbatim, copying and redistribution are permitted in all media for any purpose, 
provided all copyright notices are preserved along with related original URLs.

 
## Introduction

### About
This repository contains the latest implementation of Prolog Server Pages (PSP) as well as
the syntax eXtension Prolog XML (PLX). These are **server-side scripting languages based on
Prolog** (for information about the declarative Prolog language, see https://en.wikipedia.org/wiki/Prolog).

The current PSP/PLX implementation is developed on top of SWI Prolog version 9.x 
(https://www.swi-prolog.org/), which is available for Linux, Windows and MacOSX. 
The relevant handlers are incorporated into the SWI Prolog HTTP server infrastructure. 
Therefore, there is native support for all standard HTTP library predicates 
(for details, see the SWI Prolog HTTP server package documentation at 
https://www.swi-prolog.org/pldoc/package/http.html). 

The present brief tutorial describes how to install and get started with PSP/PLX for 
simple web applications. Detailed documentation will be available on the official 
website (https://prologserverpages.com/) in due course, depending on further developments.

### Background and prior work
The first Prolog Server Pages (PSP) implementation was developed by Suciu et al., 2006 
(https://arxiv.org/abs/cs/0603101#). Much like other server-side languages the Prolog code 
was enclosed in special HTML tags, namely `<?` or `<?psp` and `?>`. However, escaping
sequences for intermixing Prolog and HTML is not as simple as for other programming 
languages like PHP or ASP. 

In the original PSP proposal each bracketed block of Prolog code (i.e. a chunk) was assumed 
to be interpreted in *assert-mode*. Such mechanism implies independence of chunks and 
the annoying consequence that variables are not persistent across chunks, which often 
forces the user to duplicate the code and/or use the `assert/[1,2]` and `retract/1` predicate 
families. Subsequent development by Benjamin Johnston (https://www.benjaminjohnston.com.au/index)
led to idea of making PSP to function always in *query-mode* with the introduction of the 
tags `<?,` and `,?>` (reviewed by Wielemaker et al, 2007; see https://arxiv.org/abs/0711.0917). 
The latter approach was then generalized by Mauro DiNuzzo with respect to the operator that 
follow/precede each open/closed PSP chunk, in order to preserve the capability of the 
language to switch between *assert-mode* and *query-mode* (as an illustration, representative 
tags might include `<?;` or `.?>`, among others). Nonetheless, the resulting code turned out 
to be much harder to read and debug. 

A substantial change in Prolog scripting followed Richard O'Keefe’s Prolog 
Well-formed Pages (PWP). Such approach is based on the principle that a source script 
is a well-formed XML document (see https://www.swi-prolog.org/pldoc/man?section=pwp). 

Other alternatives to write HTML pages as Prolog terms include the SWI Prolog 
`html_write` library (see https://www.swi-prolog.org/pldoc/man?section=htmlwrite), which
however makes the underlying HTML hardly recognizable (see below).

### Motivation
The approach underlying PSP/PLX is based on the following complementary principles:
- PSP: each script is primarily a HTML document not a Prolog program
- PLX: each script is a valid Prolog program containing HTML elements 

Let's explore how a server-side Prolog script looks like in the different implementations using
a simple example of a script printing out a welcome message. 
We assume the following file containing the `msg/2` predicate is accessible by the server:

```prolog
% file: msg.pl
%% msg(Message,Style)
msg('Hello, world!','color:#ff0000').
```

First, let's consider the first approach (a script is a HTML page).

Here is the script written using the `pwp` library:

```html
<!-- helloworld.pwp (Prolog Well-formed Pages) -->
<html
    xmlns:pwp="https://www.cs.otago.ac.nz/staffpriv/ok/pwp.pl"
    pwp:ask="ensure_loaded('msg.pl'),msg(Greeting,Style)">
    <body>
        <p pwp:use="Style" pwp:att="$" style="$(Style)$">
            <span pwp:use="Greeting" pwp:tag='-'></span>
        </p>
    </body>
</html>
```

Here is the script written using the `psp` library:

```html
<!-- helloworld.psp (Prolog Server Pages) -->
<html 
    xmlns:psp="https://www.prologserverpages.com/"
    psp:prolog="ensure_loaded('msg.pl'),msg(Greeting,Style)">
    <body>
        <p psp:style="write(Style)" psp:prolog="write(Greeting)"></p>
    </body>
</html>
```

Notably, using PSP it is possible to use a single extra attribute with attribute overloading, 
which results in similar but substantially simpler code compared with PWP.

Next, let's consider the second approach (a script is a Prolog program).

Here is the script written using the `sgml` library:

```prolog
% helloworld.pl (native Prolog using sgml library)
:-
ensure_loaded('msg.pl'),
msg(Greeting,Style), 
html_write([
    element(html, [], [
    element(body, [], [
        element(p, [style=Style], [
        Greeting
        ])
    ])
    ])
], []).
```

Here is the script written using the `plx` library:

```prolog
% helloworld.plx (Prolog XML)
:-
ensure_loaded('msg.pl'),
msg(Greeting,Style), 
<html>,
    <body>,
    <p style=Style>,
        write(Greeting),
    </p>,
    </body>,
</html>. 
```

The latter is achieved by defining few operators that make something like 
`<Tag>(Attributes)`, `<Tag/>(Attributes)` and `</Tag>([])` valid Prolog terms. 
Yet, a pure XML element is not valid Prolog syntax, thus the choice was to have 
a preprocessing step compiling PLX into valid Prolog. 
The main motivation for the development of PLX is to provide proof-of-concept for 
the possibility of having XML elements seamlessly expressed in Prolog code. 
As opposed to the design principle of the Prolog `sgml` library, PLX is tag-based and 
non-hierarchical. This means that a Prolog term can be, for example, separately 
unified with start or end tag. Conversely, the `sgml` library is element-based and is designed to 
represent elements hierarchically, which means that elements are recursively incorporated 
into a single predicate (i.e., `element/3`). Such strategy works fine for well-structured 
documents, but fails for HTML documents due to ambiguous processing of elements that 
commonly miss the end tag (e.g., `<br>` or `<img>`). Moreover, as mentioned above for 
the `html_write` library, `sgml` does not preserve the expressiveness of markup languages. 
The PLX approach eliminates the reliance on the Prolog `sgml` library to process HTML 
documents as Prolog terms. The idea is to have the simplest coding strategy 
that allows for complete and seamless control over HTML elements.

For instance, we could do something like:

```prolog
:-
ensure_loaded('msg.pl'),
msg(Greeting,Style), 
element(p,style,Style,Greeting,Element),
<html>,
    <body>,
        Element,
    </body>,
</html>. 

%% element(+Tag,+Name,+Value,+Message,-Element)
element(Tag,Name,Value,Message,Element) :-
    OpenTag = (<Tag Name=Value>),
    CloseTag = (</Tag>),
    Element = (OpenTag, write(Message), CloseTag).
```

This way we avoid the need of quoting element content or the use of template literals, 
as is done for example in Javascript XML (JSX). 


## Getting started

### Download and installation
PSP is not yet released as a SWI Prolog add-on pack (https://www.swi-prolog.org/pack/list), 
thus to install PSP just download/unpack the archive to your favourite location and load
the `http_psp` and/or `http_plx` modules from the Prolog toplevel: 

```prolog
| ?- use_module('/path/to/psp.pl').
| ?- use_module('/path/to/plx.pl').
```

### Features
The list of PSP/PLX features include: 
- Novel and easy incorporation of markup language in Prolog scripting
- Compilation to valid Prolog with a simple preprocessing step
- Full compliance with SWI Prolog HTTP server infrastructure

In particular, PSP/PLX complies with the requirements of the SWI Prolog HTTP library 
with respect to handler definition (i.e., `http_dispatch` library), according to the 
directive: 

```prolog
:- http_handler(root(.),psp_handler,[prefix]).
```

### Configuration
The configuration file is a Prolog script named `config.pl`. This file is loaded just 
before the execution of each PSP/PLX page. The user has full control over all standard 
Prolog configuration predicates (not described here). However, it is strongly suggested 
not to add/remove clauses to/from the user:message_hook/3 predicate. 
Control of message reporting can be done by setting the `http_reporting` prolog flag, 
which accepts a list of atoms indicating the types of message to be reported. 
For example, the following code will activate reporting of warning and error messages: 

```prolog
:- set_prolog_flag(http_reporting,[warning,error]).
```

The available message types are `banner`, `debug(Topic)`, `error`, `help`, `information`, 
`informational`, `silent`, `trace` and `warning` (as defined by the built-in Prolog 
predicate `print_message/2`). The `http_reporting` flag is initially set to the empty list, 
so nothing will be reported (note that the message term will be always present in the 
log file).  

### How to write PSP/PLX scripts
In order to reconcile Prolog code readability with HTML, the present version of PSP 
implements a new programming scheme in order to incorporate Prolog into HTML documents. 
The approach is reminiscent of Prolog Well-formed Pages (PWP), but contrary to PWP, 
the current PSP implementation provides a unique attribute named `psp:prolog` that is valid 
for *all* HTML tags. 

Notice that before sending output, PSP removes the psp qualification as well as the Prolog 
code from attributes (in the case of `psp:prolog` the entire attribute is removed). 
Therefore, the documents always remains HTML-valid. 

The underlying execution model is as follows. The prolog goal contained in the `psp:prolog` 
attribute enwraps the HTML code contained within the relevant tag and backtracks successively.
In addition to readability, another advantage is the "incentive" to put long pieces of 
Prolog code into separate files, which also facilitates code re-use. 

Let us assume, for example, that a standard Prolog file named `database.pl` contains the 
following code:

```prolog
% file: database.pl
:- dynamic p/1, q/1. 

p(a). 
p(b). 
p(c). 
p(d).

q(c).
```

Now consider the following PSP page:

```html
<!DOCTYPE html>    
<html xmlns:psp="https://www.prologserverpages.com/">
    <body psp:prolog="ensure_loaded('database.pl')">
        <div psp:prolog="p(X)">
            Now X is <b psp:prolog="write(X)"></b>
            <br>
            <span psp:prolog="q(X)">
                Do something when X=c here!<br>
            </span>
        </div>
    </body>
</html>
```
 
Equivalently, using PLX the script would look like the following:

```prolog
:-
ensure_loaded('database.pl'),  
<html xmlns:psp="https://www.prologwebservices.org/psp">,
    <body>, 
        (   p(X),
            <div>,
                write('Now X is '),
                <b>,write(X),</b>,<br>,
                (   q(X)
                ->  write('Do something when X=c here!'),<br>
                ;   true
                ),
            </div>,
            fail
        ;   true 
        ),
    </body>,
</html>.
```

Whether the user prefer writing HTML pages or Prolog programs, in either case the resulting 
code is more compact, elegant and easy to manage than chunk-based code. 
In both cases above, the generated output will be:

```
Now X is a
Now X is b
Now X is c
Do something when X=c here!
Now X is d
```

The above example also illustrated the fact that standard HTML tag attributes are 
untouched by the PSP parser. One limitation of the PSP syntax scheme is that 
sometimes it is necessary to use seemingly redundant HTML tags to incorporate the 
required prolog code, as we did above using `span`. However, as a side-effect this 
requirement helps maintaining appropriate context for the Prolog code. 
Importantly, variables are local to the HTML tag where Prolog code appears.

### HTML tag attributes
Attributes can be substituted into by qualifying the attribute with the `psp` namespace. 
The psp-qualified attributes can be interpreted as overloaded versions of the corresponding 
html-qualified ones. In fact, the former are always server-side processed and returned 
to the client as the latter.
For example, filling a drop-down menu requires operating on the value attribute of the 
option HTML tag, which is achieved by:

```html
<!DOCTYPE html>    
<html xmlns:psp="https://www.prologserverpages.com/">
    <select name="PrologFlag" psp:prolog="
        current_prolog_flag(Name,Value),
        Selected = dialect
    ">
        <option 
            psp:selected="Name==Selected,write('selected')"
            psp:value="write(Name)"
            psp:prolog="write(Value)">
        </option>
    </select>
</html>
```
 
Note that a psp-qualified HTML tag attribute appears in the final HTML page only if the 
code contained therein succeeds. In the example above, only the option element with 
value="dialect" will be accompanied by a selected="selected" attribute.

NOTE: The SWI Prolog SGML library appears not to properly process the Boolean 
HTML attributes (i.e., attributes without a value), so it is recommended to always set 
a value for a boolean attribute (specifications suggest to use as value the name of 
the attribute itself).

The different approach underlying PLX scripting is well illustrated by rewriting the above example:

```prolog
:- 
<html>,
    <select name="PrologFlag">,
        (   current_prolog_flag(Name,Value),
            selected(Name,Selected),
            <option value=Name Selected>,
                write(Value),
            </option>,
            fail
        ;   true 
        ),
    </select>,
</html>.

selected(dialect,selected="selected") :- !.
selected(_,_). 
```

### Working with HTTP headers
In order to perform actions before HTTP headers are sent, PSP requires that Prolog 
code is contained in the `<html>` tag. Below is an example to implement server-side 
redirection:

```html
<!-- index.psp -->
<!DOCTYPE html>    
<html xmlns:psp="https://www.prologserverpages.com/" 
    psp:prolog="
        http_current_request(Request), 
        http_redirect(moved,'https://www.github.com/',Request)
    ">
</html>
```

Note that having a psp:prolog attribute in the `<html>` tag is also the only way to 
have variables that are global to the entire page. The SWI Prolog HTTP library 
provides several predicates that can be used to inspect and manipulate HTTP headers. 

Of course, for PLX there is no such requirement and it is sufficient headers are handled
before any HTML tag, as follows:

```prolog
% index.plx
:-
http_current_request(Request), 
http_redirect(moved,'https://www.github.com/',Request).  
```

## Concluding remarks 

We used SWI Prolog for the implementation of PSP/PLX because it is free-software 
and comes with a relatively large number of 
libraries providing interfaces to popular applications such as ODBC, SQL, Semantic Web, 
SGML/XML and RDF. Users can also benefit of several language-specific features like, 
for example, Constraint Logic Programming (CLP). It should be realized, however, 
that the best way to support spreading of Prolog language is through applications, 
notably web-based ones. Useful applications include, but are not limited to, 
content-management systems and educational web resources.

PSP/PLX is developed with the belief that the web represents an opportunity 
*"to revive and rebrand Prolog"* (Torbjörn Lager). 
 
For information, feedback or suggestions please send an email to info at prologserverpages.com. 
User requests of general interest can also be posted on the SWI Prolog forum (https://swi-prolog.discourse.group/). 
 
This work is licensed under a Creative Commons BY-SA license (https://creativecommons.org/licenses/by-sa/4.0/)

