% http_psp_plx.pl

:- module(http_psp_plx, [
        psp_plx_handler/2,
        reply_psp_plx_page/3,
        http_format/2,
        op(900,fx,(<)),
        op(900,fx,(</)),
        op(700,xfx,(/>)),
        (<)/1,
        (</)/1
    ]).

:- use_module(library(http/http_session)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_wrapper)).
:- use_module(library(http/http_stream)).
:- use_module(library(http/http_header)).
:- use_module(library(http/http_server_files)).
:- use_module(library(http/http_files)).
:- use_module(library(pengines)).
:- use_module(pengine_sandbox:library(pengines)).
:- use_module(library(http/http_error)).
:- use_module(library(http/mimetype)).
:- use_module(library(readutil)).
:- use_module(library(sgml)).
:- use_module(library(sgml_write)).
:- use_module(library(predicate_options)).
:- use_module(library(filesex)).


:- dynamic mime:mime_extension/2.
:- multifile mime:mime_extension/2.

mime:mime_extension(psp,text/psp).
mime:mime_extension(plx,text/plx).


:- dynamic user:prolog_load_file/2.
:- multifile user:prolog_load_file/2.

user:prolog_load_file(Spec,Options) :- 
    strip_module(Spec,_,Plain),
    absolute_file_name(Plain,File),
    member(MimeType,[text/psp,text/plx]),
    file_mime_type(File,MimeType), 
    !,
    reply_psp_plx_page(Spec,Options,_).


%% reply_psp_plx_page/3
% reply_psp_plx_page(:File,+Options,+Request)

:- predicate_options(reply_psp_plx_page/3,2,
    [   dtd(any),
        path_module(boolean),
        unsafe(boolean)
    ]).

:- meta_predicate reply_psp_plx_page(:,+,+).

reply_psp_plx_page(Module:Spec,Options,_Request) :-
    http_safe_file(Spec,Options),
    absolute_file_name(Spec,File,[access(read)]),
    file_directory_name(File,Path),
    (   option(path_module(true),Options)
    ->  Context = Path
    ;   Context = Module
    ),
    file_name_extension(Base,_,File),         
    file_name_extension(Base,pl,PrologFile),
    access_file(PrologFile,write), 
    !,
    (   exists_file(PrologFile),
        time_file(PrologFile,Preprocessed),
        time_file(File,LastModified),
        LastModified < Preprocessed
    ->  true % cached
    ;   process_file(File,Options,Output),
        open(PrologFile,write,Stream,[]),
        format(Stream,'~s',[Output]),
        close(Stream)
    ),
    Context:load_files(PrologFile,[]).

process_file(File,_Options,Output) :-
    file_mime_type(File,text/plx),
    !,
    read_file_to_codes(File,Codes,[]),
    phrase(plx(Output),Codes).
process_file(File,Options,Output) :-
    file_mime_type(File,text/psp),
    !,
    option(dtd(DTD),Options,_), 
    load_html(File,DOM, 
        [   case_sensitive_attributes(false), 
            case_preserving_attributes(true), 
            dtd(DTD)
        ]),    
    psp_parse(DOM,Output).


%% psp_handler/2
% psp_handler(+Options,+Request)

:- predicate_options(psp_plx_handler/2,1, 
    [   path_alias(atom), 
        index_files(list(atom)),
        index_hook(callable),
        hide_extensions(list(atom))
    ]).

psp_plx_handler(Options,Request) :-   
    memberchk(path(Path),Request),
    option(path_alias(Alias),Options,app_data),
    Location =.. [Alias,Path],
    reply(Location,Options,Request).

reply(Location,Options,Request) :-
    % Serve .psp .plx and .pl files
    absolute_file_name(Location,File),
    exists_file(File),   
    member(MimeType,[text/psp,text/plx]),
    file_mime_type(File,MimeType), 
    !,
    reply_psp_plx_page(user:Location,Options,Request).
reply(Location,Options,Request) :-
    % Serve index files
    absolute_file_name(Location,Directory),
    exists_directory(Directory), 
    option(index_files(Indexes),Options),
    member(Index,Indexes),
    directory_file_path(Directory,Index,File),
    exists_file(File),   
    !,
    memberchk(path(Path),Request),
    directory_file_path(Path,Index,To),
    http_redirect(moved,To,Request).
reply(Location,Options,Request) :-
    % Serve directory using hook (if provided)
    absolute_file_name(Location,Directory),
    exists_directory(Directory),   
    option(index_hook(Hook),Options), 
    !, 
    call(Hook,Location,Options,Request).
reply(Location,_Options,Request) :-
    % Reply forbidden (directory)
    absolute_file_name(Location,Directory),
    exists_directory(Directory),
    !,
    memberchk(path(Path),Request),
    permission_error(read,http_location,Path).    
reply(Location,Options,Request) :-
    % Reply forbidden (hidden files)
    absolute_file_name(Location,File),
    exists_file(File),
    option(hide_extensions(Extensions),Options),
    member(Extension,Extensions),
    file_name_extension(_,Extension,File), 
    !,
    memberchk(path(Path),Request),
    permission_error(read,http_location,Path).
reply(Location,_,Request) :-  
    % Reply all other files
    absolute_file_name(Location,File),
    exists_file(File), 
    !,  
    http_reply_file(File,[unsafe(true)],Request).
reply(_,_,Request) :-
    % Reply not found
    http_404([],Request).
    

% PLX tag rendering predicates.

<(>(Element,[])) :- 
    !, 
    must_be(atom,Element), 
    http_format('<~w>',[Element]).
<(>(Element,Attributes)) :-
    must_be(atom,Element),
    must_be(list(namevalue(atom,any)),Attributes),    
    http_format('<~w',[Element]),
    (   select(Name=Value,Attributes,_),
        (   Value = '' 
        ->  http_format(' ~w',[Name]) 
        ;   http_format(' ~w="~w"',[Name,Value])
        ),
        fail 
    ;   true
    ),
    http_format('>',[]).

<(/>(Element,[])) :- 
    !, 
    must_be(atom,Element), 
    http_format('<~w/>',[Element]).
<(/>(Element, Attributes)) :-
    must_be(atom,Element),
    must_be(list(namevalue(atom,any)),Attributes),
    http_format('<~w',[Element]),
    (   select(Name=Value,Attributes,_),
        (   Value = '' 
        ->  http_format(' ~w',[Name]) 
        ;   http_format(' ~w="~w"',[Name,Value])
        ),
        fail 
    ;   true
    ),
    http_format('/>',[]).

</(>(Element, [])) :- 
    must_be(atom,Element), 
    http_format('</~w>',[Element]).

http_format(Fmt,Args) :-
    (   is_cgi_stream(current_output),        
        cgi_property(current_output,state(header)) 
    ->  format('Content-type: text/html; charset=utf-8\r\n\r\n<!DOCTYPE html>\r\n\r\n',[])
    ;   true
    ),
    format(Fmt,Args).

:- multifile error:has_type/2.
error:has_type(namevalue(NameType,ValueType),Namevalue) :- 
    namevalue(Name,Value,Namevalue), 
    must_be(NameType,Name),
    must_be(ValueType,Value).

namevalue(Name,Value,Name=Value).


% PLX Parser

:- set_prolog_flag(double_quotes,codes).

plx(Atom) --> 
    script(Codes), 
    {   atom_codes(Atom,Codes)
    }.

script(Codes) --> 
    string(Atom), 
    script(Script),
    !, 
    {   atom_codes(Atom,String), 
        append([String,Script],Codes) 
    }.
script(Codes) --> 
    tag(Tag), 
    script(Script), 
    !, 
    {   append([Tag,Script],Codes) 
    }.
script([Code|Codes]) --> 
    [Code], 
    script(Codes), 
    !.
script([]) --> [].

tag(Codes) --> 
    tag_(Tag,Attributes,Open,Close), 
    !, 
    {   with_output_to(
            codes(Codes), 
            format('(~w~w~w(~w))',[Open,Tag,Close,Attributes])
        ) 
    }.

tag_(Tag,[],'</','>') --> 
    "</", 
    term(Tag), 
    spaces_or_blank(_), 
    ">", 
    !.
tag_(Tag,[],'<','>') --> 
    "<", 
    term(Tag), 
    spaces_or_blank(_), 
    ">", 
    !.
tag_(Tag,Attributes,'<','>') --> 
    "<", 
    term(Tag), 
    attributes(Attributes), 
    spaces_or_blank(_), 
    ">", 
    !.
tag_(Tag,[],'<','/>') --> 
    "<", 
    term(Tag), 
    spaces_or_blank(_), 
    "/>", 
    !.
tag_(Tag,Attributes,'<','/>') --> 
    "<", 
    term(Tag), 
    attributes(Attributes), 
    spaces_or_blank(_), 
    "/>".

spaces_or_blank([Code|Codes]) --> 
    [Code], 
    {   code_type(Code,space) 
    }, 
    spaces_or_blank(Codes), 
    !.
spaces_or_blank([]) --> [].

spaces([Code|Codes]) --> 
    [Code], 
    {   code_type(Code,space) 
    }, 
    spaces(Codes), 
    !.
spaces(" ") --> " ".

attributes([Name=Value|Attributes]) --> 
    spaces(_), 
    term(Name), 
    spaces_or_blank(_), 
    "=", 
    spaces_or_blank(_), 
    term(Value), 
    attributes(Attributes), 
    !.
attributes([Name='\'\''|Attributes]) --> 
    spaces(_), 
    term(Name), 
    attributes(Attributes), 
    !.
attributes([]) --> []. 

term(String) --> 
    string(String), 
    !.
term(Atom) --> 
    atom(Atom), 
    !.
term(Variable) --> 
    variable(Variable), 
    !.

string(String) --> 
    [39], 
    string_39(Codes), 
    [39], 
    !, 
    {   atom_codes_q(String,Codes) 
    }.
string(String) --> 
    [34], 
    string_34(Codes), 
    [34], 
    !, 
    {   atom_codes_q(String,Codes) 
    }.

string_39([92,39|Codes]) --> 
    [92,39], 
    string_39(Codes), 
    !.
string_39([92,34|Codes]) --> 
    [92,34], 
    string_39(Codes), 
    !.
string_39([92,34|Codes]) --> 
    [34], 
    string_39(Codes), 
    !.
string_39([Code|Codes]) --> 
    [Code], 
    string_39(Codes), 
    {   Code \= 39 
    }, 
    !.
string_39([]) --> [].

string_34([92,39|Codes]) --> 
    [92,39], 
    string_34(Codes), 
    !.
string_34([92,34|Codes]) --> 
    [92,34], 
    string_34(Codes), 
    !.
string_34([92,34|Codes]) --> 
    [39], 
    string_34(Codes), 
    !.
string_34([Code|Codes]) --> 
    [Code], 
    string_34(Codes),
    {   Code \= 34 
    }, 
    !.
string_34([]) --> [].

atom(Atom) --> 
    atom_(Codes), 
    {   atom_codes_q(Atom,Codes) 
    }.
atom_([Code|Codes]) --> 
    [Code], 
    {   \+ code_type(Code,prolog_var_start), 
        code_type(Code,alnum) 
    }, 
    identifier(Codes). 

variable(Variable) --> 
    variable_(Codes), 
    {   atom_codes(Variable,Codes) 
    }.
variable_([Code|Codes]) --> 
    [Code], 
    {   code_type(Code,prolog_var_start) 
    }, 
    identifier(Codes).

identifier([Code|Codes]) --> 
    [Code], 
    {   code_type(Code,prolog_identifier_continue) 
    }, 
    identifier(Codes), 
    !.
identifier([]) --> [].

atom_codes_q(Atomq,Codes) :-
    atom_codes(Atom,Codes),
    format(atom(Atomq),'~q',[Atom]).


% PSP parser

psp_parse(DOM,Codes) :-
    with_output_to(codes(Output),psp_parse_dom(DOM)),
    append([":-\n",Output,"true."],Codes).

psp_parse_dom([]) :- !.	
psp_parse_dom([element(Tag,Aux,Elements)|DOM]) :- 
    downcase_atom(Tag,html),
    select('psp:prolog'=Prolog,Aux,Attributes), 
    \+ normalize_space(atom(''),Prolog), 
    !,
    format('~w,',[Prolog]),    
    format('http_format(\'<~w\',[]), ',[Tag]),    
    psp_attributes(Attributes),
    format('http_format(\'>\',[]), ',[]),
    psp_parse_dom(Elements),
    format('http_format(\'</~w>\',[]), ',[Tag]),
    psp_parse_dom(DOM).
psp_parse_dom([element(Tag,Aux,Elements)|DOM]) :- 
    select('psp:prolog'=Prolog,Aux,Attributes), 
    \+ normalize_space(atom(''),Prolog), 
    !,  
    format('http_format(\'<~w\',[]), ',[Tag]),    
    psp_attributes(Attributes),
    format('http_format(\'>\',[]), ',[]),
    format('(~w, ',[Prolog]),
    psp_parse_dom(Elements),
    format('fail ; true), ',[]),
    (	is_html_void(Tag)
    ->  true
    ;	format('http_format(\'</~w>\',[]), ',[Tag])
    ),
    psp_parse_dom(DOM).
psp_parse_dom([element(Tag,Attributes,Elements)|DOM]) :- 
    !,
    format('http_format(\'<~w\',[]), ',[Tag]),    
    psp_attributes(Attributes),
    format('http_format(\'>\',[]), ',[]),
    psp_parse_dom(Elements),
    (	is_html_void(Tag)
    ->  true
    ;   format('http_format(\'</~w>\',[]), ',[Tag])
    ),
    psp_parse_dom(DOM).
psp_parse_dom([Atom|DOM]) :-
    psp_layout(Atom,Normalized),
    (	normalize_space(atom(''),Normalized)
    ->  true
    ;   format('http_format(~q,[]), ',[Normalized])
    ),
    psp_parse_dom(DOM).
	
is_html_void(Tag) :-	
    % HTML void elements (http://www.w3.org/TR/html-markup/syntax.html#void-element)
    memberchk(Tag, 
        [   area, 
            base, 
            br, 
            col, 
            command, 
            embed, 
            hr, 
            img, 
            input, 
            keygen, 
            link, 
            meta, 
            param, 
            source, 
            track, 
            wbr
        ]
    ).
	    
psp_attributes([]) :- !.
psp_attributes([Qualified=Prolog|Attributes]) :- 
    atom_concat('psp:',Name,Qualified), 
    !,
    format('(with_output_to(atom(_),(~w)) -> (http_format(\' ~w="\',[]), call((~w)), http_format(\'"\',[])) ; true), ',
        [Prolog,Name,Prolog]
    ),    
    psp_attributes(Attributes).    
psp_attributes([Name=Value|Attributes]) :- 
    !,
    format(atom(Atom),'~q',[Value]),
    (   sub_atom(Atom,0,1,_,'\'')
    ->  sub_atom(Atom,1,_,1,QValue)
    ;   QValue = Value
    ),
    format('http_format(\' ~w="~w"\',[]), ',[Name,QValue]),
    psp_attributes(Attributes).
psp_attributes([Name|Attributes]) :-      
    % what happens to boolean attributes (sgml does not appear to process them properly!!!!)
    format('http_format(\' ~w\',[]), ',[Name]),
    psp_attributes(Attributes).
		
psp_layout(Atom,Normalized) :-
    atom_codes(Atom,In),
    psp_layout_(In,Out),
    atom_codes(Normalized,Out). 
psp_layout_([],[]) :- !.
psp_layout_([10|Codes],List) :- 
    !, % newline
    format('~n',[]),
    psp_layout_(Codes,List).
psp_layout_([9|Codes],List) :- 
    !, % tab
    psp_layout_(Codes,List).
psp_layout_([Code|Codes],[Code|List]) :-
    psp_layout_(Codes,List).
			
			