% server.pl

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_error)).
:- use_module(library(http/http_dirindex)).
:- use_module(library(http/html_write)).
:- use_module(library(www_browser)).

:- use_module(src/http_psp_plx).
:- debug(http_psp_plx).

:- initialization(server(4040),after_load).
%:- at_halt(http_stop_server(4040,[])).


:- prolog_load_context(directory,Dir),
   absolute_file_name(wwwroot,Path,[relative_to(Dir),file_type(directory)]),
   asserta(user:file_search_path(wwwroot,Path)).

%:- http_handler(root(Path),http_reply_dirindex(wwwroot(Path),[]),[prefix]). % test

:- http_handler(root(.),psp_plx_handler([
        path_alias(wwwroot), 
        index_files(['index.psp','index.plx']),
        index_hook(http_reply_dirindex),
        hide_extensions([pl,config])
    ]),[prefix]).

server(Port) :-
    http_server(http_dispatch,[port(Port)]),
    parse_url(URL,[protocol(http),host(localhost),port(Port)]),
    www_open_url(URL).
