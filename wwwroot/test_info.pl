:-
         use_module(library(http/http_wrapper)),         use_module(library(http/http_dispatch)),         use_module(library(http/http_session)) ,http_format('<html',[]), http_format(' xmlns:psp="http://www.prologwebservices.org/psp"',[]), http_format('>',[]), http_format('<body',[]), http_format('>',[]), (     http_current_request(Request),      http_session_id(SessionID),      working_directory(Directory, Directory),      http_current_handler(Handler, Closure) , http_format('<h2',[]), http_format('>',[]), http_format('Welcome to Prolog Server Pages/Prolog XML',[]), http_format('</h2>',[]), http_format('<p',[]), http_format('>',[]), http_format('Current directory is ',[]), http_format('<code',[]), http_format('>',[]), (write(Directory), fail ; true), http_format('</code>',[]), http_format('</p>',[]), http_format('<p',[]), http_format('>',[]), http_format('Current Request is ',[]), http_format('<code',[]), http_format('>',[]), (write(Request), fail ; true), http_format('</code>',[]), http_format('</p>',[]), http_format('<p',[]), http_format('>',[]), http_format('Current SessionID is ',[]), http_format('<code',[]), http_format('>',[]), (write(SessionID), fail ; true), http_format('</code>',[]), http_format('</p>',[]), http_format('<p',[]), http_format('>',[]), http_format('Current handler is ',[]), http_format('<code',[]), http_format('>',[]), (write(Handler), fail ; true), http_format('</code>',[]), http_format(' with closure ',[]), http_format('<code',[]), http_format('>',[]), (write(Closure), fail ; true), http_format('</code>',[]), http_format('</p>',[]), fail ; true), http_format('</body>',[]), http_format('</html>',[]), true.