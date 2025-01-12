:-    
    <html>,
    <body>,
        write('Flag: '),    
        <select>,
        (   current_prolog_flag(Name, Value),
            (   Name == dialect 
            ->  Selected = 'selected="selected"'
            ;   Selected = ''
            ),
            <option value=Name Selected>,
                write(Value),
            </option>,
            fail
        ;   true
        ),
        </select>,
    </body>,
    </html>.


/*
:- op(900, xfy, (?)).

% (?)/2
?(=(Variable, Condition), :(Then, Else)) :- (Condition -> Variable = Then ; Variable = Else).

Selected = (Name == dialect) ? 'selected' : '',
*/



/*
:- use_module(library(aggregate)). % foreach/2

<selectflag defaultflag=Flag/> :-
    <select>,
        foreach(current_prolog_flag(Name, Value), (
            Name == Flag -> Selected = 'selected' ; Selected = '',
            <option value=Name Selected>,
                write(Value),
            </option>
        )),
    </select>.

:-    
    <html>,
    <body>,
        write('Flag: '),    
        <template name="selectflag" defaultflag='dialect'/>,
    </body>,
    </html>.
*/

