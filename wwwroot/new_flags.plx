



myoption(Name,Value,Selected) :-
    <option value=Value Selected>,
        format('~w: ~w',[Name,Value]),
    </option>.    

:-    
    <html>,
    <body>,
        write('Flag: '),    
        <select>,
        (   current_prolog_flag(Name,Value),
            (   Name == dialect 
            ->  Selected = 'selected'
            ;   Selected = ''
            ),
            myoption(Name,Value,Selected),
            fail
        ;   true
        ),
        </select>,
    </body>,
    </html>.

