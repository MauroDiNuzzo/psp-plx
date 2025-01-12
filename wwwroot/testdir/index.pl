
p1(What, Fontsize) :-
    format(atom(Style), 'font-size:~wpx;', [Fontsize]),
    (<p>([style=Style])),
    write(What),
    (</p>([])).


:-
    (<html>([])),
    (<body>([whatever=''])),
    load_files(wwwroot('testdir/include.plx'), []),
    (<h1>([])),
    title(Title),
    write(Title),
    (</h1>([])),
    Style = 'color: #ff0000;',
    E = ( (<'B'>([style=Style])), write('To be reused?'), (</b>([])) ),
    E,
    (<a>([href='ciao.html'])),
    write('Hello world!'),
    X = (</a>([])), 
    X,
    write('Check <span>quotes</span> or not?<br>'),
    (   member(Y, [1, 2, 3]),
        p1(Y, 20), 
        fail
    ;   true
    ),
    E, % reuse E here
    Element = img,
    Attr = src,
    (<Element/>([Attr='../pengines/queen.png'])),
    (</body>([])),
    (</html>([])).

