%Consulta el archivo con las bases de datos
:-consult(database).


%********************** Leer input del usuario ***************************

% lower_case(+C,?L)
%   If ASCII code C is an upper-case letter, then L is the
%   corresponding lower-case letter. Otherwise L=C.

lower_case(X,Y) :-
	X >= 65,
	X =< 90,
	Y is X + 32, !.

lower_case(X,X).

% read_lc_string(-String)
%  Reads a line of input into String as a list of ASCII codes,
%  with all capital letters changed to lower case.

read_lc_string(String) :-
	get0(FirstChar),
	lower_case(FirstChar,LChar),
	read_lc_string_aux(LChar,String).

read_lc_string_aux(10,[]) :- !.  % end of line

read_lc_string_aux(-1,[]) :- !.  % end of file

read_lc_string_aux(LChar,[LChar|Rest]) :- read_lc_string(Rest).


% extract_word(+String,-Rest,-Word) (final version)
%  Extracts the first Word from String; Rest is rest of String.
%  A word is a series of contiguous letters, or a series
%  of contiguous digits, or a single special character.
%  Assumes String does not begin with whitespace.

extract_word([C|Chars],Rest,[C|RestOfWord]) :-
	my_char_type(C,Type),
	extract_word_aux(Type,Chars,Rest,RestOfWord).

extract_word_aux(special,Rest,Rest,[]) :- !.
   % if Char is special, don't read more chars.

extract_word_aux(Type,[C|Chars],Rest,[C|RestOfWord]) :-
	my_char_type(C,Type), !,
	extract_word_aux(Type,Chars,Rest,RestOfWord).

extract_word_aux(_,Rest,Rest,[]).   % if previous clause did not succeed.


% remove_initial_blanks(+X,?Y)
%   Removes whitespace characters from the
%   beginning of string X, giving string Y.

remove_initial_blanks([C|Chars],Result) :-
	my_char_type(C,whitespace), !,
	remove_initial_blanks(Chars,Result).

remove_initial_blanks(X,X).   % if previous clause did not succeed.

% digit_value(?D,?V)
%  Where D is the ASCII code of a digit,
%  V is the corresponding number.

digit_value(48,0).
digit_value(49,1).
digit_value(50,2).
digit_value(51,3).
digit_value(52,4).
digit_value(53,5).
digit_value(54,6).
digit_value(55,7).
digit_value(56,8).
digit_value(57,9).

% string_to_number(+S,-N)
%  Converts string S to the number that it
%  represents, e.g., "234" to 234.
%  Fails if S does not represent a nonnegative integer.

string_to_number(S,N) :-
	string_to_number_aux(S,0,N).

string_to_number_aux([D|Digits],ValueSoFar,Result) :-
	digit_value(D,V),
	NewValueSoFar is 10*ValueSoFar + V,
	string_to_number_aux(Digits,NewValueSoFar,Result).

string_to_number_aux([],Result,Result).


% string_to_atomic(+String,-Atomic)
%  Converts String into the atom or number of
%  which it is the written representation.

string_to_atomic([C|Chars],Number) :-
	string_to_number([C|Chars],Number), !.

string_to_atomic(String,Atom) :- name(Atom,String).
  % assuming previous clause failed.

% extract_atomics(+String,-ListOfAtomics) (second version)
%  Breaks String up into ListOfAtomics
%  e.g., " abc def  123 " into [abc,def,123].

extract_atomics(String,ListOfAtomics) :-
	remove_initial_blanks(String,NewString),
	extract_atomics_aux(NewString,ListOfAtomics).

extract_atomics_aux([C|Chars],[A|Atomics]) :-
	extract_word([C|Chars],Rest,Word),
	string_to_atomic(Word,A),       % <- this is the only change
	extract_atomics(Rest,Atomics).

extract_atomics_aux([],[]).

% my_char_type(+Char,?Type)
%    Char is an ASCII code.
%    Type is whitespace, punctuation, numeric, alphabetic, or special.

my_char_type(46,period) :- !.
my_char_type(X,alphanumeric) :- X >= 65, X =< 90, !.
my_char_type(X,alphanumeric) :- X >= 97, X =< 123, !.
my_char_type(X,alphanumeric) :- X >= 48, X =< 57, !.
my_char_type(X,whitespace) :- X =< 32, !.
my_char_type(X,punctuation) :- X >= 33, X =< 47, !.
my_char_type(X,punctuation) :- X >= 59, X =< 64, !.
my_char_type(X,punctuation) :- X >= 91, X =< 96, !.
my_char_type(X,punctuation) :- X >= 123, X =< 126, !.
my_char_type(_,special).

% clean_string(+String,-Cleanstring)
%  removes all punctuation characters from String and return Cleanstring

clean_string([C|Chars],L) :-
	my_char_type(C,punctuation),
	clean_string(Chars,L), !.
clean_string([C|Chars],[C|L]) :-
	clean_string(Chars,L), !.
clean_string([C|[]],[]) :-
	my_char_type(C,punctuation), !.
clean_string([C|[]],[C]).


% read_atomics(-ListOfAtomics)
%  Reads a line of input, removes all punctuation characters, and converts
%  it into a list of atomic terms, e.g., [this,is,an,example].

read_atomics(ListOfAtomics) :-
	read_lc_string(String),
	clean_string(String,Cleanstring),
	extract_atomics(Cleanstring,ListOfAtomics).




/*****************************************************************************/
%
% Verificacion de BNF's
%


% sintagma_nominal + sintagma_verbal
oracion(S0,S):- sintagma_nominal(S0,S1), sintagma_verbal(S1,S),!.
%
% Crear aca otras oraciones con los saludos, despedidas y preguntas que
% tienen otro formato diferente a las oraciones con sintagmas definidos.
%



% Sintagmas Nominales:
%
% Determinante + Sujeto
sintagma_nominal(S0,S):- determinante(S0,S1),sujeto(S1,S),!.

% Determinante
sintagma_nominal(S0,S):- determinante(S0,S),!.
%
% Sujeto
sintagma_nominal(S0,S):- sujeto(S0,S),!.
%
%



% Sintagmas Verbales:

% -------------SI INICIA CON DETERMINANTE---------------
% Determinante(se) + Verbo + Sintagma Nominal
sintagma_verbal(S0,S):- determinante(S0,S1), verbo(S1,S2), sintagma_nominal(S2,S),!.

%Determinante(se) + Verbo + Adjetivo
sintagma_verbal(S0,S):- determinante(S0,S1), verbo(S1,S2), adjetivo(S2,S),!.

% Determinante(se) + Verbo
sintagma_verbal(S0,S):- determinante(S0,S1), verbo(S1,S),!.


% -------------SI INICIA CON NEGACION---------------

% Negacion + Determinante(se) + Verbo + Sintagma Nominal
sintagma_verbal(S0,S):- negacion(S0,S1), determinante(S1,S2), verbo(S2,S3), sintagma_nominal(S3,S),!.

% Negacion + Determinante(se) + Verbo
sintagma_verbal(S0,S):- negacion(S0,S1), determinante(S1,S2), verbo(S2,S),!.

% Negacion + Verbo + Sintagma Nominal
sintagma_verbal(S0,S):- negacion(S0,S1), verbo(S1,S2), sintagma_nominal(S2,S),!.

% Negacion + Verbo
sintagma_verbal(S0,S):- negacion(S0,S1), verbo(S1,S),!.

% -------------SI INICIA CON VERBO--------------
% Verbo + Sintagma Nominal + Adjetivo
sintagma_verbal(S0,S):- verbo(S0,S1), sintagma_nominal(S1,S2), adjetivo(S2,S),!.

% Verbo + Sintagma Nominal
sintagma_verbal(S0,S):- verbo(S0,S1), sintagma_nominal(S1,S),!.

% Verbo + Adjetivo
sintagma_verbal(S0,S):- verbo(S0,S1), adjetivo(S1,S),!.

% Verbo
sintagma_verbal(S0,S):- verbo(S0,S),!.

% Se podrian definir aca los casos "alambrados" de saludos y despedidas
% para la verificacion
%



/*****************************************************************************/
% Verificaciones de pertenencia en base de datos (archivo database.pl)

% Busca la pertenencia del determinante obtenido, en la base de datos
determinante([X|S],S) :- miembroDet(X).

% Busca la pertenencia del sujeto obtenido, en la base de datos
sujeto([X|S],S) :- miembroSuj(X).

% Busca la pertenencia del verbo obtenido, en la base de datos
verbo([X|S],S) :- miembroVer(X).

% Busca la pertenencia del adjetivo obtenido, en la base de datos
adjetivo([X|S],S) :- miembroAdj(X).

% Busca la pertenencia de la afirmacion obtenido, en la base de datos
afirmacion([X|S],S) :- miembroAfi(X).

% Busca la pertenencia de la negacion obtenido, en la base de datos
negacion([X|S],S) :- miembroNeg(X).

% Busca la pertenencia del inicio obtenido, en la base de datos
inicio([X|S],S) :- miembroIni(X).

% Busca la pertenencia del fin obtenido, en la base de datos
fin([X|S],S) :- miembroFin(X).


pth([]):- nl.
pth([H|T]):- write(H), tab(1), pth(T).

% -------------------------- List functions--------------------------
split(L,P,R):-split(L,P,[],R).
split([],_,[],[]).
split([],_,S,[S]) :- S \= [].
split([P|T],P,[],R) :- split(T,P,[],R).
split([P|T],P,L,[L|R]) :- L \= [], split(T,P,[],R).
split([H|T],P,S,R) :- H \= P, append(S, [H], S2), split(T,P,S2,R).


% add_tail(+List,+Element,-List)
% Add the given element to the end of the list, without using the "append" predicate.
add_tail([],X,[X]).
add_tail([H|T],X,[H|L]):-add_tail(T,X,L).

/*****************************************************************************/

%patrones de problema
identificaProblema(Producto,_,_):-
	problema([X|R]),X==Producto,pth(R).

%identificaProblemaAux2([X|R],

%identificaProblemaAux([],_,fail):-!.
%identificaProblemaAux([X|R],Input,Problema):-identificaProblemaAux2(X,Input,Problema),identificaProblemaAux(R,Input,Problema).
%
%


%problemaMac([X|Patrones],Input,Problema):-
%	subset(X,Input),!.

%

%Identifica si hay mas de una oracion de entrada y la valida.


identificaSolicitud([],_,_):-!,fail.
identificaSolicitud([X|_],Z,B):-miembro(X,Z),B = X,!.
identificaSolicitud([_|R],Z,B):-identificaSolicitud(R,Z,B),!.

oracionMultiple([]):-!.
oracionMultiple([X|MultipleList]):-pth(X),oracion(X,[]),oracionMultiple(MultipleList).

tipoInput(Input):-oracion(Input,[]),
	(
	%El caso de si esta pidiendo por referncia
	referencia(D),identificaSolicitud(D,Input,_);

	%El caso de si es una causa


	%El caso de si es un problema
	producto(L),identificaSolicitud(L,Input,P),
	add_tail([],problema,List),
	add_tail(List,P,List2),
	pth(List2),
	identificaProblema(List2,Input,_)
	).

tipoInput(Input):-miembro(:,Input),
	(
	split(Input,:,MultipleList),oracionMultiple(MultipleList)
	).



/*****************************************************************************/
%
% Main de CallCenterLog
%
callCenterLog :- write(">> "),
		 read_atomics(Input),nl,pth(Input),
		 tipoInput(Input).
% Inicio
%:- callCenterLog.

%Mi Macbook se reinicia sola: Mi Macbook no funciona.
