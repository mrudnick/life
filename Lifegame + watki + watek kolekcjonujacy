-module(lifegame7).
-export([testWrite/1, testRead/1, readAll/1, readPart/3, main/1,watek/0, next/1, collectData/0]).
-record(field,{y= 0,x= 0,value= 0}).

%% ZAPIS LOSOWEJ PLANSZY:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lifeWrite(FileName,Size)->
		{ok,FD} = file:open(FileName,[write, compressed]),
		file:write(FD,Size),
		{ok,FD}.

testWrite(Size) ->
		Len = trunc(math:pow(2,Size)),
		{ok,FD} = lifeWrite('fff.gz',8),
		file:write(FD,[Size]),
		feedData(FD,Len,Len),
		file:close(FD).

writeData(FD,Data) ->
	file:write(FD,Data).
		
feedData(_FD,0,_Len)-> ok;
feedData(FD,Count,Len) ->
	Data = [random:uniform(2)+47 || _ <- lists:seq(1, Len)],
	writeData(FD,Data),
	feedData(FD,Count-1,Len).

%% PRZYKLADOWE WCZYTYWANIE:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
lifeRead(FileName) ->
		{ok,FD} = file:open(FileName,[read, compressed]),
		case file:read(FD,1) of 
				{ok,[Data]} -> {FD,Data};
				eof -> io:format("~nKoniec~n",[]);
				{error,Reason} -> io:format("~s~n",[Reason])
		end.
	
testRead(FileName) ->
	{FD,Size} = lifeRead(FileName),
	Len = trunc(math:pow(2,Size)),
	io:fwrite("Rozmiar ~B, Plansza ~Bx~B~n",[Size,Len,Len]),
		file:close(FD).

		
%% WCZYTYWANIE DO FORMATU W PROGRAMIE:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Wczytywanie całości:

readData(FD,Size) -> 
	case file:read(FD,Size) of 
		{ok,Data} -> Data;
		eof -> io:format("~nKoniec~n",[]);
		{error,Reason} -> io:format("~s~n",[Reason])
	end.

%% FD- deskryptor, Rozm- rozmiar tablicy(dlugosc wiesza/kolumny), RowN- numer wiersza, Count- licznik(kolumny), Data- budowany wiersz
readRow(FD,Rozm,RowN,Count,Data) when Count > Rozm -> Data;
readRow(FD,Rozm,RowN,Count,Data) -> readRow(FD,Rozm,RowN,Count+1,lists:append(Data, [#field{y=RowN, x=Count, value= list_to_integer(readData(FD,1))}])).  %% value= list_to_integer(readData(FD,1))
	
	
% FD- deskryptor, Rozm- rozmiar tablicy(dlugosc wiesza/kolumny), Count- licznik(wierszy), Data- budowana tablica
readTableContent(FD,Rozm,Count,Data) when (Rozm =:= Count) -> lists:append(Data, [readRow(FD,Rozm,Count,1,[])]);
readTableContent(FD,Rozm,Count,Data) -> readTableContent(FD,Rozm,Count+1, lists:append(Data,[readRow(FD,Rozm,Count,1,[])])).


readTableContentPart(FD,Rozm,OGR,Count,[]) -> readTableContentPart(FD,Rozm,OGR,Count+1,[readRow(FD,Rozm,Count,1,[])]);
readTableContentPart(FD,Rozm,OGR,Count,Data) when (Count =< OGR) -> readTableContentPart(FD,Rozm,OGR,Count+1, lists:append(Data,[readRow(FD,Rozm,Count,1,[])]));
readTableContentPart(FD,Rozm,OGR,Count,Data) when (Count > OGR ) -> Data.

readFile(FileName) -> 
	{ok,FD} = file:open(FileName, [read, compressed]),
	case file:read(FD,1) of 
		{ok,[Data]} -> {FD,Data};
		eof -> io:format("~nKoniec~n",[]);
		{error,Reason} -> io:format("~s~n",[Reason])
	end.

% Czyta całą tablicę.
readAll(FileName) ->
	{FD,Size} = readFile(FileName),
	Len = trunc(math:pow(2,Size)),
	io:fwrite("Rozmiar ~B, Plansza ~Bx~B~n",[Size,Len,Len]),
	D =readTableContent(FD,Len,1,[]),
	io:format("~c",D),
	file:close(FD).

% Czyta tylko część tablicy.	
readPart(FileName,StartRow,Ile) ->
	{FD,Size} = readFile(FileName),
	Len = trunc(math:pow(2,Size)),
	io:fwrite("Rozmiar ~B, Plansza ~Bx~B~n",[Size,Len,Len]),
	file:position(FD,(StartRow-1)*Len +1),
	readTableContentPart(FD,Len,Ile,1,[]).	

%% ZAPIS PRZETWORZONEJ TABLICY DO PLIKU:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

writeElement(FD,El = #field{}) ->
	file:write(FD,[El#field.value +48]).

writeCols(FD,[]) -> ok;
writeCols(FD,[H|T]) -> writeElement(FD,H),
	writeCols(FD,T).
	
	
writeRows(FD,[]) -> ok;
writeRows(FD,[H|T]) -> 
	writeCols(FD,H),
	writeRows(FD,T).
	
% do zapisu calejtablicy:
writeTableContent(Data,Name) ->
	Len = trunc(math:pow(2,length(Data))),
	{ok,FD} = lifeWrite(string:concat("a", string:concat(integer_to_list(Name),".gz")),8),
	file:write(FD,trunc(math:sqrt(length(Data)))),
	writeRows(FD,Data).

%zapisuje jeden wiersz:
writeTableContentOneRow(Data,Name) ->
	%io:format("~c",Data),
	{ok, FD} = file:open(string:concat(integer_to_list(Name),".gz"),[write, compressed]),
	file:write(FD,[length(Data)]), 		%zapis ilosci elementow.
	writeCols(FD,Data),
	file:close(FD).
	%{ok,FD} = lifeWrite(string:concat("a", string:concat(integer_to_list(Name),".gz")),8),
	%file:write(FD,[length(Data)]),
	%writeCols(FD,Data).
	
writeIterationData(FileName,Row,Data) ->
	{FD,Size} = lifeRead(FileName),
	Len= trunc(math:pow(2,length(Data))),
	file:position(FD,(Row-1)*Len+1),
	writeCols(FD,Data),
	file:close(FD).
	%{ok,FD} = file:open(FileName,[read, append, compressed]),
	%file:position(FD, bof+8),	%przeniesienie na poczatek
	%file:position(FD,(Row-1)*length(Data)+1),
	%file:write(FD,[5]),%writeCols(FD,Data),
	%file:close(FD).
	
	
%% GRA - ITERACJA
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% funkcje do peracji na sasiadach:
neighbours_array() -> [{-1,0},{-1,-1},{0,-1},{1,-1},{1,0},{1,1},{0,1},{-1,1}].
create_neighbours({Y,X}) -> [ {C+Y,D+X} || {C,D} <- [{-1,0},{-1,-1},{0,-1},{1,-1},{1,0},{1,1},{0,1},{-1,1}]].

%przyjmuje jeden wiersz z tablicy- zwraca wiersz do nowej tablicy.
%iterate_row([H|T]) -> lists:map(check_next,[H|T]). %[ check_next(A) || A <- [H|T]].

% pobieranie elementow i wartosci z tablicy
get_element(Y,X, Tab) -> lists:nth(X,lists:nth(Y, Tab)).
get_value(A = #field{})-> A#field.value.


% sprawdzenie zasady gry w życie:
rule(1, N) when (N == 2) or (N == 3) -> 1;
rule(1, _) -> 0;
rule(0, 3) -> 1;
rule(0,_) -> 0.


check_next(El= #field{}, Tab) -> rule(get_value(get_element(El#field.y, El#field.x, Tab)) ,lists:sum([get_value(get_element(A, B, Tab)) || {A,B} <- create_neighbours({El#field.y,El#field.x}), A>=1, A=<length(Tab), B>=1, B=<length(lists:nth(1,Tab))])).

%przyjmuje jeden wiersz z tablicy oraz cala tablice zeby szukac sasiadow- zwraca wiersz do nowej tablicy.
%%
%%
iterate_row([H|T], Tab) -> [ #field{y= A#field.y, x= A#field.x, value= check_next(A, Tab)} || A <- [H|T]].

% przyjmuje aktualną tablice i pustą - nowa tablica która będzie tworzona na podstawie poprzedniej... zwraca nową tablicę.
%%
%%
iterate(Tab,0, [H2|T2]) -> [H2|T2];
iterate(Tab,N, [H2|T2]) -> iterate(Tab,N-1,[iterate_row(lists:nth(N,Tab), Tab)|[H2|T2]]);
iterate(Tab,N, []) -> iterate(Tab, N-1, [iterate_row(lists:nth(N,Tab), Tab)]).

next(Tab) -> iterate(Tab,length(Tab),[]).


%% WATKI:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

watek() ->
	receive
		{From,{Row,Max_Row},FileName, To} when Row =:= 1 ->
			To ! {Row, lists:nth(1,next(readPart(FileName,1,2))),From};		% zpisujemy tylko pierwszy wiersz bo 2 był tylko do obliczen.
		{From,{Row,Max_Row},FileName, To} when (Row > 1) and (Row < Max_Row) ->
			To ! {Row, lists:nth(2,next(readPart(FileName,Row-1,3))),From};	% tutaj srodkowy
		{From,{Row,Max_Row},FileName, To} when Row =:= Max_Row ->
			To ! {Row, lists:nth(2,next(readPart(FileName,Row-1,2))),From}	% tu 2 wiersz
	end.
	
	

make_parallel(Rows, 0, FileName, SendTo) -> "Koniec";
make_parallel(Rows, Count,FileName, SendTo) -> Pid = spawn(?MODULE,watek,[]),
	Pid ! {self(),{Count,Rows},FileName, SendTo},
	make_parallel(Rows, Count-1, FileName, SendTo).

	
% Jakiś wątek który zbiera dane i zapisuje do jednego pliku:
collectData() ->
	receive
		{Row,Data, ToShell} -> writeIterationData("out.gz",Row,Data),
			ToShell ! {collectData,Row},
			collectData()
	end.


main(FileName) ->
	CollectPid= spawn(?MODULE,collectData,[]),			% stworzenie watku zbierajacego dane od watkow pracujacych.
	{ok,FD1} = file:open("out.gz",[write, compressed]),  % stworzenie pliku wyjsciowego dla iteracji 
	file:write(FD1,8),
	{FD,Size} = lifeRead(FileName),
	Len = trunc(math:pow(2,Size)),
	file:write(FD1,[Len]),		% zapisanie rozmiaru ostatecznej tablicy do pliku wyjściowego iteracji.
	file:close(FD1),			% zamkniecie pliku z ostateczna iteracją.
	Len = trunc(math:pow(2,Size)),		%odpalenie watków pracujacych.
	make_parallel(Len,Len, FileName, CollectPid).
	
	
%% FUNKCJA TESTUJĄCA:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_avg(M, F, A, N) when N > 0 ->
    L = test_loop(M, F, A, N, []),
    Length = length(L),
    Min = lists:min(L),
    Max = lists:max(L),
    Med = lists:nth(round((Length / 2)), lists:sort(L)),
    Avg = round(lists:foldl(fun(X, Sum) -> X + Sum end, 0, L) / Length),
    io:format("Range: ~b - ~b mics~n"
          "Median: ~b mics~n"
          "Average: ~b mics~n",
          [Min, Max, Med, Avg]),
    Med.
 
test_loop(_M, _F, _A, 0, List) ->
    List;
test_loop(M, F, A, N, List) ->
    {T, _Result} = timer:tc(M, F, A),
    test_loop(M, F, A, N - 1, [T|List]).
