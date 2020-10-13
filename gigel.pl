:- ensure_loaded('chat.pl').

% Returneaza true dacă regula dată ca argument se potriveste cu
% replica data de utilizator. Replica utilizatorului este
% reprezentata ca o lista de tokens. Are nevoie de
% memoria replicilor utilizatorului pentru a deduce emoția/tag-ul
% conversației.
% match_rule/3
% match_rule(_Tokens, _UserMemory, rule(_, _, _, _, _)) :- fail.
match_rule(Tokens, UserMemory, rule(Expr, _, _, [Emotion], [Tag])) :-
     get_tag(UserMemory, Tag),
     get_emotion(UserMemory, Emotion),
     Expr = Tokens.

match_rule(Tokens, UserMemory, rule(Expr, _, _, [], [Tag])) :-
     get_tag(UserMemory, Tag),
     Expr = Tokens.

match_rule(Tokens, UserMemory, rule(Expr, _, _, [Emotion], [])) :-
     get_emotion(UserMemory, Emotion),
     Expr = Tokens.


match_rule(Tokens, _, rule(Expr, _, _, [], [])) :-
     Expr = Tokens.


% Primeste replica utilizatorului (ca lista de tokens) si o lista de
% reguli, iar folosind match_rule le filtrează doar pe cele care se
% potrivesc cu replica dată de utilizator.
% find_matching_rules/4
% find_matching_rules(+Tokens, +Rules, +UserMemory, -MatchingRules)

comparator( <, rule(_,_,_,_,[_]), rule(_,_,_,_,[])).
comparator( <, rule(_,_,_,_,[_]), rule(_,_,_,_,[])).
comparator( <, rule(_,_,_,[_],_), rule(_,_,_,[],_)).
comparator( >, rule(_,_,_,[],_), rule(_,_,_,[_],_)).
comparator( >, rule(_,_,_,_,[]), rule(_,_,_,_,[_])).



find_matching_rules(Tokens, Rules, UserMemory, MatchingRules) :-
    findall(R,(member(R,Rules), match_rule(Tokens, UserMemory, R)),
             GoodRules),
    predsort(comparator, GoodRules, MatchingRules).

% Intoarce in Answer replica lui Gigel. Selecteaza un set de reguli
% (folosind predicatul rules) pentru care cuvintele cheie se afla in
% replica utilizatorului, in ordine; pe setul de reguli foloseste
% find_matching_rules pentru a obtine un set de raspunsuri posibile.
% Dintre acestea selecteaza pe cea mai putin folosita in conversatie.
%
% Replica utilizatorului este primita in Tokens ca lista de tokens.
% Replica lui Gigel va fi intoarsa tot ca lista de tokens.
%
% UserMemory este memoria cu replicile utilizatorului, folosita pentru
% detectarea emotiei / tag-ului.
% BotMemory este memoria cu replicile lui Gigel și va si folosită pentru
% numararea numarului de utilizari ale unei replici.
%
% In Actions se vor intoarce actiunile de realizat de catre Gigel in
% urma replicii (e.g. exit).
%
% Hint: min_score, ord_subset, find_matching_rules

% select_answer/5
% select_answer(+Tokens, +UserMemory, +BotMemory, -Answer, -Actions)
select_answer(Tokens, UserMemory, BotMem, Answer, Actions) :-
   rules(SubTokens, Rules),
   ord_subset(SubTokens, Tokens),
   find_matching_rules(Tokens, Rules, UserMemory,
   [(rule(_, Replies, Actions, _, _))|_]),
   findall((Key,F),(member(R,Replies), get_answer(R,BotMem,F), unwords(R, Key)),ReplFreq),
   min_element(ReplFreq, AnswerStr),
   words(AnswerStr, Answer).

% Esuează doar daca valoarea exit se afla in lista Actions.
% Altfel, returnează true.
% handle_actions/1
% handle_actions(+Actions)
handle_actions(Actions) :- not(member(exit, Actions)).


% Caută frecvența (numărul de apariți) al fiecarui cuvânt din fiecare
% cheie a memoriei.
% e.g
% ?- find_occurrences(memory{'joc tenis': 3, 'ma uit la box': 2, 'ma uit la un film': 4}, Result).
% Result = count{box:2, film:4, joc:3, la:6, ma:6, tenis:3, uit:6, un:4}.
% Observați ca de exemplu cuvântul tenis are 3 apariți deoarce replica
% din care face parte a fost spusă de 3 ori (are valoarea 3 în memorie).
% Recomandăm pentru usurința să folosiți înca un dicționar în care să tineți
% frecvențele cuvintelor, dar puteți modifica oricum structura, această funcție
% nu este testată direct.

% find_occurrences/2
% find_occurrences(+UserMemory, -Result)

s_list([],0).
s_list([Head|Rest], S):- s_list(Rest, RestSum), S is Head + RestSum.

add_freq([],[]).
add_freq([HKey-Frq|R], [HKey-S|ResR]):-  s_list(Frq, S), add_freq(R,ResR).

find_occurrences(UserMemory, Result):-
      dict_keys(UserMemory, Keys),
      findall(T-F, (member(Key,Keys), (words(Key, Tokens),
      member(T,Tokens), get_value(UserMemory,Key, F))), KeyPairs),
      sort(1, @>=, KeyPairs, SortedPairs),
      group_pairs_by_key(SortedPairs, GroupedSorted),
      add_freq(GroupedSorted, FrqWords),
      dict_pairs(Result,_,FrqWords) .

% Atribuie un scor pentru fericire (de cate ori au fost folosit cuvinte din predicatul happy(X))
% cu cât scorul e mai mare cu atât e mai probabil ca utilizatorul să fie fericit.
% get_happy_score/2
% get_happy_score(+UserMemory, -Score)
get_happy_score(UserMemory, Score) :-
    findall(S, (happy(X), find_occurrences(UserMemory,FreqW), get_value(FreqW, X, S)), ScoreList),
    s_list(ScoreList, Score).

% Atribuie un scor pentru tristețe (de cate ori au fost folosit cuvinte din predicatul sad(X))
% cu cât scorul e mai mare cu atât e mai probabil ca utilizatorul să fie trist.
% get_sad_score/2
% get_sad_score(+UserMemory, -Score)
get_sad_score(UserMemory, Score) :-
    findall(S, (sad(X), find_occurrences(UserMemory,FreqW), get_value(FreqW, X, S)), ScoreList),
    s_list(ScoreList, Score).


% Pe baza celor doua scoruri alege emoția utilizatorul: `fericit`/`trist`,
% sau `neutru` daca scorurile sunt egale.
% e.g:
% ?- get_emotion(memory{'sunt trist': 1}, Emotion).
% Emotion = trist.
% get_emotion/2
% get_emotion(+UserMemory, -Emotion)
get_emotion(UserMemory, fericit) :- get_happy_score(UserMemory, S1), get_sad_score(UserMemory, S2), S1 > S2.
get_emotion(UserMemory, trist) :- get_happy_score(UserMemory, S1), get_sad_score(UserMemory, S2), S1 < S2.
get_emotion(UserMemory, neutru) :- get_happy_score(UserMemory, S1), get_sad_score(UserMemory, S2), S1 = S2.

% Atribuie un scor pentru un Tag (de cate ori au fost folosit cuvinte din lista tag(Tag, Lista))
% cu cât scorul e mai mare cu atât e mai probabil ca utilizatorul să vorbească despre acel subiect.
% get_tag_score/3
% get_tag_score(+Tag, +UserMemory, -Score)
get_tag_score(tag(_,TList), UserMemory, Score) :-
     find_occurrences(UserMemory,FreqW),
     findall(S, (member(T, TList), get_value(FreqW, T, S)), ScoreList),
     s_list(ScoreList, Score).


% Pentru fiecare tag calculeaza scorul și îl alege pe cel cu scorul maxim.
% Dacă toate scorurile sunt 0 tag-ul va fi none.
% e.g:
% ?- get_tag(memory{'joc fotbal': 2, 'joc box': 3}, Tag).
% Tag = sport.
% get_tag/2
% get_tag(+UserMemory, -Tag)

empty_tag([], none).

get_tag(UserMemory, Tag) :-
     findall((T, S), (tag(T, TList), get_tag_score(tag(T,TList), UserMemory, S), S\== 0), TagList),
    (max_element(TagList, Tag) ; empty_tag(TagList, Tag)).
