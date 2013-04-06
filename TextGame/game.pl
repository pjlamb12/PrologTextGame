%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% game.pl  
%
% Use this as the starting point for your game.  This starter code includes
% the following:
%  - Two example areas
%  - An example of how you might connect those areas 
%  - Handling of the actions 'go _______.', 'help.', and 'quit.'
%  - Basic input processing which strips punctuation and puts the words into a list 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module(library(readln)).

% Use this dynamic fact to store the players current location
:- dynamic current_area/1.

% Use this dynamic fact to add to the inventory
:- dynamic have/1.

% Use this dynamic fact to unlock doors and other similar items
:- dynamic unlocked/1.

% These are the items they can pick up
item(key, 'Key', 'A key to open locked doors').
item(shield, 'Shield', 'A shield to protect you').
item(sword, 'Sword', 'A sword with which to fight').
item(flashlight, 'Flashlight', 'The ligth will guide you through certain rooms').
item(irondoor, 'Iron Door', 'This door stands between you and the final room').
item(belt, 'Belt', 'The belt will hold your sword and flashlight').
item(elevator, 'Elevator', 'The elevator will take you to the top floor').
item(candy, 'Candybar', 'The Snickers bar will give you extra energy').
item(passage, 'Secret Passage', 'This passage will take you to the final room').
item(coin, 'Gold Coin', 'This gold coin is necessary to get in to the final room').

% Here is one way you might create your areas
area(room1, 'Room 1', 'You are in Room 1').
area(room2, 'Room 2', 'You are in Room 2').
area(room3, 'Room 3', 'You are in Room 3').
area(room4, 'Room 4', 'You are in Room 4').
area(room5, 'Room 5', 'You are in Room 5').
area(room6, 'Room 6', 'You are in Room 6').
area(room7, 'Quick Stop', 'You have entered the Quick Stop. All the partying and dancing has made you hungry. ').
area(room8, 'Admissions Office', 'You are in the Admissions office. You look around and see a lot of desks, but because it is after hours nobody is there working. Don\'t forget to leave without something to carry your items you will get throughout the night...').
area(room9, 'Ballroom', 'You have reached the ballroom!').

% Place the items in rooms
placed(room8, key).

% What you can pick up and what you cant
can_pickup(key).
can_pickup(backpack).
can_pickup(flashlight).
can_pickup(candy).



% You might connect those areas like this:
connected(south, room1, room4).
connected(east, room1, room2).
connected(east, room2, room3).
connected(south, room2, room5).
connected(west, room3, room2).
connected(south, room3, room6).
connected(north, room4, room1).
connected(south, room4, room7).
connected(west, room5, room4).
connected(south, room5, room8).
connected(north, room6, room3).
connected(south, room6, room9).
connected(north, room7, room4).
connected(east, room7, room8).
connected(north, room8, room5).
connected(north, room9, room6).

% This rule starts everything off
play :-
    retractall(current_area(_)),
    retractall(have(_)),
    assertz(current_area(room8)),
    print_introduction,
    print_location,
	dispPrompt,
    get_input.

% Prints out a welcome and introduction
print_introduction:-
    print('Welcome to Adventures at Utah State University. Go ahead and walk through some of the different rooms in the TSC that you may be familiar with.'), nl,
    print('As you go from one room to another, you will see a description of that room.'), nl,
    print('Remember that you are at the TSC during the HOWL Halloween party, so crazy stuff happens. When you\'re ready to start your quest, just type \'quest\'.'), nl,
    print('Hope you enjoy!'), nl, nl.

% Prints out the players current location description
print_location :-
    current_area(Current),
    area(Current, _, Description), write(Description), nl. 

print_inventory([]).
print_inventory([H|T]):-
    item(H, ItemName, Description), write(ItemName), tab(4), write(Description), nl, print_inventory(T).

% Changes the players current location, validity of change is checked earlier
change_area(NewArea) :-
    current_area(Current),
    retract(current_area(Current)),
    assertz(current_area(NewArea)).

% Displays the player prompt so they can enter actions
dispPrompt :- prompt(_, '> ').

% Add some help output here to explain how to play your game
process_input([help]) :- print('Add some help output here...'), nl.

% Opens doors and such if it is unlocked
process_input([open, Item]):-
    unlocked(Item),
    print('You have opened the door!'), nl.
process_input([open, _]):-
    print('You can\'t open the door, it is still locked.'), nl.

% Print out a players inventory
process_input([inventory]) :-
    findall(Item, have(Item), ItemList),
    print_inventory(ItemList).

% Handling of the action 'pickup _______'
process_input([pickup, Item]):-
    current_area(Current),
    placed(Current, Item),
    assertz(have(Item)),
    item(Item, ItemName, Description),
    print('You picked up a(n) '), write(ItemName), print('. Description: '), print(Description), nl, nl.
process_input([pickup, _]) :-
    print('There is nothing to pick up with that description. Sorry!'), nl, nl.

% Handling of the action 'go _______', and a good example of how you might implement others
process_input([go, Direction]) :-
    current_area(Current),
    connected(Direction, Current, NewRoom),
    change_area(NewRoom).
process_input([go, _]) :-
    print('You hit an invisible wall and can\'t go that way'), nl, nl.
process_input([_]) :-
    print('No idea what you are talking about...try again'), nl, nl.


%%%% Below is just some basic input handling, you probably dont have to mess with it %%%%

% Get input from the user
get_input :- read_sentence(Input), get_input(Input).
get_input([quit]).
get_input(Input) :-
    process_input(Input), print_location,
    read_sentence(Input1), get_input(Input1).
	
% Reads a sentence from the prompt
read_sentence(Input) :-
    readln(Input1, _, ".!?", "_0123456789", lowercase),
    strip_punctuation(Input1, Input).

% Strips punctuation out of the user input
strip_punctuation([], []).
strip_punctuation([Word|Tail], [Word|Result]) :-
    \+(member(Word, ['.', ',', '?', '!'])),
    strip_punctuation(Tail, Result).
strip_punctuation([_|Tail], Result) :-
    strip_punctuation(Tail, Result).