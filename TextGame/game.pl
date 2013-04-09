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

% This is similar to the inventory, but to put items in your backpack
:- dynamic in_backpack/1.

% Use this dynamic fact to unlock doors and other similar items
:- dynamic unblocked/1.

% These are the items they can pick up
item(key, 'Key', 'A key to open locked doors').
item(idcard, 'ID Card', 'A shield to protect you').
item(mask, 'Mask', 'This is the rest of your costume and will hide your identity').
item(flashlight, 'Flashlight', 'The light will guide you through certain rooms').
item(door, 'Door', 'This door stands between you and the final room').
item(backpack, 'Backpack', 'The backpack will hold items that you pick up').
item(elevator, 'Elevator', 'The elevator will take you to the top floor').
item(candy, 'Snickers', 'The Snickers bar will give you extra energy').
item(soda, 'Dr. Pepper', 'The Dr. Pepper will quench your thirst').
item(passage, 'Secret Passage', 'This passage will take you to the final room').
item(money, 'Money', 'The money is necessary to get in to the final room').

% Here is one way you might create your areas
area(room1, 'The Registrar\'s Office', 'You just entered the Registrar\'s office. Nothing is really happening in this room.').
area(room2, 'The Ballroom', 'You just entered the Ballroom. There are a lot of people milling about, and as you walk around you realize you\'ve dropped an important item that you use frequently on campus to prove who you are. Don\'t leave without finding it, you will not be able to finish your quest without it.').
area(room3, 'The Bookstore', 'You just entered the Bookstore. You look around and see the aisles and aisles of school accessories, and realize that you may end up needing some sort of bag to take with you and store items.').
area(room4, 'The Marketplace', 'You just entered the Marketplace. The lights are dim, and people have slipped in to chill out after the blaring music in the Sunburst Lounge. If you are going to move forward on the quest, however, you may want to look around for some light.').
area(room5, 'The Hub', 'You have just entered the Hub, where many people are hanging out. You see some friends and go over to see what they\'re up to. After talking to them you see something on the ground that looks valuable. It may help you later when you get hungry. Also in the corner is an elevator that will take you up to the Sunburst Lounge.').
area(room6, 'Sunburst Lounge', 'The Sunburst Lounge is bouncin. There is a live band playing up front, and hundreds of people milling about, making it really difficult to be able to find where the key you will need later in the quest is hidden.').
area(room7, 'Quick Stop', 'You have entered the Quick Stop. All the partying and dancing has made you hungry. You step in here for a quick second to refuel.').
area(room8, 'Admissions Office', 'You are in the Admissions office. You look around and see a lot of desks, but because it is after hours nobody is there working. Don\'t forget to leave without the rest of your costume.').
area(room9, 'The Sky Room', 'You have reached the Sky Room! Congratulations on making it through the maze of rooms and for finding the key and the idcard. I hope you enjoyed playing \'Adventures at Utah State\'. Play again soon!').

% Place the items in rooms
placed(room1, passage).
placed(room2, idcard).
placed(room3, backpack).
placed(room4, flashlight).
placed(room4, soda).
placed(room5, money).
placed(room6, key).
placed(room6, door).
placed(room5, elevator).
placed(room7, candy).
placed(room7, drink).
placed(room8, mask).

% What you can pick up and what you cant
can_pickup(key).
can_pickup(backpack).
can_pickup(flashlight).
can_pickup(candy).
can_pickup(money).
can_pickup(idcard).
can_pickup(mask).

% Can eat an item
can_eat(candy).
can_eat(drink).

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

% This blocks the final room from the one possible way to reach it.
unblocked(room1, room4).
unblocked(room1, room2).
unblocked(room2, room3).
unblocked(room2, room5).
unblocked(room3, room2).
unblocked(room3, room6).
unblocked(room3, room6).
unblocked(room4, room1).
unblocked(room4, room7).
unblocked(room5, room4).
unblocked(room5, room8).
unblocked(room5, room8).
unblocked(room6, room3).
unblocked(room7, room4).
unblocked(room7, room8).
unblocked(room8, room5).
unblocked(room9, room6).

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
    print('Hope you enjoy!'), nl, nl, nl.

% Prints out the players current location description
print_location :-
    current_area(Current),
    area(Current, _, Description), write(Description), nl, nl. 

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
process_input([help]) :-
    print('Here are some clues for how to play the game: '), nl, nl,
    print('Type quest to start the game'), nl,
    print('Type eat candy to eat the candy. You will lose the candy from your inventory after eating it.'), nl,
    print('Type drink soda to drink a Dr. Pepper. You will lose the soda from your inventory after doing so.'), nl,
    print('Type turnon flashlight to turn the flashlight on to light your way.'), nl,
    print('Type open door to open a door. Make sure to have any items you will need to open the door first!'), nl,
    print('Type inventory to see what you have in your inventory.'), nl,
    print('Type contents to see what you have in your backpack.'), nl,
    print('Type drop followed by an item name to drop the item from your inventory.'), nl,
    print('Type fill_backpack followed by an item name to place the item in your backpack.'), nl,
    print('Type pickup followed by an item name to pick up an item. Not all items are able to be picked up.'), nl, 
    print('Type ride elevator to ride the elevator from the Hub to the Sunburst Lounge.'), nl,
    print('Type go followed by a direction (north, south, east, west) to change rooms. Some rooms only allow you to come in to them, and if you try to go out a wall it will tell you and prevent you.'), nl, nl,
    print('Hope these hints helped you! Good luck!'), nl, nl.

% Display the description of the quest
process_input([quest]):-
    print('You have started the quest! Your mission, should you choose to accept, is to find two items and make it to the Sky Room. The Sky Room is located on the far side of the Sunburst Lounge. The first item that you need to find is the idcard. Good luck!').

% Handling of the action 'eat _______'
process_input([eat, candy]):-
    have(candy),
    retract(have(candy)),
    print('You have eaten the candy bar. If you get hungry, go back to the Quick Stop to get more food!'), nl, nl.
process_input([eat, _]):-
    print('You must have the candy bar to eat it!'), nl, nl.

% Handling of the action 'drink _______'
process_input([drink, soda]):-
    have(soda),
    retract(have(soda)),
    print('You have drunk the Dr. Pepper. If you get thirsty, go back to the Quick Stop to get another!'), nl, nl.
process_input([eat, _]):-
    print('You must have the candy bar to eat it!'), nl, nl.

% Handling of the action 'turnon _______'
process_input([turnon, flashlight]):-
    have(flashlight),
    print('You have turned on the flashlight! It will now light your way through the dark rooms.'), nl, nl.
process_input([eat, _]):-
    print('You must have the flashlight to turn it on. Nice try though!'), nl, nl.

% Handling of the action 'open _______'
process_input([open, Item]):-
    have(key),
    have(idcard),
    current_area(room6),
    print('You have opened the door!'), nl, nl,
    change_area(room9).
process_input([open, _]):-
    print('You can\'t open the door, it is still locked. Make sure you have all the items you need and are in the correct room.'), nl, nl.

% Print out a players inventory
process_input([inventory]) :-
    findall(Item, have(Item), ItemList),
    print_inventory(ItemList).

% Print out a players backpack contents
process_input([contents]):-
    findall(Content, in_backpack(Content), ContentList),
    print_inventory(ContentList).

% Handling of the action 'drop ______'
process_input([drop, Item]):-
    have(Item),
    retract(have(Item)),
    item(Item, ItemName, _),
    print('You have dropped the '), write(ItemName), write('.'), nl, nl.
process_input([drop, _]):-
    print('You have nothing in your inventory by that name!'), nl, nl.

% Handling of the action 'fill_backpack ________'
process_input([fill_backpack, Item]):-
    have(backpack),
    have(Item),
    assertz(in_backpack, Item),
    item(Item, ItemName, Description),
    print('You placed the '), write(ItemName), print(' in your backpack.'), print(Description), nl, nl.
process_input([fill_backpack, _]):-
    print('You either don\'t have the backpack or have nothing to put in it!'), nl, nl.

% Handling of the action 'pickup idcard' and 'pickup key', and then 'pickup ______'
process_input([pickup, idcard]):-
    current_area(Current),
    placed(Current, idcard),
    assertz(have(idcard)),
    item(idcard, ItemName, Description),
    print('You picked up the '), write(ItemName), print('. Description: '), print(Description), nl, nl,
    print('You have finished the first quest. Now go on to the next quest!'), nl, nl.
process_input([pickup, key]):-
    have(idcard),
    current_area(Current),
    placed(Current, key),
    assertz(have(key)),
    item(key, ItemName, Description),
    print('You picked up the '), write(ItemName), print('. Description: '), print(Description), nl, nl,
    print('You have finished the second quest. Now go finish the game by finding the Sky Room!'), nl, nl.
process_input([pickup, Item]):-
    current_area(Current),
    placed(Current, Item),
    assertz(have(Item)),
    item(Item, ItemName, Description),
    print('You picked up a(n) '), write(ItemName), print('. Description: '), print(Description), nl, nl.
process_input([pickup, _]) :-
    print('There is nothing to pick up with that name. Sorry!'), nl, nl.

% Ride the elevator
process_input([ride, elevator]):-
    current_area(room5),
    print('You have used the elevator. Have fun!'), nl, nl,
    change_area(room6).
process_input([ride, _]):-
    print('The elevator is not in this room. Sorry!'), nl, nl.

% Handling of the action 'go _______', and a good example of how you might implement others
process_input([go, Direction]) :-
    current_area(Current),
    connected(Direction, Current, NewRoom),
    unblocked(Current, NewRoom),
    change_area(NewRoom).
process_input([go, _]) :-
    print('You can not go that way. You have either hit a wall, or there is something like a door blocking your way.'), nl, nl.
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