% Use this dynamic fact to add to the inventory
:- dynamic have/1.

% These are the items they can pick up
item(key, 'Gold key to open doors').

% Print out a players inventory
process_input([inventory]) :-
    print(findall(X, have(X), InventoryList)), nl, nl.

% Handling of the action 'pickup _______'
process_input([pickup, Item]):-
    print('You picked up an item.'), nl, nl.
    assertz(have(Item)).
process_input([pickup, _]) :-
    print('There is nothing to pick up. Sorry!'), nl, nl.
