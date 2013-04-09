PrologTextGame
"Adventures at Utah State University"
==============

This is a Prolog text based logic game. 
DISCLAIMER: I am not a creative person. This game is kind of lame, and the room descriptions are not super creative, but hopefully you can have fun while playing it and enjoy the simplicity of it.

This game is set in the Taggart Student Center here at USU. You start off in the Admissions Office and go from room to room. Each room has at least one item, and the room description gives a hint as to what items might be in there. The item list is this:

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

To get a hint during the game of how to play, you just have to type 'help' and some help text will come up. I will copy that text in here, so I don't have to rewrite everything that it already says. 

print('Here are some clues for how to play the game: '), nl, nl,
print('Type quest to start the game'), nl,
print('Type eat candy to eat the candy. You will lose the candy from your inventory after eating it.'), nl,
print('Type drink soda to drink a Dr. Pepper. You will lose the soda from your inventory after doing so.'), nl,
print('Type turnon flashlight to turn the flashlight on to light your way.'), nl,
print('Type open door to open a door. Make sure to have any items you will need to open the door first!'), nl,
print('Type enter passage to enter the secret passage to take you right to the end. You will need a certain item, and the passage is hidden!'), nl,
print('Type inventory to see what you have in your inventory.'), nl,
print('Type contents to see what you have in your backpack.'), nl,
print('Type drop followed by an item name to drop the item from your inventory.'), nl,
print('Type fill_backpack followed by an item name to place the item in your backpack.'), nl,
print('Type pickup followed by an item name to pick up an item. Not all items are able to be picked up.'), nl, 
print('Type ride elevator to ride the elevator from the Hub to the Sunburst Lounge.'), nl,
print('Type go followed by a direction (north, south, east, west) to change rooms. Some rooms only allow you to come in to them, and if you try to go out a wall it will tell you and prevent you.'), nl, nl,
print('Hope these hints helped you! Good luck!'), nl, nl.

For the quests, after typing quest, it will give you a small overview of the quests, and then send you off to get the first item, which is the idcard. After you get the idcard, it will tell you the next step, which is getting the key. You can not pick up the key if you don't have the idcard first. After getting the idcard and the key, you need to make your way to the Sunburst Lounge to find your way to the end of the quest, which is getting into the Sky Room. To do so, you have to type open door, because the door is in betwen you and the Sky Room. After opening the door it will take you into the Sky Room and the game is over. 

Hopefully this is a good overview of the game, and you should be able to figure it out. Attached is the pdf of the gameboard, with the names of all the rooms and where each of the items are. 

Enjoy!
