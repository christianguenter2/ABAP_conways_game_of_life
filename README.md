# ABAP_conways_game_of_life

Implementation of [Conways Game of Life](https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life) in ABAP. 

NW ABAP 7.50 required.

![demo](/assets/gol.gif?raw=true "demo")

To start execute report ZCG_CONWAY_VIEW

![Start screen](/assets/start_screen.jpg?raw=true "Start screen")

Then you can either execute turns manually (F8 or F9) or start simulation mode (Shift-F9).
In simulation mode you can either use a SAPGUI timer (option GUI) or the modern approach with AMC, APC and websocket (option WS). Although the latter might not work in every environment. You can montior websocket connections with transactoin SMWS.

![Init](/assets/gol_with_websocket_timer.jpg?raw=true "Init")

![After 10 turns](/assets/gol_after_10_turns.jpg?raw=true "After 10 turns")

![After 110 turns](/assets/gol_after_110_turns.jpg?raw=true "After 110 turns")
