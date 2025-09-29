CLASS zcl_cg_conways_game_of_life DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_coordinate,
        col TYPE i,
        row TYPE i,
      END OF ty_coordinate.

    TYPES:
      BEGIN OF ty_cell.
        INCLUDE TYPE ty_coordinate.
    TYPES: alive TYPE abap_bool,
           END OF ty_cell.

    TYPES:
      tty_coordinates TYPE HASHED TABLE OF ty_coordinate
                           WITH UNIQUE KEY col row.

    TYPES:
      tty_cell TYPE HASHED TABLE OF ty_cell
                    WITH UNIQUE KEY col row
                    WITH NON-UNIQUE SORTED KEY key_alive
                         COMPONENTS alive.

    DATA: m_size TYPE i READ-ONLY.

    METHODS:
      constructor
        IMPORTING
          !size TYPE i,

      fill_randomly
        IMPORTING
          !i_seed        TYPE i OPTIONAL
        RETURNING
          VALUE(r_board) TYPE REF TO zcl_cg_conways_game_of_life,

      turn,

      get_cell
        IMPORTING
          !i_col         TYPE i
          !i_row         TYPE i
        RETURNING
          VALUE(r_alive) TYPE abap_bool,

      multiple_turns
        IMPORTING
          !i_count TYPE i,

      get_turns
        RETURNING
          VALUE(r_turn_count) TYPE i,

      get_alive_cells_count
        RETURNING
          VALUE(r_cells_alive) TYPE i .

  PRIVATE SECTION.

    TYPES: BEGIN OF ty_neighbour.
        INCLUDE TYPE ty_coordinate.
    TYPES: neighbours_coordinates TYPE tty_coordinates,
           END OF ty_neighbour,
           tty_neighbour TYPE HASHED TABLE OF ty_neighbour
                              WITH UNIQUE KEY col row.

    DATA:
      cells             TYPE tty_cell,
      neighbours_buffer TYPE tty_neighbour,
      turn_count        TYPE i.

    METHODS:
      activate_cell
        IMPORTING
          i_col TYPE i
          i_row TYPE i,

      get_alive_neighbours_of_cell
        IMPORTING
          i_col                     TYPE i
          i_row                     TYPE i
        RETURNING
          VALUE(r_alive_neighbours) TYPE i,

      get_neighbours_of_cell
        IMPORTING
          i_col               TYPE i
          i_row               TYPE i
        RETURNING
          VALUE(r_neighbours) TYPE tty_cell,

      _execute_rule
        IMPORTING
          i_cell_alive        TYPE abap_bool
          i_alive_neighbours  TYPE i
        RETURNING
          VALUE(r_cell_alive) TYPE abap_bool.

ENDCLASS.



CLASS zcl_cg_conways_game_of_life IMPLEMENTATION.


  METHOD activate_cell.

    cells[ col = i_col
           row = i_row ]-alive = abap_true.

  ENDMETHOD.


  METHOD constructor.

    m_size = size.

    cells = VALUE tty_cell( FOR col = 1 WHILE col <= m_size
                            FOR row = 1 WHILE row <= m_size
                            ( col = col
                              row = row ) ).

  ENDMETHOD.


  METHOD fill_randomly.

    DATA(rnd) = cl_abap_random_int=>create( seed = COND #( WHEN i_seed IS SUPPLIED THEN i_seed
                                                           ELSE CONV #( sy-uzeit ) )
                                            min  = 0
                                            max  = 1 ).

    LOOP AT cells ASSIGNING FIELD-SYMBOL(<cell>).

      <cell>-alive = boolc( rnd->get_next( ) = 1 ).

    ENDLOOP.

    r_board = me.

  ENDMETHOD.


  METHOD get_alive_cells_count.

    r_cells_alive = lines( FILTER #( cells USING KEY key_alive
                                           WHERE alive = abap_true  ) ).

  ENDMETHOD.


  METHOD get_alive_neighbours_of_cell.

    r_alive_neighbours = lines( FILTER #( get_neighbours_of_cell( i_col = i_col
                                                                  i_row = i_row )
                                USING KEY key_alive
                                WHERE alive = abap_true  ) ).


  ENDMETHOD.


  METHOD get_cell.

    r_alive = cells[ col = i_col
                     row = i_row ]-alive.

  ENDMETHOD.


  METHOD get_neighbours_of_cell.

    DATA: neighbours_coordinates TYPE zcl_cg_conways_game_of_life=>ty_neighbour-neighbours_coordinates.

    ASSIGN neighbours_buffer[ col = i_col
                              row = i_row ] TO FIELD-SYMBOL(<neighbour>).
    IF sy-subrc = 0.

      neighbours_coordinates = <neighbour>-neighbours_coordinates.

    ELSE.

      neighbours_coordinates = VALUE #( FOR cell IN cells
                               WHERE ( ( row = i_row + 1 AND col = i_col + 1 )
                                    OR ( row = i_row     AND col = i_col + 1 )
                                    OR ( row = i_row + 1 AND col = i_col     )
                                    OR ( row = i_row - 1 AND col = i_col - 1 )
                                    OR ( row = i_row     AND col = i_col - 1 )
                                    OR ( row = i_row - 1 AND col = i_col     )
                                    OR ( row = i_row + 1 AND col = i_col - 1 )
                                    OR ( row = i_row - 1 AND col = i_col + 1 ) )
                               (  col = cell-col row = cell-row  ) ).

      INSERT VALUE #( col 									 = i_col
                      row 									 = i_row
                      neighbours_coordinates = neighbours_coordinates )
             INTO TABLE neighbours_buffer.

    ENDIF.

    r_neighbours = VALUE #( FOR neighbour IN neighbours_coordinates
                            ( cells[ row = neighbour-row
                                     col = neighbour-col ] ) ).

  ENDMETHOD.


  METHOD get_turns.

    r_turn_count = turn_count.

  ENDMETHOD.


  METHOD multiple_turns.

    DO i_count TIMES.

      turn( ).

    ENDDO.

  ENDMETHOD.


  METHOD turn.

    turn_count = turn_count + 1.

    DATA(new_cells) = VALUE tty_cell( FOR cell IN cells
                                      LET alive_neighbours = get_alive_neighbours_of_cell( i_col = cell-col
                                                                                           i_row = cell-row )
                                      IN ( col    = cell-col
                                           row    = cell-row
                                           alive  = _execute_rule( i_cell_alive       = cell-alive
                                                                   i_alive_neighbours = alive_neighbours ) ) ).

    cells = new_cells.

  ENDMETHOD.


  METHOD _execute_rule.

    r_cell_alive =  COND #( WHEN i_alive_neighbours = 3 														 THEN abap_true
                            WHEN i_alive_neighbours = 2 AND i_cell_alive = abap_true THEN abap_true ).

  ENDMETHOD.
ENDCLASS.
