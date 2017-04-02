class ZCL_CG_CONWAYS_GAME_OF_LIFE definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF ty_coordinate,
             col TYPE i,
             row TYPE i,
           END OF ty_coordinate .
  types:
    BEGIN OF ty_cell.
        INCLUDE TYPE ty_coordinate.
    TYPES: active TYPE abap_bool,
           END OF ty_cell .
  types:
    tty_coordinates TYPE HASHED TABLE OF ty_coordinate
                           WITH UNIQUE KEY col row .
  types:
    tty_cell        TYPE HASHED TABLE OF ty_cell
                    WITH UNIQUE KEY col row .

  data M_SIZE type I read-only .

  methods CONSTRUCTOR
    importing
      !SIZE type I .
  methods FILL_RANDOMLY
    importing
      !I_SEED type I optional
    returning
      value(R_BOARD) type ref to ZCL_CG_CONWAYS_GAME_OF_LIFE .
  methods TURN .
  methods GET_CELL
    importing
      !I_COL type I
      !I_ROW type I
    returning
      value(R_ACTIVE) type ABAP_BOOL .
  methods MULTIPLE_TURNS
    importing
      !I_COUNT type I .
  methods GET_TURNS
    returning
      value(R_TURN_COUNT) type I .
  methods GET_ACTIVE_CELLS
    returning
      value(R_CELLS_ALIVE) type I .
  PRIVATE SECTION.

    TYPES: BEGIN OF ty_neighbour.
        INCLUDE TYPE ty_coordinate.
    TYPES: neighbours TYPE tty_coordinates,
           END OF ty_neighbour,
           tty_neighbour TYPE HASHED TABLE OF ty_neighbour
                              WITH UNIQUE KEY col row.

    DATA:
      mt_cells             TYPE tty_cell,
      mt_neighbours_buffer TYPE tty_neighbour,
      m_turn_count         TYPE i.

    METHODS:
      activate_cell
        IMPORTING
          i_col TYPE i
          i_row TYPE i,

      get_active_neighbours_of_cell
        IMPORTING
          i_col                      TYPE i
          i_row                      TYPE i
        RETURNING
          VALUE(r_active_neighbours) TYPE i,

      get_neighbours_of_cell
        IMPORTING
          i_col               TYPE i
          i_row               TYPE i
        RETURNING
          VALUE(r_neighbours) TYPE tty_coordinates.

ENDCLASS.



CLASS ZCL_CG_CONWAYS_GAME_OF_LIFE IMPLEMENTATION.


  METHOD activate_cell.

    mt_cells[ col = i_col
              row = i_row ]-active = abap_true.

  ENDMETHOD.


  METHOD constructor.

    m_size = size.

    mt_cells = VALUE tty_cell( FOR col = 1 WHILE col <= m_size
                               FOR row = 1 WHILE row <= m_size
                               ( col    = col
                                 row    = row ) ).

  ENDMETHOD.


  METHOD fill_randomly.

    DATA(rnd) = cl_abap_random_int=>create( seed = COND #( WHEN i_seed IS SUPPLIED THEN i_seed
                                                           ELSE CONV #( sy-uzeit ) )
                                            min  = 0
                                            max  = 1 ).

    LOOP AT mt_cells ASSIGNING FIELD-SYMBOL(<cell>).

      <cell>-active = boolc( rnd->get_next( ) = 1 ).

    ENDLOOP.

    r_board = me.

  ENDMETHOD.


  METHOD get_active_cells.

    r_cells_alive = REDUCE #( INIT result = 0
                              FOR cell IN mt_cells
                              WHERE ( active = abap_true  )
                              NEXT result = result + 1 ).

  ENDMETHOD.


  METHOD get_active_neighbours_of_cell.

    DATA(neighbours) = get_neighbours_of_cell( i_col = i_col
                                               i_row = i_row ).

    r_active_neighbours = REDUCE #( INIT result = 0
                                    FOR neighbour IN neighbours
                                    NEXT result = COND #( WHEN mt_cells[ col = neighbour-col
                                                                         row = neighbour-row ]-active = abap_true
                                                          THEN result + 1
                                                          ELSE result ) ).

  ENDMETHOD.


  METHOD get_cell.

    r_active = mt_cells[ col = i_col
                         row = i_row ]-active.

  ENDMETHOD.


  METHOD get_neighbours_of_cell.

    ASSIGN mt_neighbours_buffer[ col = i_col
                                 row = i_row ] TO FIELD-SYMBOL(<neighbour>).
    IF sy-subrc = 0.

      r_neighbours = <neighbour>-neighbours.
      RETURN.

    ENDIF.

    r_neighbours = VALUE #( FOR cell IN mt_cells
                            WHERE  ( ( row = i_row + 1 AND col = i_col + 1 )
                                  OR ( row = i_row     AND col = i_col + 1 )
                                  OR ( row = i_row + 1 AND col = i_col     )
                                  OR ( row = i_row - 1 AND col = i_col - 1 )
                                  OR ( row = i_row     AND col = i_col - 1 )
                                  OR ( row = i_row - 1 AND col = i_col     )
                                  OR ( row = i_row + 1 AND col = i_col - 1 )
                                  OR ( row = i_row - 1 AND col = i_col + 1 ) )
                            ( col = cell-col row = cell-row ) ).

    INSERT VALUE #( col        = i_col
                    row        = i_row
                    neighbours = r_neighbours )
           INTO TABLE mt_neighbours_buffer.

  ENDMETHOD.


  METHOD get_turns.

    r_turn_count = m_turn_count.

  ENDMETHOD.


  METHOD multiple_turns.

    DO i_count TIMES.

      turn( ).

    ENDDO.

  ENDMETHOD.


  METHOD turn.

    m_turn_count = m_turn_count + 1.

    DATA(new_cells) = VALUE tty_cell( FOR cell IN mt_cells
                                      LET active_neighbours = get_active_neighbours_of_cell( i_col = cell-col
                                                                                             i_row = cell-row )
                                      IN ( col    = cell-col
                                           row    = cell-row
                                           active = COND #( WHEN active_neighbours = 3 THEN abap_true
                                                            WHEN active_neighbours = 2
                                                             AND cell-active      = abap_true THEN abap_true
                                                            ELSE abap_false ) ) ).

    mt_cells = new_cells.

  ENDMETHOD.
ENDCLASS.
