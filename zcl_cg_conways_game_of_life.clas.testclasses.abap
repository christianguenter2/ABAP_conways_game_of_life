*"* use this source file for your ABAP unit test classes

CLASS test_conway DEFINITION DEFERRED.

CLASS zcl_cg_conways_game_of_life DEFINITION LOCAL FRIENDS test_conway.

CLASS test_conway DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    TYPES: tty_values TYPE STANDARD TABLE OF char1
                           WITH NON-UNIQUE EMPTY KEY.

    DATA: m_board      TYPE REF TO zcl_cg_conways_game_of_life,
          m_row_count  TYPE i,
          m_given_rows TYPE i.

    METHODS:
      init_board								 FOR TESTING RAISING cx_static_check,
      fill_randomly 						 FOR TESTING RAISING cx_static_check,
      less_than_two_neighbours	 FOR TESTING RAISING cx_static_check,
      two_or_three_neighbours 	 FOR TESTING RAISING cx_static_check,
      two_neighbours						 FOR TESTING RAISING cx_static_check,
      more_than_three_neighbours FOR TESTING RAISING cx_static_check,
      exactly_three_neighbours	 FOR TESTING RAISING cx_static_check,
      turn_with_two_active_cells FOR TESTING RAISING cx_static_check,
      two_consecutive_turns 		 FOR TESTING RAISING cx_static_check,
      turn_count								 FOR TESTING RAISING cx_static_check,
      cells_alive 							 FOR TESTING RAISING cx_static_check,
      large_board 							 FOR TESTING RAISING cx_static_check,

      _given_board_is
        IMPORTING
          given_row TYPE string,

      _when_turn_is_executed,

      _when_mult_turns_are_executed
        IMPORTING
          turns_to_execute TYPE i,

      _then_cells_alive_should_be
        IMPORTING
          i_exp_cells_alive TYPE i,

      _then_board_should_be
        IMPORTING
          exp_val TYPE string,

      _split_row_string
        IMPORTING
          i_row_string     TYPE string
        RETURNING
          VALUE(rt_values) TYPE tty_values.

ENDCLASS.

CLASS test_conway IMPLEMENTATION.

  METHOD _when_turn_is_executed.

    m_board->turn( ).

  ENDMETHOD.

  METHOD _when_mult_turns_are_executed.

    m_board->multiple_turns( turns_to_execute ).

  ENDMETHOD.

  METHOD _then_cells_alive_should_be.

    cl_abap_unit_assert=>assert_equals( act = m_board->get_active_cells( )
                                        exp = i_exp_cells_alive ).

  ENDMETHOD.

  METHOD _given_board_is.

    m_given_rows = m_given_rows + 1.

    DATA(values) = _split_row_string( given_row ).

    IF m_board IS NOT BOUND.

      m_board = NEW zcl_cg_conways_game_of_life( size = lines( values ) ).

    ENDIF.

    LOOP AT values ASSIGNING FIELD-SYMBOL(<value>).

      DATA(col) = sy-tabix.

      CHECK <value> = abap_true.

      m_board->activate_cell( i_col = col
                              i_row = m_given_rows ).

    ENDLOOP.

  ENDMETHOD.

  METHOD _then_board_should_be.

    IF m_row_count >= m_given_rows.

      CLEAR: m_row_count.

    ENDIF.

    m_row_count = m_row_count + 1.

    DATA(values) = _split_row_string( exp_val ).

    LOOP AT values ASSIGNING FIELD-SYMBOL(<value>).

      DATA(col) = sy-tabix.

      cl_abap_unit_assert=>assert_equals( act = m_board->get_cell( i_col = col
                                                                   i_row = m_row_count )
                                          exp = <value>
                                          msg = |Col { col }, Row { m_row_count }| ).

    ENDLOOP.

  ENDMETHOD.

  METHOD _split_row_string.

    SPLIT i_row_string AT `|` INTO TABLE rt_values.

    DELETE rt_values INDEX 1.

  ENDMETHOD.

  METHOD init_board.

    _given_board_is(: `| | | |` ),
                      `| | | |` ),
                      `| | | |` ).

    _then_board_should_be(: `| | | |` ),
													  `| | | |` ),
													  `| | | |` ).

  ENDMETHOD.

  METHOD fill_randomly.

    _given_board_is(: `| | | |` ),
                      `| | | |` ),
                      `| | | |` ).

    m_board->fill_randomly( i_seed = 1234 ).

    _then_board_should_be(: `| |X|X|` ),
													  `| | |X|` ),
													  `|X|X|X|` ).

  ENDMETHOD.

  METHOD less_than_two_neighbours.

    _given_board_is(: `| | | |` ),
                      `| |X| |` ),
                      `| | | |` ).

    _when_turn_is_executed( ).

    _then_board_should_be(: `| | | |` ),
													  `| | | |` ),
													  `| | | |` ).

  ENDMETHOD.

  METHOD two_or_three_neighbours.

    _given_board_is(: `| | | |` ),
                      `|X|X| |` ),
                      `| |X|X|` ).

    _when_turn_is_executed( ).


    _then_board_should_be(: `| | | |` ),
													  `|X|X|X|` ),
													  `|X|X|X|` ).

  ENDMETHOD.

  METHOD two_neighbours.

    _given_board_is(: `| | | |` ),
                      `|X|X| |` ),
                      `| | |X|` ).

    _when_turn_is_executed( ).

    _then_board_should_be(: `| | | |` ),
													  `| |X| |` ),
													  `| |X| |` ).

  ENDMETHOD.

  METHOD more_than_three_neighbours.

    _given_board_is(: `| | |X|` ),
                      `|X|X| |` ),
                      `| |X|X|` ).

    _when_turn_is_executed( ).

    _then_board_should_be(: `| |X| |` ),
													  `|X| | |` ),
													  `|X|X|X|` ).

  ENDMETHOD.

  METHOD exactly_three_neighbours.

    _given_board_is(: `| | | |` ),
                      `| |X| |` ),
                      `| |X|X|` ).

    _when_turn_is_executed( ).

    _then_board_should_be(: `| | | |` ),
													  `| |X|X|` ),
													  `| |X|X|` ).

  ENDMETHOD.

  METHOD turn_with_two_active_cells.

    _given_board_is(: `| | | |` ),
                      `| | | |` ),
                      `| |X|X|` ).

    _when_turn_is_executed( ).

    _then_board_should_be(: `| | | |` ),
													  `| | | |` ),
													  `| | | |` ).

  ENDMETHOD.

  METHOD two_consecutive_turns.

    _given_board_is(: `| |X|X|` ),
                      `| | | |` ),
                      `| | |X|` ).

    _when_mult_turns_are_executed( turns_to_execute = 2 ).

    _then_board_should_be(: `| | | |` ),
													  `| | | |` ),
													  `| | | |` ).

  ENDMETHOD.

  METHOD turn_count.

    _given_board_is( `| |` ).

    _when_mult_turns_are_executed( turns_to_execute = 17 ).

    cl_abap_unit_assert=>assert_equals( act = m_board->get_turns( )
                                        exp = 17 ).


  ENDMETHOD.

  METHOD cells_alive.

    _given_board_is(: `| |X|X|` ),
                      `|X| | |` ),
                      `|X| |X|` ).

    _then_cells_alive_should_be( 5 ).

    _when_turn_is_executed( ).

    _then_cells_alive_should_be( 4 ).

    _when_turn_is_executed( ).

    _then_cells_alive_should_be( 4 ).

  ENDMETHOD.

  METHOD large_board.

    _given_board_is(: `| | | | | |` ),
                      `| | | | | |` ),
                      `| | | | | |` ),
                      `| | | | | |` ),
                      `| | | | | |` ).

    m_board->fill_randomly( i_seed = 789 ).

    _then_board_should_be(: `| | |X|X| |` ),
													  `| |X| | | |` ),
													  `| |X| | |X|` ),
													  `| |X|X|X|X|` ),
													  `|X|X| | |X|` ).

    _when_turn_is_executed( ).

    _then_board_should_be(: `| | |X| | |` ),
													  `| |X| |X| |` ),
													  `|X|X| | |X|` ),
													  `| | | | |X|` ),
													  `|X|X| | |X|` ).

  ENDMETHOD.

ENDCLASS.
