*&---------------------------------------------------------------------*
*& Report z_test_conway_view
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_conway_view.

PARAMETERS: size TYPE i DEFAULT 5 OBLIGATORY.

SELECTION-SCREEN BEGIN OF SCREEN 2000.
PARAMETERS: count TYPE i DEFAULT 100 OBLIGATORY.
SELECTION-SCREEN END OF SCREEN 2000.

SELECTION-SCREEN BEGIN OF SCREEN 3000.
SELECTION-SCREEN INCLUDE PARAMETERS count.
PARAMETERS: interval TYPE p LENGTH 2 DECIMALS 2 DEFAULT 1 OBLIGATORY.
SELECTION-SCREEN END OF SCREEN 3000.

CLASS lcx_error DEFINITION
                INHERITING FROM cx_static_check.

  PUBLIC SECTION.
    CLASS-METHODS:
      raise_syst
        RAISING
          lcx_error,

      raise_exception
        IMPORTING
          i_previous TYPE REF TO cx_root
        RAISING
          lcx_error,

      raise_text
        IMPORTING
          i_text TYPE string
        RAISING
          lcx_error.

    METHODS:
      constructor
        IMPORTING
          textid   LIKE textid OPTIONAL
          previous LIKE previous OPTIONAL
          msg      TYPE symsg OPTIONAL
          text     TYPE csequence OPTIONAL,

      get_text REDEFINITION.

  PRIVATE SECTION.
    DATA: m_msg  TYPE symsg,
          m_text TYPE string.

    METHODS:
      _get_msg_text
        RETURNING VALUE(r_msg_text) TYPE string.

ENDCLASS.

CLASS conway_view DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.

    CLASS-DATA: okcode TYPE syucomm.

    METHODS:
      constructor
        IMPORTING
          i_board TYPE REF TO zcl_cg_conways_game_of_life,

      display,

      pai_0100,

      pbo_0100.

    EVENTS:
      function_code_pressed
        EXPORTING
          VALUE(e_fcode) TYPE sy-ucomm.

  PRIVATE SECTION.

    TYPES:
      tty_html TYPE STANDARD TABLE OF char255
                    WITH NON-UNIQUE DEFAULT KEY.

    DATA:
      m_board             TYPE REF TO zcl_cg_conways_game_of_life,
      mr_table            TYPE REF TO data,
      struct_descr        TYPE REF TO cl_abap_structdescr,
      alv                 TYPE REF TO cl_salv_table,
      m_docking_container TYPE REF TO cl_gui_docking_container,
      m_html_container    TYPE REF TO cl_gui_html_viewer.

    METHODS:
      _fill_board
        CHANGING
          ct_table TYPE INDEX TABLE,

      _display_alv
        CHANGING
          ct_table TYPE INDEX TABLE
        RAISING
          lcx_error,
      _display_docking_top
        RAISING
          lcx_error,

      _get_html
        RETURNING
          VALUE(rt_html) TYPE tty_html,

      _resize_columns
        RAISING
          cx_salv_not_found,
      _set_status.

ENDCLASS.

CLASS controller DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS:
      get_instance
        RETURNING
          VALUE(r_instance) TYPE REF TO controller.

    DATA: m_view  TYPE REF TO conway_view READ-ONLY.

    METHODS:
      start
        IMPORTING
          i_size TYPE i,
      is_timer_active
        RETURNING
          VALUE(r_is_timer_active) TYPE abap_bool.

  PRIVATE SECTION.
    CLASS-DATA: _instance TYPE REF TO controller.

    DATA: m_board         TYPE REF TO zcl_cg_conways_game_of_life,
          m_timer         TYPE REF TO cl_gui_timer,
          m_timer_current TYPE i,
          m_timer_max     TYPE i.

    METHODS:
      _dispatch_fcode FOR EVENT function_code_pressed OF conway_view
        IMPORTING
            e_fcode,

      _refresh,

      _start_simulation,

      _handler_timer_finished
        FOR EVENT finished OF cl_gui_timer,

      _stop_simulation
        RAISING
          lcx_error,

      __dispatch_fcode
        IMPORTING
          i_fcode TYPE sy-ucomm
        RAISING
          lcx_error.

ENDCLASS.

CLASS lcx_error IMPLEMENTATION.

  METHOD constructor.

    super->constructor( textid = textid previous = previous ).
    m_msg  = msg.
    m_text = text.

  ENDMETHOD.

  METHOD raise_syst.

    RAISE EXCEPTION TYPE lcx_error
      EXPORTING
        msg = VALUE #( msgty  = sy-msgty
                       msgid  = sy-msgid
                       msgno  = sy-msgno
                       msgv1  = sy-msgv1
                       msgv2  = sy-msgv2
                       msgv3  = sy-msgv3
                       msgv4  = sy-msgv4 ).

  ENDMETHOD.

  METHOD get_text.

    result = COND #( WHEN m_msg  IS NOT INITIAL THEN _get_msg_text(  )
                     WHEN m_text IS NOT INITIAL THEN m_text
                     ELSE super->get_text( ) ).

  ENDMETHOD.

  METHOD _get_msg_text.

    MESSAGE ID m_msg-msgid TYPE m_msg-msgty NUMBER m_msg-msgno
            WITH m_msg-msgv1 m_msg-msgv2 m_msg-msgv3 m_msg-msgv4
            INTO r_msg_text.

  ENDMETHOD.

  METHOD raise_exception.

    RAISE EXCEPTION TYPE lcx_error
      EXPORTING
        previous = i_previous.

  ENDMETHOD.

  METHOD raise_text.

    RAISE EXCEPTION TYPE lcx_error
      EXPORTING
        text = i_text.

  ENDMETHOD.

ENDCLASS.

CLASS conway_view IMPLEMENTATION.

  METHOD constructor.

    m_board = i_board.

    struct_descr = cl_abap_structdescr=>create(
                        VALUE #( LET type = CAST cl_abap_datadescr( cl_abap_typedescr=>describe_by_name( |ICON_D| ) )
                                 IN
                                 FOR x = 1 WHILE x <= m_board->m_size
                                 ( name = |COL_{ x }|
                                   type = type ) ) ).

    DATA(table_descr) = cl_abap_tabledescr=>create( struct_descr ).

    CREATE DATA mr_table TYPE HANDLE table_descr.
    ASSERT mr_table IS BOUND.

  ENDMETHOD.

  METHOD display.

    CALL SCREEN 0100.

  ENDMETHOD.

  METHOD _fill_board.

    DATA: lr_line TYPE REF TO data.

    CLEAR ct_table.

    DO m_board->m_size TIMES.

      CREATE DATA lr_line TYPE HANDLE struct_descr.
      ASSIGN lr_line->* TO FIELD-SYMBOL(<line>).

      DATA(row) = sy-index.

      DO m_board->m_size TIMES.

        DATA(col) = sy-index.

        ASSIGN COMPONENT |COL_{ col }|
               OF STRUCTURE <line>
               TO FIELD-SYMBOL(<value>).
        ASSERT sy-subrc = 0.

        <value> = COND #( WHEN m_board->get_cell( i_col = col
                                                  i_row = row ) = abap_true
                          THEN icon_modify ).

      ENDDO.

      INSERT <line> INTO TABLE ct_table.

    ENDDO.

  ENDMETHOD.

  METHOD _display_alv.

    TRY.

        IF alv IS NOT BOUND.

          cl_salv_table=>factory(
            EXPORTING
              r_container    = cl_gui_container=>screen0
            IMPORTING
              r_salv_table   = alv
            CHANGING
              t_table        = ct_table ).

          _resize_columns( ).

          alv->display( ).

        ELSE.

          alv->set_data(
            CHANGING
              t_table = ct_table ).

          _resize_columns( ).

          alv->refresh(  ).

        ENDIF.

      CATCH cx_salv_error INTO DATA(error).
        lcx_error=>raise_exception( error ).
    ENDTRY.

  ENDMETHOD.

  METHOD _resize_columns.

    DO m_board->m_size TIMES.

      alv->get_columns( )->get_column( |COL_{ sy-index }| )->set_output_length( 2 ).

    ENDDO.

  ENDMETHOD.

  METHOD pbo_0100.

    _set_status( ).

    SET TITLEBAR 'CONWAY'.

    ASSIGN mr_table->* TO FIELD-SYMBOL(<table>).
    ASSERT sy-subrc = 0.

    TRY.
        _fill_board(  CHANGING ct_table = <table> ).
        _display_docking_top( ).
        _display_alv( CHANGING ct_table = <table> ).

      CATCH lcx_error INTO DATA(error).
        MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.

  METHOD pai_0100.

    DATA(save_ok_code) = okcode.

    CLEAR okcode.

    RAISE EVENT function_code_pressed
      EXPORTING
        e_fcode = save_ok_code.

  ENDMETHOD.

  METHOD _display_docking_top.

    DATA: assigned_url TYPE c LENGTH 255.

    IF m_docking_container IS NOT BOUND.

      m_docking_container = NEW cl_gui_docking_container( side      = cl_gui_docking_container=>dock_at_top
                                                          extension = 30 ).

      m_html_container = NEW cl_gui_html_viewer( parent = m_docking_container ).

    ENDIF.

    DATA(html_table) = _get_html( ).

    m_html_container->load_data(
      IMPORTING
        assigned_url = assigned_url    " URL
      CHANGING
        data_table   = html_table
      EXCEPTIONS
        OTHERS       = 5 ).

    IF sy-subrc <> 0.
      lcx_error=>raise_syst( ).
    ENDIF.

    m_html_container->show_url(
      EXPORTING
        url    = assigned_url
      EXCEPTIONS
        OTHERS = 5 ).

    IF sy-subrc <> 0.
      lcx_error=>raise_syst( ).
    ENDIF.

  ENDMETHOD.

  METHOD _get_html.

    rt_html = VALUE #( ( '<html>'  )
                       ( '<body>'  )
                       ( '<font size="4" face="Arial,MS Sans Serif">'  )
                       ( '<h2>Turns: ' && | { m_board->get_turns( ) }| )
                       ( |  Cells alive: { m_board->get_active_cells( ) }| && '</h2>'  )
                       ( '</body>' )
                       ( '</html>' ) ).

  ENDMETHOD.

  METHOD _set_status.

    DATA: lt_excluding TYPE STANDARD TABLE OF char20
                            WITH NON-UNIQUE DEFAULT KEY.

    IF NOT controller=>get_instance( )->is_timer_active( ).
      INSERT CONV #( 'STOP' ) INTO TABLE lt_excluding.
    ENDIF.

    SET PF-STATUS 'STATUS_0100' EXCLUDING lt_excluding.

  ENDMETHOD.

ENDCLASS.

CLASS controller IMPLEMENTATION.

  METHOD get_instance.

    IF _instance IS NOT BOUND.

      _instance = NEW controller( ).

    ENDIF.

    r_instance = _instance.

  ENDMETHOD.

  METHOD start.

    m_board = NEW zcl_cg_conways_game_of_life( size = i_size )->fill_randomly( ).
    m_view  = NEW conway_view( m_board ).

    SET HANDLER _dispatch_fcode FOR m_view.

    m_view->display( ).

  ENDMETHOD.

  METHOD _dispatch_fcode.

    TRY.
        __dispatch_fcode( e_fcode ).

      CATCH lcx_error INTO DATA(error).
        MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.

  METHOD _refresh.

    cl_gui_cfw=>set_new_ok_code(
      EXPORTING
        new_code = 'REFRESH' ).

  ENDMETHOD.

  METHOD _start_simulation.

    IF m_timer IS NOT BOUND.

      m_timer = NEW cl_gui_timer( ).

      SET HANDLER _handler_timer_finished FOR m_timer.

    ENDIF.

    CALL SELECTION-SCREEN 3000 STARTING AT 5 5.
    CHECK sy-subrc = 0.

    m_timer_current = 0.
    m_timer_max     = count.

    m_timer->interval = interval.
    m_timer->run( ).

  ENDMETHOD.

  METHOD _stop_simulation.

    m_timer_current = m_timer_max.

    m_timer->cancel(  ).

    IF sy-subrc <> 0.
      lcx_error=>raise_syst( ).
    ENDIF.

  ENDMETHOD.

  METHOD _handler_timer_finished.

    m_timer_current = m_timer_current + 1.

    m_board->turn( ).
    _refresh( ).

    IF m_timer_current < m_timer_max.

      m_timer->run( ).

    ENDIF.

  ENDMETHOD.

  METHOD is_timer_active.

    r_is_timer_active = boolc( m_timer_current < m_timer_max ).

  ENDMETHOD.

  METHOD __dispatch_fcode.

    CASE i_fcode.
      WHEN 'BACK'
      OR   'EXIT'
      OR   'CANC'.

        SET SCREEN 0.

      WHEN 'NEXT'.

        m_board->turn( ).
        _refresh( ).

      WHEN 'N_TURN'.

        CALL SELECTION-SCREEN 2000 STARTING AT 5 5.
        IF sy-subrc = 0.

          m_board->multiple_turns( count ).
          _refresh( ).

        ENDIF.

      WHEN 'SIM'.

        _start_simulation( ).

      WHEN 'STOP'.

        _stop_simulation( ).

      WHEN 'REFRESH'.

      WHEN OTHERS.

        MESSAGE |Fcode { i_fcode } not implemented...| TYPE 'I'.

    ENDCASE.

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  controller=>get_instance( )->start( size ).

MODULE pbo_0100 OUTPUT.
  controller=>get_instance( )->m_view->pbo_0100( ).
ENDMODULE.

MODULE pai_0100 INPUT.
  controller=>get_instance( )->m_view->pai_0100( ).
ENDMODULE.
