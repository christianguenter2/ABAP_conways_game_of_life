*&---------------------------------------------------------------------*
*& Report z_test_send_amc
*&---------------------------------------------------------------------*
*& Gol Timer via Websocket
*&---------------------------------------------------------------------*
REPORT zcg_conway_timer_ws.

PARAMETERS: mil_sec TYPE i DEFAULT 1000 OBLIGATORY,
            times   TYPE i DEFAULT 10 OBLIGATORY.

CLASS send_amc DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      constructor
        RAISING
          cx_amc_error,

      start
        RAISING
          cx_amc_error.

  PRIVATE SECTION.
    DATA:
      _producer TYPE REF TO if_amc_message_producer_text.

    METHODS:
      _wait_n_milliseconds
        IMPORTING
          i_milliseconds TYPE i.

ENDCLASS.

CLASS send_amc IMPLEMENTATION.

  METHOD constructor.

    _producer ?= cl_amc_channel_manager=>create_message_producer( i_application_id = 'ZAMC_WAKEUP'
                                                                  i_channel_id     = '/channel' ).

  ENDMETHOD.

  METHOD start.

    DO times TIMES.

      _wait_n_milliseconds( mil_sec ).

      _producer->send( || ).

    ENDDO.

  ENDMETHOD.


  METHOD _wait_n_milliseconds.

    DATA: ts1 TYPE timestampl,
          ts2 TYPE timestampl,
          ts3 TYPE timestampl.

    DATA(seconds) = i_milliseconds / 1000.

    GET TIME STAMP FIELD ts1.

    DO.

      GET TIME STAMP FIELD ts2.
      ts3 = ts2 - ts1.
      IF ts3 > seconds.
        EXIT.
      ENDIF.

    ENDDO.

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  TRY.
      NEW send_amc( )->start( ).

    CATCH cx_amc_error INTO DATA(error).
      MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
  ENDTRY.
