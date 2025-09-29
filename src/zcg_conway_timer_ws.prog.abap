*&---------------------------------------------------------------------*
*& Report z_test_send_amc
*&---------------------------------------------------------------------*
*& Gol Timer via Websocket
*&---------------------------------------------------------------------*
REPORT zcg_conway_timer_ws.

PARAMETERS: seconds TYPE i DEFAULT 1 OBLIGATORY,
            times   TYPE i DEFAULT 10 OBLIGATORY.

START-OF-SELECTION.
  CALL FUNCTION 'ZCG_CONWAY_TIMER_WS'
    IN BACKGROUND TASK
    DESTINATION 'NONE'
    EXPORTING
      i_wait_in_seconds = seconds
      i_times           = times
    EXCEPTIONS
      error             = 1
      OTHERS            = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
      DISPLAY LIKE sy-msgty
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    RETURN.
  ENDIF.

  COMMIT WORK.

  MESSAGE 'Timer started in background' TYPE 'S'.
