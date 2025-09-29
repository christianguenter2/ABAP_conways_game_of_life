FUNCTION zcg_conway_timer_ws.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_WAIT_IN_SECONDS) TYPE  I DEFAULT 1
*"     VALUE(I_TIMES) TYPE  I DEFAULT 10
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------
  TRY.
      NEW websocket_timer( i_wait_in_seconds = i_wait_in_seconds
                           i_times           = i_times
        )->run( ).

    CATCH cx_amc_error INTO DATA(lx_error).
      MESSAGE lx_error->get_text( ) TYPE 'E' RAISING error.
  ENDTRY.

ENDFUNCTION.
