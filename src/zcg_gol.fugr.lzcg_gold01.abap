CLASS websocket_timer DEFINITION.

  PUBLIC SECTION.
    METHODS: constructor
        IMPORTING
          i_wait_in_seconds TYPE i DEFAULT 1
          i_times           TYPE i DEFAULT 10
        RAISING
          cx_amc_error,

      run
        RETURNING
          VALUE(result) TYPE REF TO websocket_timer
        RAISING
          cx_amc_error.


  PRIVATE SECTION.
    DATA:
      wait_in_seconds TYPE i,
      times           TYPE i,
      producer        TYPE REF TO if_amc_message_producer_text.

ENDCLASS.
