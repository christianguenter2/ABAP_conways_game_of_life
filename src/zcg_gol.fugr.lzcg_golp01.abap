CLASS websocket_timer IMPLEMENTATION.

  METHOD constructor.

    wait_in_seconds = i_wait_in_seconds.
    times = i_times.

    producer ?= cl_amc_channel_manager=>create_message_producer( i_application_id = zcl_apc_wsp_ext_zapc_wakeup=>c_amc_application_id
                                                                 i_channel_id     = zcl_apc_wsp_ext_zapc_wakeup=>c_channel_id ).

  ENDMETHOD.


  METHOD run.

    DO times TIMES.

      CALL FUNCTION 'ENQUE_SLEEP'
        EXPORTING
          seconds        = wait_in_seconds
        EXCEPTIONS
          system_failure = 1
          OTHERS         = 2.
      ASSERT sy-subrc = 0.

      producer->send( || ).

    ENDDO.

    result = me.

  ENDMETHOD.

ENDCLASS.
