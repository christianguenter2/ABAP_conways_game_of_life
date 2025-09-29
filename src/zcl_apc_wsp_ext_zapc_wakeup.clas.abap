CLASS zcl_apc_wsp_ext_zapc_wakeup DEFINITION
  PUBLIC
  INHERITING FROM cl_apc_wsp_ext_stateless_base
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS:
      if_apc_wsp_extension~on_message REDEFINITION,
      if_apc_wsp_extension~on_start REDEFINITION.

    CONSTANTS:
      c_amc_application_id TYPE amc_application_id VALUE 'ZAMC_WAKEUP' ##NO_TEXT,
      c_channel_id         TYPE amc_channel_id VALUE '/channel' ##NO_TEXT.

ENDCLASS.



CLASS zcl_apc_wsp_ext_zapc_wakeup IMPLEMENTATION.

  METHOD if_apc_wsp_extension~on_message.

  ENDMETHOD.


  METHOD if_apc_wsp_extension~on_start.

    TRY.

        i_context->get_binding_manager(
                )->bind_amc_message_consumer( i_application_id = c_amc_application_id
                                              i_channel_id     = c_channel_id ).

      CATCH cx_apc_error INTO DATA(apc_error).

    ENDTRY.

  ENDMETHOD.

ENDCLASS.
