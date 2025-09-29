CLASS zcl_cg_conway_abap2ui5 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES:
      z2ui5_if_app.

    DATA:
      size  TYPE i,
      times TYPE i.

  PRIVATE SECTION.
    DATA:
      started TYPE abap_bool,
      gol     TYPE REF TO zcl_cg_conways_game_of_life,
      client  TYPE REF TO z2ui5_if_client.

    METHODS:
      render_grid
        IMPORTING
          layout TYPE REF TO z2ui5_cl_xml_view,

      render
        RETURNING
          VALUE(result) TYPE string.

ENDCLASS.


CLASS zcl_cg_conway_abap2ui5 IMPLEMENTATION.

  METHOD z2ui5_if_app~main.

    me->client = client.

    IF z2ui5_if_app~check_initialized = abap_false.
      size = 25.
      times = 10.
    ENDIF.

    CASE client->get( )-event.
      WHEN `GO`.

        started = abap_true.
        gol = NEW zcl_cg_conways_game_of_life( size ).
        gol->fill_randomly( ).

      WHEN `NEXT`.

        gol->turn( ).

      WHEN `MULTI`.

        gol->multiple_turns( times ).

      WHEN `TIMER`.

        " start websocket timer in background task (trfc)
        CALL FUNCTION 'ZCG_CONWAY_TIMER_WS'
          IN BACKGROUND TASK
          DESTINATION 'NONE'
          EXPORTING
            i_times = times
          EXCEPTIONS
            OTHERS  = 1.
        IF sy-subrc <> 0.
          client->message_box_display( type = 'Error' text = 'Error starting timer task' ).
          RETURN.
        ENDIF.

        client->message_toast_display( text = |Timer started for { times } turns| ).

        COMMIT WORK.

    ENDCASE.

    client->view_display( render( ) ).

  ENDMETHOD.


  METHOD render_grid.

    DO size TIMES.
      DATA(row) = sy-index.
      DATA(h) = layout->hbox( ).
      DO size TIMES.
        DATA(col) = sy-index.
        DATA(state) = gol->get_cell(
            i_col   = col
            i_row   = row ).
        IF state = abap_true.
          h->icon( src = `sap-icon://biometric-thumb` ).
        ELSE.
          h->icon( src = `sap-icon://to-be-reviewed` ).
        ENDIF.
      ENDDO.
    ENDDO.

  ENDMETHOD.


  METHOD render.

    DATA(view) = z2ui5_cl_xml_view=>factory( ).

    DATA(page) = view->shell(
                    )->page(
                       title          = `abap2UI5 - Conway's Game of Life`
                       navbuttonpress = client->_event( 'BACK' )
                       shownavbutton  = xsdbool( client->get( )-s_draft-id_prev_app_stack IS NOT INITIAL ) ).

    DATA(layout) = page->scroll_container( vertical = abap_true
                                           height   = `100%` ).
    DATA(fbox) = layout->flex_box( direction  = `Row`
                                   alignitems = `Start` ).

    DATA(form) = fbox->simple_form( editable = abap_true title = `Grid`
                    )->content( `form` ).

    form->label( text     = `size`
                 labelFor = `sizeInput`
       )->input( id          = `sizeInput`
                 placeholder = `size`
                 type        = `Number`
                 value       = client->_bind_edit( size )
       )->button( text = `Go` press = client->_event( `GO` ) ).

    IF started = abap_true.

      form = fbox->simple_form( editable = abap_true title = `Turn` class = `sapUiTinyMarginBegin`
                )->content( `form` ).
      form->button( text = `Single` press = client->_event( `NEXT` ) ).
      form->input( value = client->_bind_edit( times ) type = `Number` placeholder = `times` ).
      form->button( text = `Mutliple` press = client->_event( `MULTI` ) ).

      form = fbox->simple_form( editable = abap_true title = `Timer` class = `sapUiSmallMarginBegin`
                )->content( `form` ).
      form->input( value = client->_bind_edit( times ) type = `Number` placeholder = `times` ).
      form->button( text = `Run Timer` press = client->_event( `TIMER` ) ).

      render_grid( layout ).

    ENDIF.

    view->_generic( name = `script`
                    ns   = `html`
       )->_cc_plain_xml(
          `(()=>{ ` &&
          `  var ws_url = window.location.origin + '/sap/bc/apc/sap/zapc_wakeup';` &&
          `  var ws_url = ws_url.replace('http','ws');` &&
          `  try { ` &&
          `    ws = new WebSocket(ws_url);` &&
          `  } catch (err) {` &&
          `    alert(err);` &&
          `  }` &&
          `  ws.onopen = ()=>{};` &&
          `  ws.onmessage = (msg)=>{` &&
          `    if (msg.data == '__CLOSE__') {` &&
          `      ws.close();` &&
          `      return;` &&
          `    }` &&
          `    z2ui5.oController.eB(['NEXT']);` &&
          `  };` &&
          `  ws.onclose = (msg)=>{};` &&
          `})()` ).

    result = view->stringify( ).

  ENDMETHOD.

ENDCLASS.
