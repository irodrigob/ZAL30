*&---------------------------------------------------------------------*
*&  Include           ZAL30_MAIN_VIEW_C01
*&---------------------------------------------------------------------*

CLASS lcl_controller DEFINITION.
  PUBLIC SECTION.
    METHODS:
      on_data_changed
                    FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm.
    METHODS:
      on_hotspot_click
                    FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id es_row_no.

    METHODS trans_alv_error_2_row
      IMPORTING
        io_data_changed TYPE REF TO cl_alv_changed_data_protocol
      CHANGING
        cs_row_data     TYPE any.
    METHODS upd_values_from_data_changed
      IMPORTING
        is_mod_cell     TYPE lvc_s_modi
      CHANGING
        cs_row_data     TYPE any
        co_data_changed TYPE REF TO cl_alv_changed_data_protocol.
    METHODS show_messages
      IMPORTING
        !it_messages TYPE zal30_i_row_status_msg.
    METHODS data_with_error
      RETURNING VALUE(rv_have) TYPE sap_bool.
    METHODS clear_row_status_fields
      CHANGING cs_row_data TYPE any.
  PROTECTED SECTION.
    TYPES: BEGIN OF ts_message_list,
             fieldname TYPE string,
             semaphor  TYPE c LENGTH 1,
             message   TYPE bapi_msg,
           END OF ts_message_list.
    TYPES: tt_message_list TYPE STANDARD TABLE OF ts_message_list WITH EMPTY KEY.

    METHODS hotspot_field_actions
      IMPORTING
        is_row_data TYPE any.
ENDCLASS.                    "lcl_event_alv DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_event_alv IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_controller IMPLEMENTATION.
  METHOD on_data_changed.
    PERFORM data_changed CHANGING er_data_changed
                                   e_onf4
                                   e_onf4_before
                                   e_onf4_after
                                   e_ucomm.
  ENDMETHOD.                    "handle_data_changed

  METHOD trans_alv_error_2_row.
    FIELD-SYMBOLS <lt_row_msg> TYPE zal30_i_row_status_msg.

    " Para poder guardar los errores necesito los campos donde se almacenan dichos mensajes y su campo de control
    ASSIGN COMPONENT zif_al30_data=>cs_control_fields_alv_data-row_status_msg OF STRUCTURE cs_row_data TO <lt_row_msg>.
    IF sy-subrc = 0.
      ASSIGN COMPONENT zif_al30_data=>cs_control_fields_alv_data-row_status OF STRUCTURE cs_row_data TO FIELD-SYMBOL(<row_status>).
      IF sy-subrc = 0.

        LOOP AT io_data_changed->mt_protocol ASSIGNING FIELD-SYMBOL(<ls_protocol>).
          INSERT VALUE #( type = zif_al30_data=>cs_msg_type-error fieldname = <ls_protocol>-fieldname  ) INTO TABLE <lt_row_msg> ASSIGNING FIELD-SYMBOL(<ls_row_msg>).

          <ls_row_msg>-message =  zcl_al30_util=>fill_return( iv_type =  <ls_protocol>-msgty
                                                              iv_number = <ls_protocol>-msgno
                                                              iv_message_v1 = <ls_protocol>-msgv1
                                                              iv_message_v2 = <ls_protocol>-msgv2
                                                              iv_message_v3 = <ls_protocol>-msgv3
                                                              iv_message_v4 = <ls_protocol>-msgv4
                                                              iv_id         = <ls_protocol>-msgid )-message.


        ENDLOOP.
        IF sy-subrc = 0.
          " Se marca el registro como erroneo
          <row_status> = zif_al30_data=>cs_msg_type-error.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD on_hotspot_click.
    READ TABLE <it_datos> ASSIGNING FIELD-SYMBOL(<ls_data>) INDEX e_row_id.
    IF sy-subrc = 0.
      " Se lanza el proceso segun la columna seleccionada
      CASE e_column_id.
        WHEN zif_al30_data=>cs_control_fields_alv_data-actions.
          hotspot_field_actions( EXPORTING is_row_data = <ls_data>  ).
      ENDCASE.
    ENDIF.
  ENDMETHOD.


  METHOD hotspot_field_actions.
    FIELD-SYMBOLS <lt_row_msg> TYPE zal30_i_row_status_msg.

    " Se recuperan los mensajes y se pasan al método que los mostrará
    ASSIGN COMPONENT zif_al30_data=>cs_control_fields_alv_data-row_status_msg OF STRUCTURE is_row_data TO <lt_row_msg>.
    IF sy-subrc = 0.

      show_messages( it_messages =  <lt_row_msg> ).

    ENDIF.
  ENDMETHOD.


  METHOD upd_values_from_data_changed.
    FIELD-SYMBOLS: <styles> TYPE lvc_t_styl.

* Paso los datos al listado
    LOOP AT mt_fieldcat ASSIGNING FIELD-SYMBOL(<ls_fieldcat>).
      CASE <ls_fieldcat>-fieldname.
          " El campo de acciones depende del valor del campo ROW_STATUS
        WHEN zif_al30_data=>cs_control_fields_alv_data-actions.
          ASSIGN COMPONENT zif_al30_data=>cs_control_fields_alv_data-row_status OF STRUCTURE cs_row_data TO FIELD-SYMBOL(<row_status>).
          IF sy-subrc = 0.
            CALL METHOD co_data_changed->modify_cell
              EXPORTING
                i_row_id    = is_mod_cell-row_id
                i_fieldname = zif_al30_data=>cs_control_fields_alv_data-actions
                i_value     = COND #( WHEN <row_status> = zif_al30_data=>cs_msg_type-error THEN icon_led_red ELSE space ).
          ENDIF.
        WHEN OTHERS.
          ASSIGN COMPONENT <ls_fieldcat>-fieldname OF STRUCTURE cs_row_data TO FIELD-SYMBOL(<field>).
          IF sy-subrc = 0.
            " Si el campo de idioma de la tabla de texto esta vacio lo informo. Aunque en principio no debería suceder porque al
            " insertar registros se inserta de manera automática.
            IF <ls_fieldcat>-fieldname = mv_field_lang_textable AND mv_field_lang_textable IS NOT INITIAL AND <field> IS NOT INITIAL.
              CALL METHOD co_data_changed->modify_cell
                EXPORTING
                  i_row_id    = is_mod_cell-row_id
                  i_fieldname = <ls_fieldcat>-fieldname
                  i_value     = sy-langu.

            ELSE.
              CALL METHOD co_data_changed->modify_cell
                EXPORTING
                  i_row_id    = is_mod_cell-row_id
                  i_fieldname = <ls_fieldcat>-fieldname
                  i_value     = <field>.
            ENDIF.

          ENDIF.
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.

  METHOD show_messages.

    DATA(lt_message_list) = VALUE tt_message_list( ).
    LOOP AT it_messages ASSIGNING FIELD-SYMBOL(<ls_messages>).
      " Se informan los valores fijos
      INSERT VALUE #( message = <ls_messages>-message
                      semaphor = COND #( WHEN <ls_messages>-type = zif_al30_data=>cs_msg_type-error
                                         THEN zif_al30_data=>cs_semaphor_alv_excep-error
                                         ELSE COND #( WHEN <ls_messages>-type = zif_al30_data=>cs_msg_type-success
                                              THEN zif_al30_data=>cs_semaphor_alv_excep-ok
                                              ELSE COND #( WHEN <ls_messages>-type = zif_al30_data=>cs_msg_type-warning
                                                           THEN zif_al30_data=>cs_semaphor_alv_excep-warning ) ) ) )
           INTO TABLE lt_message_list ASSIGNING FIELD-SYMBOL(<ls_message_list>).

      " Se busca la descripción del campo afectado
      READ TABLE mt_fieldcat ASSIGNING FIELD-SYMBOL(<ls_fieldcat>) WITH KEY fieldname = <ls_messages>-fieldname.
      IF sy-subrc = 0.
        <ls_message_list>-fieldname = COND #( WHEN <ls_fieldcat>-coltext IS NOT INITIAL THEN <ls_fieldcat>-coltext ELSE <ls_fieldcat>-reptext ).
      ENDIF.
    ENDLOOP.

    TRY.
        cl_salv_table=>factory( IMPORTING r_salv_table = DATA(lo_salv_table)
                                CHANGING  t_table      = lt_message_list ).

        IF lo_salv_table IS BOUND.

          lo_salv_table->get_columns( )->set_optimize( abap_true ).

          lo_salv_table->get_columns( )->set_exception_column( 'SEMAPHOR' ).

          DATA(lo_column) = lo_salv_table->get_columns(  )->get_column( 'FIELDNAME' ).
          lo_column->set_medium_text( TEXT-c01 ).

          lo_salv_table->set_screen_popup( start_column = '5'
                                           end_column   = '100'
                                           start_line   = '5'
                                           end_line     = '10' ).
          lo_salv_table->display( ).
        ENDIF.

      CATCH cx_salv_msg ##NO_HANDLER.
      CATCH cx_salv_not_found ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.

  METHOD data_with_error.

    " Devuelve si hay algun registro con errores y eso se mirá mediante el cmapo ROW_STATUS
    READ TABLE <it_datos> TRANSPORTING NO FIELDS WITH KEY (zif_al30_data=>cs_control_fields_alv_data-row_status) = zif_al30_data=>cs_msg_type-error.
    IF sy-subrc = 0.
      rv_have = abap_true.
    ELSE.
      rv_have = abap_false.
    ENDIF.

  ENDMETHOD.

  METHOD clear_row_status_fields.
    ASSIGN COMPONENT zif_al30_data=>cs_control_fields_alv_data-row_status OF STRUCTURE cs_row_data TO FIELD-SYMBOL(<field>).
    IF sy-subrc = 0.
      CLEAR <field>.
    ENDIF.

    ASSIGN COMPONENT zif_al30_data=>cs_control_fields_alv_data-row_status_msg OF STRUCTURE cs_row_data TO <field>.
    IF sy-subrc = 0.
      CLEAR <field>.
    ENDIF.

  ENDMETHOD.

ENDCLASS.                    "lcl_event_alv IMPLEMENTATION
