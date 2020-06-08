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
  PROTECTED SECTION.
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
          mo_controller->hotspot_field_actions( EXPORTING is_row_data = <ls_data>  ).
      ENDCASE.
    ENDIF.
  ENDMETHOD.


  METHOD hotspot_field_actions.
  BREAK-POINT.
  ENDMETHOD.

ENDCLASS.                    "lcl_event_alv IMPLEMENTATION
