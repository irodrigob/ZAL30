CLASS zcl_al30_ui5_test_exit DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_al30_exit_class.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_al30_ui5_test_exit IMPLEMENTATION.
  METHOD zif_al30_exit_class~exit_before_read_data.
    FIELD-SYMBOLS <lt_style> TYPE lvc_t_styl.


    LOOP AT ct_data ASSIGNING FIELD-SYMBOL(<ls_data>).
      DATA(lv_tabix) = sy-tabix.

      " Ejemplo donde si la fecha es inferior al 01/01/2020 se desactiva.
      ASSIGN COMPONENT 'FIELDDATE' OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<field>).
      IF sy-subrc = 0.
        IF <field> < '20200101'.
          ASSIGN COMPONENT zif_al30_data=>cs_control_fields_alv_data-style OF STRUCTURE <ls_data> TO <lt_style>.
          IF sy-subrc = 0.
            INSERT VALUE #( fieldname = 'FIELDDATE' style = cl_gui_alv_grid=>mc_style_disabled )  INTO TABLE <lt_style>.
          ENDIF.
        ENDIF.
      ENDIF.

      " Relleno de la posición en el campo virtual
      ASSIGN COMPONENT 'VIR_POS' OF STRUCTURE <ls_data> TO <field>.
      IF sy-subrc = 0.
        <field> = lv_tabix.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
  METHOD zif_al30_exit_class~exit_verify_field_data.
    IF iv_fieldname = 'FIELDDATE'.
      DATA(lv_date) = CONV sydatum( iv_value ).
      IF lv_date < '20000101'.
        es_return-type = 'E'.
        es_return-message = 'The date cannot be less than the year 2000'.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD zif_al30_exit_class~exit_after_read_data.

  ENDMETHOD.

  METHOD zif_al30_exit_class~exit_after_save_data.

  ENDMETHOD.

  METHOD zif_al30_exit_class~exit_before_save_data.

    ev_abort_save = abap_true.

  ENDMETHOD.

  METHOD zif_al30_exit_class~exit_check_auth_data_read.

  ENDMETHOD.

  METHOD zif_al30_exit_class~exit_in_process_data_read.

  ENDMETHOD.

  METHOD zif_al30_exit_class~exit_process_catalog_of_field.

  ENDMETHOD.

  METHOD zif_al30_exit_class~exit_set_edit_mode_alv.

  ENDMETHOD.

  METHOD zif_al30_exit_class~exit_verify_change_row_data.

    CLEAR et_return.

*    ASSIGN COMPONENT 'FIELDWAERS' OF STRUCTURE cs_row_data TO FIELD-SYMBOL(<waers>).
*    IF sy-subrc = 0.
*      SELECT SINGLE waers INTO @DATA(lv_waers) FROM tcurc WHERE waers =  @<waers>.
*      IF sy-subrc NE 0.
*        INSERT VALUE #( type = zif_al30_data=>cs_msg_type-error
*                        id = '5A'
*                        number = '139'
*                        field = 'FIELDWAERS' ) INTO TABLE et_return.
*      ENDIF.
*    ENDIF.

  ENDMETHOD.

  METHOD zif_al30_exit_class~exit_verify_row_data.

*    IF iv_save_process = abap_true.
    ASSIGN COMPONENT 'FIELDWAERS' OF STRUCTURE is_row_data TO FIELD-SYMBOL(<waers>).
    IF sy-subrc = 0.
      IF <waers> = 'GBP'.
        INSERT VALUE #( type = 'E' id = 'ZCA_AL30' number = '0000' message_v1 = 'GBP no permitido' field = 'FIELDWAERS' ) INTO TABLE et_return.
      ENDIF.
    ENDIF.
*    ENDIF.

  ENDMETHOD.

  METHOD zif_al30_exit_class~exit_verify_save_data.

  ENDMETHOD.

  METHOD zif_al30_exit_class~exit_ui5_change_f4_catalog.

  ENDMETHOD.

  METHOD zif_al30_exit_class~exit_ui5_get_f4_data_forgn_key.

  ENDMETHOD.

  METHOD zif_al30_exit_class~exit_ui5_fill_f4_foreign_key.

    IF iv_fieldname = 'FIELDWAERS'.
      CLEAR et_data.
      ev_completed_data = abap_true.

      LOOP AT it_foreign_key_data ASSIGNING FIELD-SYMBOL(<ls_foreign_key_data>).
        DATA(ls_data) = VALUE zif_al30_ui5_data=>ts_f4_data(  ).

        ASSIGN COMPONENT iv_fieldname OF STRUCTURE <ls_foreign_key_data> TO FIELD-SYMBOL(<code>).
        IF sy-subrc = 0.
          ls_data-code = <code>.
        ENDIF.
        ASSIGN COMPONENT 'LTEXT' OF STRUCTURE <ls_foreign_key_data> TO FIELD-SYMBOL(<description>).
        IF sy-subrc = 0.
          ls_data-description = <description> .
        ENDIF.

        INSERT ls_data INTO TABLE et_data.

      ENDLOOP.
    ENDIF.

  ENDMETHOD.

  METHOD zif_al30_exit_class~exit_ui5_post_fill_f4_data.

  ENDMETHOD.

ENDCLASS.
