CLASS zcl_al30_view_ui5 DEFINITION
  PUBLIC
  INHERITING FROM zcl_al30_view
  CREATE PUBLIC .

  PUBLIC SECTION.
    "! <p class="shorttext synchronized">Save the original data in the global variable</p>
    "! This has to be done because the original data is lost at the end of the data collection service.
    "! @parameter io_original_data | <p class="shorttext synchronized">Original data object</p>
    METHODS set_original_data
      IMPORTING
        !io_original_data TYPE REF TO data.


  PROTECTED SECTION.
    METHODS add_edit_fields REDEFINITION.
    METHODS internal_verify_row_data REDEFINITION.
    "! <p class="shorttext synchronized">Verify foreign key data</p>
    "! @parameter cs_row_data | <p class="shorttext synchronized">Row data</p>
    METHODS verify_foreign_key
      CHANGING
        cs_row_data TYPE any.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_al30_view_ui5 IMPLEMENTATION.
  METHOD add_edit_fields.

    " Campos de siempre
    super->add_edit_fields( CHANGING ct_fcat = ct_fcat ).

* Se añade el campo de estilo a medida para UI5
    INSERT VALUE #( fieldname = zif_al30_ui5_data=>cs_control_fields_ui5_data-style rollname = 'ZAL30_I_UI5_FIELDS_STYLES' ) INTO TABLE ct_fcat.

  ENDMETHOD.

  METHOD set_original_data.
    mo_original_data = io_original_data.
  ENDMETHOD.

  METHOD internal_verify_row_data.

    " Se ejecuta la validación de la clase padre
    super->internal_verify_row_data(
      EXPORTING
        iv_row      = iv_row
      IMPORTING
        et_return   = et_return
      CHANGING
        cs_row_data = cs_row_data ).

    " En UI5 se chequea que los valores de los campos con clave externa existan
    verify_foreign_key( CHANGING cs_row_data = cs_row_data ).

  ENDMETHOD.


  METHOD verify_foreign_key.
    FIELD-SYMBOLS <lt_row_msg> TYPE zal30_i_row_status_msg.
    DATA ls_msg TYPE lvc_s_msg1.

    " Se procesan aquellos campos que tienen un clave externa
    LOOP AT mt_fields_ddic ASSIGNING FIELD-SYMBOL(<ls_fields_ddic>) WHERE checktable IS NOT INITIAL.

      DATA(lt_additional_fields) = VALUE dcfielddats(  ).

      " Se ponen en field symbols los campos necesarios para poder validar y devolver los posibles mensajes de error
      ASSIGN COMPONENT <ls_fields_ddic>-fieldname OF STRUCTURE cs_row_data TO FIELD-SYMBOL(<value>).
      IF sy-subrc = 0.
        ASSIGN COMPONENT zif_al30_data=>cs_control_fields_alv_data-row_status_msg OF STRUCTURE cs_row_data TO <lt_row_msg>.
        IF sy-subrc = 0.
          " Se hace un prepaso en la validación que consiste en poner el valor por defecto a los campos de la clave, excluyendo el propio
          " campo principal.
          LOOP AT mt_foreign_key_ddic ASSIGNING FIELD-SYMBOL(<ls_foreign_key>) WHERE fieldname EQ <ls_fields_ddic>-fieldname
                                                                                     AND forkey NE <ls_fields_ddic>-fieldname.

            " Compruebo que el campo de la clave externa lo tenga como campo en la vista
            READ TABLE mt_fields_ddic TRANSPORTING NO FIELDS
                       WITH KEY fieldname = <ls_foreign_key>-forkey.
            IF sy-subrc EQ 0.
              " Si lo tenemos en los datos se informa el valor en un puntero para pasarlo a la estructura
              ASSIGN COMPONENT <ls_foreign_key>-forkey OF STRUCTURE cs_row_data TO FIELD-SYMBOL(<value_forkey>).
              IF sy-subrc EQ 0.
                GET REFERENCE OF <value_forkey> INTO DATA(lo_value_forkey).
                INSERT VALUE #( tabname = <ls_foreign_key>-fortable
                                fieldname = <ls_foreign_key>-forkey
                                found = abap_true
                                value = lo_value_forkey  ) INTO TABLE lt_additional_fields.
              ENDIF.
            ENDIF.
          ENDLOOP.

          " Se lanza la validación estándar
          data(lv_lfieldname) = conv dfies-lfieldname( <ls_fields_ddic>-fieldname ).
          CALL FUNCTION 'DDUT_INPUT_CHECK'
            EXPORTING
              tabname           = <ls_fields_ddic>-tabname
              fieldname         = lv_lfieldname
              value             = <value>
            IMPORTING
              msgid             = ls_msg-msgid
              msgty             = ls_msg-msgty
              msgno             = ls_msg-msgno
              msgv1             = ls_msg-msgv1
              msgv2             = ls_msg-msgv2
              msgv3             = ls_msg-msgv3
              msgv4             = ls_msg-msgv4
            CHANGING
              additional_fields = lt_additional_fields
            EXCEPTIONS
              no_ddic_field     = 1
              illegal_move      = 2
              OTHERS            = 3.

          " Si la función devuelve un error o hay un mensaje de error se informará del problema en la línea
          IF sy-subrc NE 0 OR ls_msg-msgty = zif_al30_data=>cs_msg_type-error.
            INSERT VALUE #( type = zif_al30_data=>cs_msg_type-error
                            fieldname = <ls_fields_ddic>-fieldname ) INTO TABLE <lt_row_msg> ASSIGNING FIELD-SYMBOL(<ls_row_msg>).


            " Si hay un error se informa del mensaje exacto producido
            IF ls_msg-msgty = zif_al30_data=>cs_msg_type-error.
              <ls_row_msg>-message = zcl_al30_util=>fill_return( EXPORTING iv_type = ls_msg-msgty
                                                                           iv_number = ls_msg-msgno
                                                                           iv_id = ls_msg-msgid
                                                                           iv_message_v1 = ls_msg-msgv1
                                                                           iv_message_v2 = ls_msg-msgv2
                                                                           iv_message_v3 = ls_msg-msgv3
                                                                           iv_message_v4 = ls_msg-msgv4
                                                                           iv_langu = mv_langu )-message.


            ELSE. " Error de llamada a la función se informa un mensaje generico
              <ls_row_msg>-message = zcl_al30_util=>fill_return( EXPORTING iv_type = zif_al30_data=>cs_msg_type-error
                                                                            iv_number = '046'
                                                                            iv_id = zif_al30_data=>cv_msg_id
                                                                            iv_langu = mv_langu )-message.
            ENDIF.

          ENDIF.

        ENDIF.

      ENDIF.


    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
