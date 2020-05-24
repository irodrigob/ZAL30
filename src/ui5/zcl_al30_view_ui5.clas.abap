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

    "! <p class="shorttext synchronized">Get fields with search help</p>
    "!
    "! @parameter et_catalog | <p class="shorttext synchronized" lang="en"></p>
    METHODS get_f4_catalog
      EXPORTING
        !et_catalog TYPE zif_al30_ui5_data=>tt_f4_catalog.


  PROTECTED SECTION.
    METHODS add_edit_fields REDEFINITION.
    METHODS internal_verify_row_data REDEFINITION.
    "! <p class="shorttext synchronized">Verify foreign key data</p>
    "! @parameter iv_fieldname | <p class="shorttext synchronized">Fieldname</p>
    "! @parameter cs_row_data | <p class="shorttext synchronized">Row data</p>
    METHODS verify_foreign_key
      IMPORTING
        !iv_fieldname TYPE fieldname
      CHANGING
        cs_row_data   TYPE any.
    "! <p class="shorttext synchronized">Verify exist data</p>
    "! Two validations are made: foreign key and fix values
    "! @parameter cs_row_data | <p class="shorttext synchronized">Row data</p>
    METHODS verify_exist_data
      CHANGING
        cs_row_data TYPE any.
    "! <p class="shorttext synchronized">Verify fix values of domain</p>
    "! @parameter iv_fieldname | <p class="shorttext synchronized">Fieldname</p>
    "! @parameter cs_row_data | <p class="shorttext synchronized">Row data</p>
    METHODS verify_fix_values
      IMPORTING
        iv_fieldname TYPE dd03p-fieldname
      CHANGING
        cs_row_data  TYPE any.
    "! <p class="shorttext synchronized" lang="en">Exit UI5 for change F4 search help</p>
    "! Field that in UI5 will still have search help
    "! @parameter it_fields_text | <p class="shorttext synchronized">Descriptions of field</p>
    "! @parameter is_field_ddic | <p class="shorttext synchronized">Information of fields in DDIC</p>
    "! @parameter iv_langu | <p class="shorttext synchronized">Language</p>
    "! @parameter ev_no_include | <p class="shorttext synchronized">No include field un the catalog</p>
    "! @parameter cs_f4_catalog | <p class="shorttext synchronized">Data of catalog</p>
    METHODS exit_ui5_change_f4_catalog
      IMPORTING
        !it_fields_text TYPE zif_al30_data=>tt_fields_text_view
        !is_field_ddic  TYPE dd03p
        !iv_langu       TYPE sy-langu
      EXPORTING
        !ev_no_include  TYPE sap_bool
      CHANGING
        cs_f4_catalog   TYPE zif_al30_ui5_data=>ts_f4_catalog.
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

    " En UI5 se chequea de dos maneras distintas: por clave externa o por valores fijos del domino
    verify_exist_data( CHANGING cs_row_data = cs_row_data ).

  ENDMETHOD.


  METHOD verify_foreign_key.
    FIELD-SYMBOLS <lt_row_msg> TYPE zal30_i_row_status_msg.
    DATA ls_msg TYPE lvc_s_msg1.

    " Se procesan aquellos campos que tienen un clave externa
    READ TABLE mt_fields_ddic ASSIGNING FIELD-SYMBOL(<ls_fields_ddic>) WITH KEY fieldname = iv_fieldname.
    IF sy-subrc = 0.
      " No debería ocurrir porque la método se llama si el campo tiene clave externa, pero aún así pongo el contro.
      IF <ls_fields_ddic>-checktable IS NOT INITIAL.
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
            DATA(lv_lfieldname) = CONV dfies-lfieldname( <ls_fields_ddic>-fieldname ).
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

      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD verify_exist_data.

    " Se procesan aquellos campos que tienen un clave externa o dominio
    LOOP AT mt_fields_ddic ASSIGNING FIELD-SYMBOL(<ls_fields_ddic>) WHERE checktable IS NOT INITIAL
                                                                          OR domname IS NOT INITIAL.
      IF <ls_fields_ddic>-checktable IS NOT INITIAL. " Tiene preferencia la clave externa
        verify_foreign_key( EXPORTING iv_fieldname = <ls_fields_ddic>-fieldname
                            CHANGING cs_row_data = cs_row_data ).
      ELSEIF <ls_fields_ddic>-domname IS NOT INITIAL.
        verify_fix_values( EXPORTING iv_fieldname = <ls_fields_ddic>-fieldname
                            CHANGING cs_row_data = cs_row_data ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD verify_fix_values.
    FIELD-SYMBOLS <lt_row_msg> TYPE zal30_i_row_status_msg.
    DATA ls_msg TYPE lvc_s_msg1.
    DATA lt_dd07v      TYPE STANDARD TABLE OF dd07v.

    " Se procesan aquellos campos que tienen un clave externa
    READ TABLE mt_fields_ddic ASSIGNING FIELD-SYMBOL(<ls_fields_ddic>) WITH KEY fieldname = iv_fieldname.
    IF sy-subrc = 0.
      " No debería ocurrir porque la método se llama si el campo tiene clave externa, pero aún así pongo el contro.
      IF <ls_fields_ddic>-domname IS NOT INITIAL.


        " Se ponen en field symbols los campos necesarios para poder validar y devolver los posibles mensajes de error
        ASSIGN COMPONENT <ls_fields_ddic>-fieldname OF STRUCTURE cs_row_data TO FIELD-SYMBOL(<value>).
        IF sy-subrc = 0.
          ASSIGN COMPONENT zif_al30_data=>cs_control_fields_alv_data-row_status_msg OF STRUCTURE cs_row_data TO <lt_row_msg>.
          IF sy-subrc = 0.

            CALL FUNCTION 'DDUT_DOMVALUES_GET'
              EXPORTING
                name      = <ls_fields_ddic>-domname
                langu     = ' '
              TABLES
                dd07v_tab = lt_dd07v[].

            IF lt_dd07v IS NOT INITIAL. " A de tener valores fijos
              LOOP AT lt_dd07v TRANSPORTING NO FIELDS WHERE domvalue_l EQ <value>
                                                            OR ( domvalue_l LE <value> AND
                                                                 domvalue_h GE <value> AND NOT
                                                                 domvalue_h IS INITIAL ).
                EXIT.
              ENDLOOP.
              " Si no existe se informa del error. Se aprovecha el mismo mensaje que el usado en los ALV
              IF sy-subrc NE 0.
                INSERT VALUE #( type = zif_al30_data=>cs_msg_type-error
                                             fieldname = <ls_fields_ddic>-fieldname ) INTO TABLE <lt_row_msg> ASSIGNING FIELD-SYMBOL(<ls_row_msg>).

                <ls_row_msg>-message = zcl_al30_util=>fill_return( EXPORTING iv_type = ls_msg-msgty
                                                            iv_number = '002'
                                                            iv_id = '00'
                                                            iv_message_v1 = <ls_fields_ddic>-fieldname
                                                            iv_langu = mv_langu )-message.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD get_f4_catalog.

    CLEAR: et_catalog.

    " Se buscan los dominios que tienen valores
    SELECT DISTINCT domname
           FROM dd07l
           FOR ALL ENTRIES IN @mt_fields_ddic
           WHERE domname = @mt_fields_ddic-domname
           INTO TABLE @DATA(lt_dom_values).

    " Los campos que tendrán catalogo de campos serán aquellos que tengan un tabla de chequeo y los campos con dominio que tengan valores fijos
    LOOP AT mt_fields_ddic ASSIGNING FIELD-SYMBOL(<ls_fields_ddic>) WHERE checktable IS NOT INITIAL
                                                                         OR domname IS NOT INITIAL.
      " Por defecto el campo entrará en el catalogo
      DATA(lv_add) = abap_true.

      DATA(ls_f4_catalog) = VALUE zif_al30_ui5_data=>ts_f4_catalog( fieldname = <ls_fields_ddic>-fieldname ).

      " Si no tiene clave externa hay que comprobar si el dominio tiene datos
      IF <ls_fields_ddic>-checktable IS INITIAL.
        READ TABLE lt_dom_values TRANSPORTING NO FIELDS WITH KEY domname = <ls_fields_ddic>-domname.
        IF sy-subrc NE 0 .
          lv_add = abap_false.
        ELSE.
          " Se mira el el campo es un checkbox. En caso afirmativo tampoco se añade
          READ TABLE mt_fields TRANSPORTING NO FIELDS
                               WITH KEY fieldname = <ls_fields_ddic>-fieldname
                                        checkbox = abap_true.
          IF sy-subrc = 0.
            lv_add = abap_false.
          ENDIF.
        ENDIF.
      ENDIF.

      IF lv_add = abap_true.
        " El texto para el campo donde se verá el código será el header text
        READ TABLE mt_fields_text ASSIGNING FIELD-SYMBOL(<ls_fields_text>)
                                  WITH KEY fieldname = <ls_fields_ddic>-fieldname
                                           spras = mv_langu.
        IF sy-subrc = 0.
          ls_f4_catalog-label_field_code = <ls_fields_text>-reptext.
        ENDIF.

        " Para el campo de descripción del código será el texto "description"
        ls_f4_catalog-label_field_description = 'Description'(t01).

        " Se llama al exit para poder cambiar textos y si se incluye o no

        DATA(ls_f4_catalog_exit) = ls_f4_catalog.
        exit_ui5_change_f4_catalog(
          EXPORTING
            it_fields_text = VALUE #( FOR <wa> IN mt_fields_text WHERE ( fieldname = <ls_fields_ddic>-fieldname ) ( <wa> ) )
            is_field_ddic  = <ls_fields_ddic>
            iv_langu       = mv_langu
          IMPORTING
            ev_no_include  =  DATA(lv_no_include) " Posibilidad de indicar que no se incluya el campo
          CHANGING
            cs_f4_catalog  = ls_f4_catalog_exit ).

        IF lv_no_include = abap_false.

          " Se transfieren los campos de texto
          ls_f4_catalog-label_field_code = ls_f4_catalog_exit-label_field_code.
          ls_f4_catalog-label_field_description = ls_f4_catalog_exit-label_field_description.

          INSERT ls_f4_catalog INTO TABLE et_catalog.

        ENDIF.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD exit_ui5_change_f4_catalog.
    DATA lv_metodo TYPE seocpdname.

    IF mo_exit_class IS BOUND.

* Monto el método al cual se llamará de la clase de exit.
      CONCATENATE zif_al30_data=>cv_intf_exit '~EXIT_UI5_CHANGE_F4_CATALOG' INTO lv_metodo.

      TRY.
          CALL METHOD mo_exit_class->(lv_metodo)
            EXPORTING
              it_fields_text = it_fields_text
              is_field_ddic  = is_field_ddic
              iv_langu       = iv_langu
            IMPORTING
              ev_no_include  = ev_no_include
            CHANGING
              cs_f4_catalog  = cs_f4_catalog.

        CATCH cx_root.
      ENDTRY.

    ENDIF.

  ENDMETHOD.

ENDCLASS.
