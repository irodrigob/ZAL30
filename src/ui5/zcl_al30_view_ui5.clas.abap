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
    "! <p class="shorttext synchronized">Get data for search help of a field</p>
    "!
    "! @parameter iv_field_name | <p class="shorttext synchronized">Field name</p>
    "! @parameter et_data | <p class="shorttext synchronized">Data for the search help</p>
    METHODS get_f4_data
      IMPORTING
        !iv_field_name TYPE fieldname
      EXPORTING
        !et_data       TYPE zif_al30_ui5_data=>tt_f4_data.


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
        iv_field_name TYPE dd03p-fieldname
      CHANGING
        cs_row_data   TYPE any.
    "! <p class="shorttext synchronized" lang="en">Exit UI5 for change F4 search help</p>
    "! Field that in UI5 will still have search help
    "! @parameter it_fields_text | <p class="shorttext synchronized">Descriptions of field</p>
    "! @parameter is_field_ddic | <p class="shorttext synchronized">Information of fields in DDIC</p>
    "! @parameter iv_langu | <p class="shorttext synchronized">Language</p>
    "! @parameter ev_no_include | <p class="shorttext synchronized">No include field un the catalog</p>
    "! @parameter cs_f4_catalog | <p class="shorttext synchronized">Data of catalog</p>
    METHODS exit_ui5_change_f4_catalog
      IMPORTING
        it_fields_text TYPE zif_al30_data=>tt_fields_text_view
        is_field_ddic  TYPE dd03p
        iv_langu       TYPE sy-langu
      EXPORTING
        ev_no_include  TYPE sap_bool
      CHANGING
        cs_f4_catalog  TYPE zif_al30_ui5_data=>ts_f4_catalog.
    "! <p class="shorttext synchronized" lang="en">Get F4 Data for domain</p>
    "! @parameter iv_domain | <p class="shorttext synchronized">Dominio</p>
    "! @parameter et_data | <p class="shorttext synchronized">Data of domain</p>
    METHODS get_f4_data_domain
      IMPORTING
        iv_domain TYPE dd03p-domname
      EXPORTING
        et_data   TYPE zif_al30_ui5_data=>tt_f4_data.
    "! <p class="shorttext synchronized" lang="en">Get F4 Data for foreign key</p>
    "! @parameter iv_fieldname | <p class="shorttext synchronized">Fieldname</p>
    "! @parameter iv_checktable | <p class="shorttext synchronized">Check table</p>
    "! @parameter et_data | <p class="shorttext synchronized">Data of foreign key</p>
    METHODS get_f4_data_foreign_key
      IMPORTING
        iv_field_name TYPE dd03p-fieldname
        iv_checktable TYPE dd03p-checktable
      EXPORTING
        et_data       TYPE zif_al30_ui5_data=>tt_f4_data.
    "! <p class="shorttext synchronized">Exit UI5 for get data for foreign key</p>
    "!
    "! @parameter iv_fieldname | <p class="shorttext synchronized">Fieldname</p>
    "! @parameter iv_checktable | <p class="shorttext synchronized">Checktable</p>
    "! @parameter ev_own_data | <p class="shorttext synchronized" lang="en">Own data obtained</p>
    "! @parameter et_data | <p class="shorttext synchronized" lang="en">Values</p>
    METHODS exit_ui5_get_f4_data_forgn_key
      IMPORTING
        iv_fieldname  TYPE fieldname
        iv_checktable TYPE tabname
      EXPORTING
        ev_own_data   TYPE sap_bool
        et_data       TYPE zif_al30_ui5_data=>tt_f4_data.
    "! <p class="shorttext synchronized">Build the SQL statement to obtain the values</p>
    "! Build the SQL statement to obtain the values of the foreign key
    "! @parameter iv_fieldname | <p class="shorttext synchronized">Fieldname</p>
    "! @parameter iv_checktable | <p class="shorttext synchronized">Checktable</p>
    "! @parameter ev_sql | <p class="shorttext synchronized">SQL statement</p>
    "! @parameter et_fields | <p class="shorttext synchronized">Fields</p>
    "! @parameter ev_texttable | <p class="shorttext synchronized">Text table</p>
    "! @parameter et_cols | <p class="shorttext synchronized">Fields of the SQL</p>
    "! @parameter eo_itab | <p class="shorttext synchronized">Internal table for save the sql values</p>
    METHODS create_sql_foreign_key
      IMPORTING
        iv_fieldname  TYPE fieldname
        iv_checktable TYPE dd03p-checktable
      EXPORTING
        ev_sql        TYPE string
        et_fields     TYPE zif_al30_data=>tt_fields_view
        ev_texttable  TYPE tabname
        et_cols       TYPE adbc_column_tab
        eo_itab       TYPE REF TO data.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_al30_view_ui5 IMPLEMENTATION.


  METHOD add_edit_fields.

    " Campos de siempre
    super->add_edit_fields( CHANGING ct_fcat = ct_fcat ).

* Se añade el campo de estilo a medida para UI5
    INSERT VALUE #( fieldname = zif_al30_ui5_data=>cs_control_fields_ui5_data-style rollname = 'ZAL30_I_UI5_FIELDS_STYLES' ) INTO TABLE ct_fcat.

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


  METHOD get_f4_data.

    CLEAR et_data.

    " Se lee como es el campo para saber como buscar los datos
    READ TABLE mt_fields_ddic ASSIGNING FIELD-SYMBOL(<ls_fields_ddic>) WITH KEY fieldname = iv_field_name.
    IF sy-subrc = 0.

      IF <ls_fields_ddic>-checktable IS NOT INITIAL. " Por tabla de verificación
        get_f4_data_foreign_key( EXPORTING iv_checktable = <ls_fields_ddic>-checktable
                                           iv_field_name = <ls_fields_ddic>-fieldname
                                    IMPORTING et_data = et_data ).
      ELSEIF <ls_fields_ddic>-domname IS NOT INITIAL. " Por dominio
        get_f4_data_domain( EXPORTING iv_domain = <ls_fields_ddic>-domname
                            IMPORTING et_data = et_data ).
      ENDIF.

    ENDIF.

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


  METHOD set_original_data.
    mo_original_data = io_original_data.
  ENDMETHOD.


  METHOD verify_exist_data.

    " Se procesan aquellos campos que tienen un clave externa o dominio
    LOOP AT mt_fields_ddic ASSIGNING FIELD-SYMBOL(<ls_fields_ddic>) WHERE checktable IS NOT INITIAL
                                                                          OR domname IS NOT INITIAL.
      IF <ls_fields_ddic>-checktable IS NOT INITIAL. " Tiene preferencia la clave externa
        verify_foreign_key( EXPORTING iv_fieldname = <ls_fields_ddic>-fieldname
                            CHANGING cs_row_data = cs_row_data ).
      ELSEIF <ls_fields_ddic>-domname IS NOT INITIAL.
        verify_fix_values( EXPORTING iv_field_name = <ls_fields_ddic>-fieldname
                            CHANGING cs_row_data = cs_row_data ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD verify_fix_values.
    FIELD-SYMBOLS <lt_row_msg> TYPE zal30_i_row_status_msg.
    DATA ls_msg TYPE lvc_s_msg1.
    DATA lt_dd07v      TYPE STANDARD TABLE OF dd07v.

    " Se procesan aquellos campos que tienen un clave externa
    READ TABLE mt_fields_ddic ASSIGNING FIELD-SYMBOL(<ls_fields_ddic>) WITH KEY fieldname = iv_field_name.
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

  METHOD get_f4_data_domain.
    DATA lt_values TYPE STANDARD TABLE OF dd07v.

    CALL FUNCTION 'DDUT_DOMVALUES_GET'
      EXPORTING
        name      = iv_domain
        langu     = mv_langu
      TABLES
        dd07v_tab = lt_values[]
      EXCEPTIONS
        OTHERS    = 2.

    et_data = VALUE #( FOR <wa> IN lt_values ( code = <wa>-domvalue_l description = <wa>-ddtext ) ).

  ENDMETHOD.

  METHOD get_f4_data_foreign_key.
    FIELD-SYMBOLS: <tbl> TYPE STANDARD TABLE.

    CLEAR et_data.

    " Se llama la exit para ver si los datos se van a determinar de manera interna
    exit_ui5_get_f4_data_forgn_key(
      EXPORTING
        iv_fieldname  = iv_field_name
        iv_checktable = iv_checktable
      IMPORTING
        ev_own_data   = DATA(lv_own_data)
        et_data       = DATA(lt_data) ).

    " Si se indica que no los datos no son propios de la exit se continua con el proceso de buscarlo a nivel interno
    IF lv_own_data = abap_false.

      " se crea la sentencia SQL que se usará para buscar los datos
      create_sql_foreign_key( EXPORTING iv_fieldname  = iv_field_name
                                        iv_checktable = iv_checktable
                              IMPORTING ev_sql = DATA(lv_sql)
                                        et_cols = DATA(lt_cols)
                                        et_fields = DATA(lt_fields)
                                        eo_itab = DATA(lo_data) ).
      TRY.
          IF lv_sql IS NOT INITIAL. " Si hay SQL se ejecuta

            ASSIGN lo_data->* TO <tbl>.

            " Se lanza la SQL
            DATA(lo_result) = NEW cl_sql_statement( )->execute_query( lv_sql ).

            " Se indica donde se guardarán los datos y que columnas se quiere
            lo_result->set_param_table( itab_ref = lo_data corresponding_fields = lt_cols ).

            "Se leen los datos
            IF lo_result->next_package( ) > 0.

            ENDIF.
          ENDIF.
        CATCH cx_sql_exception INTO DATA(lo_err).
        CATCH cx_parameter_invalid  INTO DATA(lo_err2).
      ENDTRY.

    ELSE.
      et_data = lt_data.
    ENDIF.




  ENDMETHOD.

  METHOD exit_ui5_get_f4_data_forgn_key.
    DATA lv_metodo TYPE seocpdname.

    IF mo_exit_class IS BOUND.

* Monto el método al cual se llamará de la clase de exit.
      CONCATENATE zif_al30_data=>cv_intf_exit '~EXIT_UI5_GET_F4_DATA_FORGN_KEY' INTO lv_metodo.

      TRY.
          CALL METHOD mo_exit_class->(lv_metodo)
            EXPORTING
              iv_fieldname  = iv_fieldname
              iv_checktable = iv_checktable
              iv_langu      = mv_langu
            IMPORTING
              ev_own_data   = ev_own_data
              et_data       = et_data.

        CATCH cx_root.
      ENDTRY.

    ENDIF.
  ENDMETHOD.


  METHOD create_sql_foreign_key.
    DATA lt_fcat_itab TYPE lvc_t_fcat.

    CLEAR: ev_sql, et_cols, ev_texttable, et_fields, eo_itab.
    TRY.

        NEW zcl_al30_conf(  )->get_fields_view_ddic(
                    EXPORTING
                      iv_name_view        = iv_checktable
                      iv_langu            = mv_langu
                      iv_add_texttable    = abap_true
                      iv_all_language     = abap_false
                    IMPORTING
                      et_fields           = et_fields
                      ev_texttable        = DATA(lv_texttable)
                      et_fields_ddic = DATA(lt_fields_ddic) ).



        " Leo cual es el campo de la tabla de valores que esta unido al campo de la tabla principal. Normalmente los campos se llaman igual, pero se pone por las excepciones.
        READ TABLE mt_foreign_key_ddic ASSIGNING FIELD-SYMBOL(<ls_foreign_key_ddic>) WITH KEY forkey = iv_fieldname.
        IF sy-subrc = 0.

          " En ET_COLS se guardan los campos que se usarán en el SELECT estos campos luego se pasarán a las clases que ejecutarán la consulta
          INSERT iv_fieldname INTO TABLE et_cols.

          " Se informa el campo en la estructura que permitirá montar la tabla interna para guardar el resultado de la SQL
          READ TABLE lt_fields_ddic ASSIGNING FIELD-SYMBOL(<ls_fields_ddic>) WITH KEY fieldname = <ls_foreign_key_ddic>-checkfield.
          IF sy-subrc = 0.
            IF <ls_fields_ddic>-rollname IS NOT INITIAL.
              INSERT VALUE #( fieldname = iv_fieldname rollname = <ls_fields_ddic>-rollname ) INTO TABLE lt_fcat_itab.
            ELSE.
              INSERT VALUE #( fieldname = iv_fieldname inttype = <ls_fields_ddic>-inttype intlen = <ls_fields_ddic>-intlen decimals = <ls_fields_ddic>-decimals  ) INTO TABLE lt_fcat_itab.
            ENDIF.
          ENDIF.

          " El campo principal de la tabla de la clave externa le pongo un alias para equiparar su nombre al del campo de la tabla que edita,
          " de esta manera facilitar pasar datos a la tabla que contendrá los valores
          ev_sql = |SELECT { zif_al30_data=>cs_alias_sql-view }.{ <ls_foreign_key_ddic>-checkfield } AS { iv_fieldname }|.

          " Si hay tabla de textos se montan los campos de la tabla de texto ignorando los campos clave
          IF lv_texttable IS NOT INITIAL.

            LOOP AT et_fields ASSIGNING FIELD-SYMBOL(<ls_fields>) WHERE field_texttable = abap_true AND key_ddic = abap_false.
              ev_sql = |{ ev_sql }, { zif_al30_data=>cs_alias_sql-texttable }.{ <ls_fields>-fieldname }|.
              INSERT <ls_fields>-fieldname INTO TABLE et_cols. " Campos de mapeos para el SQL

              " Se informa el campo en la estructura que permitirá montar la tabla interna para guardar el resultado de la SQL
              READ TABLE lt_fields_ddic ASSIGNING <ls_fields_ddic> WITH KEY fieldname = <ls_fields>-fieldname.
              IF sy-subrc = 0.
                IF <ls_fields_ddic>-rollname IS NOT INITIAL.
                  INSERT VALUE #( fieldname = <ls_fields>-fieldname rollname = <ls_fields_ddic>-rollname ) INTO TABLE lt_fcat_itab.
                ELSE.
                  INSERT VALUE #( fieldname = <ls_fields>-fieldname inttype = <ls_fields_ddic>-inttype intlen = <ls_fields_ddic>-intlen decimals = <ls_fields_ddic>-decimals  ) INTO TABLE lt_fcat_itab.
                ENDIF.
              ENDIF.

            ENDLOOP.

          ENDIF.

          " Se informa de donde salen los datos
          ev_sql = |{ ev_sql } FROM { iv_checktable } { zif_al30_data=>cs_alias_sql-view }|.

          " De nuevo si hay tabla de texto se hace la condición entre las dos tablas
          IF lv_texttable IS NOT INITIAL.
            " Se como es la relación de tabla de chequeo y su tabla de textos
            NEW zcl_al30_conf( )->read_single_view_ddic( EXPORTING iv_name_view = lv_texttable
                                                         IMPORTING et_dd05m = DATA(lt_dd05m) ).



            " Se informan la condicion general
            DATA(lv_condition) = REDUCE string( INIT sql TYPE string FOR <ls_dd05m> IN lt_dd05m WHERE ( fortable = lv_texttable AND checktable = iv_checktable ) NEXT sql = sql &&
                                  COND #( LET sep = 'AND' condition = |{ zif_al30_data=>cs_alias_sql-texttable }.{ <ls_dd05m>-forkey } = { zif_al30_data=>cs_alias_sql-view }.{ <ls_dd05m>-checkfield }|
                                  IN WHEN sql IS NOT INITIAL THEN | { sep } { condition }| ELSE |{ condition }| ) ).

            " Se le suma el campo idioma de la tabla de textos
            READ TABLE et_fields ASSIGNING <ls_fields> WITH KEY lang_texttable = abap_true.
            IF sy-subrc = 0.
              lv_condition = |{ lv_condition } AND { zif_al30_data=>cs_alias_sql-texttable }.{ <ls_fields>-fieldname } = '{ mv_langu }'|.
            ENDIF.

            ev_sql = |{ ev_sql } LEFT OUTER JOIN { lv_texttable } { zif_al30_data=>cs_alias_sql-texttable } ON { lv_condition }|.

          ENDIF.

          " Se añade en el where si el campo tiene mandante
          READ TABLE lt_fields_ddic ASSIGNING <ls_fields_ddic> WITH KEY datatype = zif_al30_data=>cs_datatype-mandt.
          IF sy-subrc = 0.
            ev_sql = |{ ev_sql } WHERE ( { zif_al30_data=>cs_alias_sql-view }.{ <ls_fields_ddic>-fieldname } = '{ sy-mandt }' )|.
          ENDIF.

          " Se añade que se orden por el campo clave que se pide
          ev_sql = |{ ev_sql } ORDER BY { zif_al30_data=>cs_alias_sql-view }.{ <ls_foreign_key_ddic>-checkfield }|.


          " Se genera la tabla para guardar los datos
          zcl_ca_dynamic_tables=>create_it_from_fcat( EXPORTING i_fields = lt_fcat_itab
                                                      IMPORTING e_table = eo_itab ).

        ENDIF.

      CATCH cx_root.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
