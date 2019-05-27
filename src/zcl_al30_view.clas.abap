CLASS zcl_al30_view DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
*"* public components of class ZCL_AL30_VIEW
*"* do not include other source files here!!!
    TYPE-POOLS adbc .

    METHODS read_data
      IMPORTING
        !it_fields TYPE zal30_i_fields_alv
        !is_view   TYPE zal30_t_view
      EXPORTING
        !es_return TYPE bapiret2
        !et_data   TYPE STANDARD TABLE .
    METHODS get_fieldcat_edit_view
      IMPORTING
        !it_fields       TYPE zal30_i_fields_alv
        !is_view         TYPE zal30_t_view
      EXPORTING
        !es_return       TYPE bapiret2
        !et_fieldcat     TYPE lvc_t_fcat
        !et_fieldcat_key TYPE lvc_t_fcat .
    METHODS get_fieldcat_view
      IMPORTING
        !it_fields       TYPE zal30_i_fields_alv
        !is_view         TYPE zal30_t_view
      EXPORTING
        !es_return       TYPE bapiret2
        !et_fieldcat     TYPE lvc_t_fcat
        !et_fieldcat_key TYPE lvc_t_fcat .
    METHODS create_it_data_view
      IMPORTING
        !is_view   TYPE zal30_t_view
      EXPORTING
        !et_data   TYPE REF TO data
        !es_return TYPE bapiret2 .
    METHODS create_it_edit_data_view
      IMPORTING
        !is_view   TYPE zal30_t_view
      EXPORTING
        !et_data   TYPE REF TO data
        !es_return TYPE bapiret2 .
    METHODS save_data
      IMPORTING
        !is_view      TYPE zal30_t_view
      EXPORTING
        !es_return    TYPE bapiret2
      CHANGING
        !ct_datos_del TYPE STANDARD TABLE
        !ct_datos     TYPE STANDARD TABLE .
    METHODS verify_field_data
      IMPORTING
        !iv_fieldname    TYPE any
        !iv_value        TYPE any
        !it_fields       TYPE zal30_i_fields_alv
        !iv_exit_class   TYPE zal30_e_exit_class
      RETURNING
        VALUE(rs_return) TYPE bapiret2 .
    METHODS check_authorization
      IMPORTING
        !iv_view_name   TYPE tabname
        !iv_view_action TYPE any DEFAULT 'U'
      RAISING
        zcx_al30 .
    METHODS transport_entries
      IMPORTING
        !iv_name_view    TYPE tabname
        !it_keys         TYPE STANDARD TABLE
      RETURNING
        VALUE(rs_return) TYPE bapiret2 .
    METHODS verify_change_row_data
      IMPORTING
        !iv_exit_class   TYPE zal30_e_exit_class
      EXPORTING
        VALUE(es_return) TYPE bapiret2
      CHANGING
        !cs_row_data     TYPE any .
  PROTECTED SECTION.
*"* protected components of class ZCL_AL30_VIEW
*"* do not include other source files here!!!

    METHODS exit_after_save_data
      IMPORTING
        !it_datos      TYPE STANDARD TABLE
        !iv_exit_class TYPE zal30_e_exit_class
        !iv_error_save TYPE sap_bool .
    METHODS exit_before_save_data
      IMPORTING
        !iv_exit_class TYPE zal30_e_exit_class
      CHANGING
        !ct_datos      TYPE STANDARD TABLE .
    METHODS exit_save_data
      CHANGING
        !ct_datos TYPE STANDARD TABLE .
    METHODS reset_data
      CHANGING
        !ct_datos     TYPE STANDARD TABLE
        !ct_datos_del TYPE STANDARD TABLE .
    METHODS exit_check_auth_data_record
      IMPORTING
        !iv_exit_class TYPE zal30_e_exit_class
        !is_row_data   TYPE any
      RETURNING
        VALUE(rv_auth) TYPE sap_bool .
    METHODS exit_in_process_data_record
      IMPORTING
        !iv_exit_class TYPE zal30_e_exit_class
      CHANGING
        !cs_row_data   TYPE any .
    METHODS exit_before_process_data
      IMPORTING
        !iv_exit_class TYPE zal30_e_exit_class
      CHANGING
        !ct_data       TYPE STANDARD TABLE .
    METHODS exit_verify_field_data
      IMPORTING
        !iv_exit_class   TYPE zal30_e_exit_class
        !iv_fieldname    TYPE any
        !iv_value        TYPE any
      RETURNING
        VALUE(rs_return) TYPE bapiret2 .
    METHODS exit_verify_change_row_data
      IMPORTING
        !iv_exit_class   TYPE zal30_e_exit_class
      EXPORTING
        VALUE(es_return) TYPE bapiret2
      CHANGING
        !cs_row_data     TYPE any .
  PRIVATE SECTION.
*"* private components of class ZCL_AL30_VIEW
*"* do not include other source files here!!!
ENDCLASS.



CLASS zcl_al30_view IMPLEMENTATION.


  METHOD check_authorization.
    DATA ld_view_name TYPE dd25v-viewname.

    ld_view_name = iv_view_name.

* Lanzo la función estándar de verificacion que se encarga de comprabar si se tiene
* autorizacion para la accion a realizar
    CALL FUNCTION 'VIEW_AUTHORITY_CHECK'
      EXPORTING
        view_action                    = iv_view_action
        view_name                      = ld_view_name
        no_warning_for_clientindep     = ' '
      EXCEPTIONS
        invalid_action                 = 1
        no_authority                   = 2
        no_clientindependent_authority = 3
        table_not_found                = 4
        no_linedependent_authority     = 5
        OTHERS                         = 6.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_al30
        EXPORTING
          textid = zcx_al30=>no_authorization.
    ENDIF.

  ENDMETHOD.


  METHOD create_it_data_view.

    DATA lo_wa_view TYPE REF TO data.
    DATA lo_datos TYPE REF TO data.
    DATA lt_fcat TYPE lvc_t_fcat.
    DATA lt_fcat_control TYPE lvc_t_fcat.
    DATA ls_fcat TYPE LINE OF lvc_t_fcat.

    CLEAR: et_data, es_return.

* Creo la tabla interna dinámica en base a la vista/tabla indicada
    CALL METHOD zcl_ca_dynamic_tables=>create_it_from_struc
      EXPORTING
        i_struc = is_view-tabname
      IMPORTING
        e_table = et_data.

    IF et_data IS NOT BOUND.

      es_return = zcl_al30_data=>fill_return( iv_type = 'E' iv_number = '022' iv_message_v1 = is_view-tabname ).

    ENDIF.

  ENDMETHOD.


  METHOD create_it_edit_data_view.

    DATA lo_wa_view TYPE REF TO data.
    DATA lo_datos TYPE REF TO data.
    DATA lt_fcat TYPE lvc_t_fcat.
    DATA lt_fcat_control TYPE lvc_t_fcat.
    DATA ls_fcat TYPE LINE OF lvc_t_fcat.

    CLEAR: et_data, es_return.

* Creo la tabla interna dinámica en base a la vista/tabla indicada
    CALL METHOD zcl_ca_dynamic_tables=>create_wa_from_struc
      EXPORTING
        i_struc    = is_view-tabname
      IMPORTING
        e_workarea = lo_wa_view.

    IF lo_wa_view IS BOUND.

* Añado los campos de control
      lt_fcat_control = zcl_al30_data=>get_fcat_control_edit_view( ).
      APPEND LINES OF lt_fcat_control TO lt_fcat.

* Añado el campo de estilo
      ls_fcat-fieldname = zif_al30_data=>cv_field_style.
      ls_fcat-rollname = 'LVC_T_STYL'.
      APPEND ls_fcat TO lt_fcat.

* Ahora creo una nueva tabla interna en base a la de la vista + los campos nuevos.
      CALL METHOD zcl_ca_dynamic_tables=>create_it_fields_base_ref
        EXPORTING
          i_base_fields = lo_wa_view
          i_new_fields  = lt_fcat
        IMPORTING
          e_table       = et_data.

      IF et_data IS NOT BOUND.
        es_return = zcl_al30_data=>fill_return( iv_type = 'E' iv_number = '022' iv_message_v1 = is_view-tabname ).
      ENDIF.

    ELSE.

      es_return = zcl_al30_data=>fill_return( iv_type = 'E' iv_number = '022' iv_message_v1 = is_view-tabname ).

    ENDIF.

  ENDMETHOD.


  METHOD exit_after_save_data.

    DATA ld_metodo TYPE seocpdname.

* Monto el método al cual se llamará de la clase de exit.
    CONCATENATE zif_al30_data=>cv_intf_exit '~EXIT_AFTER_SAVE_DATA' INTO ld_metodo.

    TRY.
        CALL METHOD (iv_exit_class)=>(ld_metodo)
          EXPORTING
            it_data      = it_datos
            iv_error_save = iv_error_save.

      CATCH cx_root.
    ENDTRY.

  ENDMETHOD.


  METHOD exit_before_process_data.
    DATA ld_metodo TYPE seocpdname.

* Monto el método al cual se llamará de la clase de exit.
    CONCATENATE zif_al30_data=>cv_intf_exit '~EXIT_BEFORE_PROCESS_DATA' INTO ld_metodo.

    TRY.
        CALL METHOD (iv_exit_class)=>(ld_metodo)
          CHANGING
            ct_data = ct_data.

      CATCH cx_root.
    ENDTRY.

  ENDMETHOD.


  METHOD exit_before_save_data.

    DATA ld_metodo TYPE seocpdname.

* Monto el método al cual se llamará de la clase de exit.
    CONCATENATE zif_al30_data=>cv_intf_exit '~EXIT_BEFORE_SAVE_DATA' INTO ld_metodo.

    TRY.
        CALL METHOD (iv_exit_class)=>(ld_metodo)
          CHANGING
            ct_data = ct_datos.

      CATCH cx_root.
    ENDTRY.

  ENDMETHOD.


  METHOD exit_check_auth_data_record.
    DATA ld_metodo TYPE seocpdname.

* Monto el método al cual se llamará de la clase de exit.
    CONCATENATE zif_al30_data=>cv_intf_exit '~EXIT_CHECK_AUTH_DATA_RECORD' INTO ld_metodo.

* Por defecto tiene autorizacion
    rv_auth = abap_true.

    TRY.

        CALL METHOD (iv_exit_class)=>(ld_metodo)
          EXPORTING
            is_row_data       = is_row_data
          EXCEPTIONS
            no_authorization = 1
            OTHERS           = 2.
        IF sy-subrc NE 0.
          rv_auth = abap_false.
        ENDIF.
      CATCH cx_root.
    ENDTRY.

  ENDMETHOD.


  METHOD exit_in_process_data_record.
    DATA ld_metodo TYPE seocpdname.

* Monto el método al cual se llamará de la clase de exit.
    CONCATENATE zif_al30_data=>cv_intf_exit '~EXIT_IN_PROCESS_DATA_RECORD' INTO ld_metodo.

    TRY.
        CALL METHOD (iv_exit_class)=>(ld_metodo)
          CHANGING
            cs_row_data = cs_row_data.
      CATCH cx_root.
    ENDTRY.

  ENDMETHOD.


  METHOD exit_save_data.
  ENDMETHOD.


  METHOD exit_verify_change_row_data.
    DATA ld_metodo TYPE seocpdname.

* Monto el método al cual se llamará de la clase de exit.
    CONCATENATE zif_al30_data=>cv_intf_exit '~EXIT_VERIFY_CHANGE_ROW_DATA' INTO ld_metodo.

    TRY.
        CALL METHOD (iv_exit_class)=>(ld_metodo)
          IMPORTING
            es_return   = es_return
          CHANGING
            cs_row_data = cs_row_data.

      CATCH cx_root.
    ENDTRY.
  ENDMETHOD.


  METHOD exit_verify_field_data.
    DATA ld_metodo TYPE seocpdname.

* Monto el método al cual se llamará de la clase de exit.
    CONCATENATE zif_al30_data=>cv_intf_exit '~EXIT_VERIFY_FIELD_DATA' INTO ld_metodo.

    TRY.
        CALL METHOD (iv_exit_class)=>(ld_metodo)
          EXPORTING
            iv_fieldname = iv_fieldname
            iv_value     = iv_value
          RECEIVING
            rs_return    = rs_return.
      CATCH cx_root.
    ENDTRY.

  ENDMETHOD.


  METHOD get_fieldcat_edit_view.
    DATA ls_fieldcat TYPE LINE OF lvc_t_fcat.
    DATA lt_fieldcat TYPE lvc_t_fcat.

    CLEAR: es_return, et_fieldcat, et_fieldcat_key.

* El catalogo de campos para la edicición es el mismo que el de visualización pero
* añadiendole los campos de control de datos

    CALL METHOD get_fieldcat_view
      EXPORTING
        it_fields       = it_fields
        is_view         = is_view
      IMPORTING
        es_return       = es_return
        et_fieldcat     = et_fieldcat
        et_fieldcat_key = et_fieldcat_key.

    IF es_return-type IS INITIAL AND et_fieldcat IS NOT INITIAL.

* Obtengo el catalogo de campos de control
      lt_fieldcat = zcl_al30_data=>get_fcat_control_edit_view( ).
      APPEND LINES OF lt_fieldcat TO et_fieldcat.

    ENDIF.

  ENDMETHOD.


  METHOD get_fieldcat_view.


    FIELD-SYMBOLS <ls_fieldcat> TYPE LINE OF lvc_t_fcat.
    FIELD-SYMBOLS <ls_fields> TYPE LINE OF zal30_i_fields_alv.

    CLEAR: es_return, et_fieldcat, et_fieldcat_key.

* Recupero el catalogo de la tabla de campos
    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name       = is_view-tabname
      CHANGING
        ct_fieldcat            = et_fieldcat[]
      EXCEPTIONS
        inconsistent_interface = 1
        program_error          = 2
        OTHERS                 = 3.

    IF sy-subrc = 0 AND et_fieldcat IS NOT INITIAL.

      LOOP AT et_fieldcat ASSIGNING <ls_fieldcat>.
* Pongo que las columnas esten optimizadas. <- En el futuro sera un parámetro de la configuración
        <ls_fieldcat>-col_opt = abap_true.

* Los campos que no son claves se tratan como editables. <-- En el futuro sera parámetro de configuración
*      IF <ls_fieldcat>-key = abap_false.
        <ls_fieldcat>-edit = abap_true.
*      ENDIF.

* Pongo la configuración segun los campos de la configuración de la vista
        READ TABLE it_fields ASSIGNING <ls_fields> WITH KEY fieldname = <ls_fieldcat>-fieldname.
        IF sy-subrc = 0.
          <ls_fieldcat>-scrtext_s = <ls_fields>-scrtext_s.
          <ls_fieldcat>-scrtext_m = <ls_fields>-scrtext_m.
          <ls_fieldcat>-scrtext_l = <ls_fields>-scrtext_l.
          <ls_fieldcat>-reptext = <ls_fields>-reptext.
          <ls_fieldcat>-no_out = <ls_fields>-no_output.
          <ls_fieldcat>-tech = <ls_fields>-tech.
        ENDIF.

        IF <ls_fieldcat>-key = abap_true.
          APPEND <ls_fieldcat> TO et_fieldcat_key.
        ENDIF.

      ENDLOOP.

    ELSE.
      es_return = zcl_al30_data=>fill_return( iv_type = 'E' iv_number = '025' iv_message_v1 = is_view-tabname ).
    ENDIF.

  ENDMETHOD.


  METHOD read_data.
    FIELD-SYMBOLS <lt_datos> TYPE STANDARD TABLE.
    FIELD-SYMBOLS <ls_fields> TYPE LINE OF zal30_i_fields_alv.
    FIELD-SYMBOLS <ls_datos> TYPE any.
    FIELD-SYMBOLS <ls_data> TYPE any.
    FIELD-SYMBOLS <field> TYPE any.

    DATA lo_datos TYPE REF TO data.
    DATA lo_sql TYPE REF TO cl_sql_statement.
    DATA lo_result TYPE REF TO cl_sql_result_set.
    DATA lo_err TYPE REF TO cx_sql_exception.
    DATA lt_cols TYPE adbc_column_tab.
    DATA lo_et_datos TYPE REF TO data.
    DATA lo_et_data TYPE REF TO data.

    CLEAR: es_return, et_data.

    CALL METHOD zcl_ca_dynamic_tables=>create_it_from_struc
      EXPORTING
        i_struc = is_view-tabname
      IMPORTING
        e_table = lo_datos.

    IF lo_datos IS BOUND.

* Asigno la tabla a un puntero para poder trabajar con ella.
      ASSIGN lo_datos->* TO <lt_datos>.

* Creo una workarea para pasar los datos
      CREATE DATA lo_et_datos LIKE LINE OF <lt_datos>.
      ASSIGN lo_et_datos->* TO <ls_datos>.

* Creo una workarea para la tabla de salida (E_DATA)
      CREATE DATA lo_et_data LIKE LINE OF et_data.
      ASSIGN lo_et_data->* TO <ls_data>.

* Añado las columnas que quiero leer
      LOOP AT it_fields ASSIGNING <ls_fields>.
        APPEND <ls_fields>-fieldname TO lt_cols.
      ENDLOOP.
      IF sy-subrc = 0.

* NOTA IRB: Código de ejemplo extraido del report: DEMO_ADBC_QUERY

        CREATE OBJECT lo_sql.
* Nota: Si no se hubiese hecho la tabla interna dinámica con mi clase de utilidades, deberia
* crear de la tabla interna interna su referencia a un objeto.
*GET REFERENCE OF <lt_datos> INTO lo_datos.

        TRY.
            lo_result = lo_sql->execute_query(
             `SELECT * ` &&
             `FROM ` && is_view-tabname ).

            lo_result->set_param_table( itab_ref = lo_datos
                                     corresponding_fields = lt_cols ).

* Leo los datos y miro si tiene. Si tiene los devuelvo al parámetro de salida.
            IF lo_result->next_package( ) > 0.

* Exit antes de procesar los registros
              exit_before_process_data( EXPORTING iv_exit_class = is_view-exit_class
                                        CHANGING ct_data = <lt_datos> ).

* No haga una igualdad porque las estructuras pueden ser distintas. Por ello paso la informacion
* con un move-corresponding
              LOOP AT <lt_datos> ASSIGNING <ls_datos>.

* Exit para comprobar que se tenga autorizacion
                IF exit_check_auth_data_record( iv_exit_class = is_view-exit_class
                                                is_row_data = <ls_datos> ) = abap_true.
* Exit mientras se procesan los registros.
                  exit_in_process_data_record( EXPORTING iv_exit_class = is_view-exit_class
                                               CHANGING cs_row_data = <ls_datos> ).

                  MOVE-CORRESPONDING <ls_datos> TO <ls_data>.

* Si la tabla tiene el componentes de numero de línea original de la fila, que no necesariamente tiene que coincidir
* con el del ALV, que puede variar segun se insertar o borren líneas.
                  ASSIGN COMPONENT zif_al30_data=>cv_field_tabix_ddic OF STRUCTURE <ls_data> TO <field>.
                  IF sy-subrc = 0.
                    <field> = sy-tabix.
                  ENDIF.

                  APPEND <ls_data> TO et_data.
                ELSE.
                  es_return = zcl_al30_data=>fill_return( iv_type = 'I' iv_number = '040' ).
                ENDIF.
                CLEAR <ls_data>.
              ENDLOOP.
            ENDIF.

          CATCH cx_sql_exception INTO lo_err.
            es_return = zcl_al30_data=>fill_return( iv_type = 'E' iv_number = '024' iv_message_v1 = lo_err->get_text( ) ).
        ENDTRY.

      ELSE.
        es_return = zcl_al30_data=>fill_return( iv_type = 'E' iv_number = '023' iv_message_v1 = is_view-tabname ).
      ENDIF.


    ELSE.
      es_return = zcl_al30_data=>fill_return( iv_type = 'E' iv_number = '022' iv_message_v1 = is_view-tabname ).
    ENDIF.

  ENDMETHOD.


  METHOD reset_data.
    FIELD-SYMBOLS <field> TYPE any.
    FIELD-SYMBOLS <ls_wa> TYPE any.

* El reseto consta de borrar el campo de actualización y rellenar
* el campo de línea en el diccionario
    LOOP AT ct_datos ASSIGNING <ls_wa>.

* Campo de posicion en el dicionario
      ASSIGN COMPONENT zif_al30_data=>cv_field_tabix_ddic OF STRUCTURE <ls_wa> TO <field>.
      IF sy-subrc = 0.
        <field> = sy-tabix.
      ENDIF.

* Campo de actualizacion
      ASSIGN COMPONENT zif_al30_data=>cv_field_updkz OF STRUCTURE <ls_wa> TO <field>.
      IF sy-subrc = 0.
        <field> = space.
      ENDIF.

    ENDLOOP.

* Borro la tabla de datos
    CLEAR ct_datos_del.

  ENDMETHOD.


  METHOD save_data.

    DATA lo_lt_view TYPE REF TO data.
    DATA lo_ls_view TYPE REF TO data.
    DATA ld_cond TYPE string.
    DATA ld_borrado_ok TYPE sap_bool.
    DATA ld_hay_borrado TYPE sap_bool.
    DATA ld_save_error TYPE sap_bool.

    FIELD-SYMBOLS <lt_view> TYPE STANDARD TABLE.
    FIELD-SYMBOLS <ls_view> TYPE any.
    FIELD-SYMBOLS <ls_datos> TYPE any.

    CLEAR es_return.

* Creo una tabla interna igual a la de la vista para borrar/modificar/insertar los datos
    CALL METHOD zcl_ca_dynamic_tables=>create_it_from_struc
      EXPORTING
        i_struc    = is_view-tabname
      IMPORTING
        e_table    = lo_lt_view
        e_workarea = lo_ls_view.

    IF lo_lt_view IS BOUND AND lo_ls_view IS BOUND.

* Primero borro los datos
      ASSIGN lo_lt_view->* TO <lt_view>.
      ASSIGN lo_ls_view->* TO <ls_view>.

      LOOP AT ct_datos_del ASSIGNING <ls_datos>.
        MOVE-CORRESPONDING <ls_datos> TO <ls_view>.
        APPEND <ls_view> TO <lt_view>.
      ENDLOOP.

      IF sy-subrc = 0.
        ld_hay_borrado = abap_true.
        DELETE (is_view-tabname) FROM TABLE <lt_view>.
        IF sy-subrc = 0.
          ld_borrado_ok = abap_true.
        ELSE.
          ld_borrado_ok = abap_false.
        ENDIF.
      ELSE.
        ld_hay_borrado = abap_false.
      ENDIF.

* Si el borrado ha ido bien o no hay datos...
      IF ld_hay_borrado = abap_false
         OR ( ld_hay_borrado = abap_true AND ld_borrado_ok = abap_true ).

* ....Paso los datos que se insertarán/o modificarán.
* Es decir, aquellos cuyo campo de actualización esta informado.
        REFRESH <lt_view>.
        CONCATENATE zif_al30_data=>cv_field_updkz ' IS NOT INITIAL'
                    INTO ld_cond SEPARATED BY space.

        LOOP AT ct_datos ASSIGNING <ls_datos> WHERE (ld_cond).
          MOVE-CORRESPONDING <ls_datos> TO <ls_view>.
          APPEND <ls_view> TO <lt_view>.
        ENDLOOP.
        IF sy-subrc = 0.

* Exit antes de grabar los datos
          CALL METHOD exit_before_save_data
            EXPORTING
              iv_exit_class = is_view-exit_class
            CHANGING
              ct_datos      = <lt_view>.

* En vez de separar insercion o modificacion uso la sentencia MODIFY para que haga todo a la vez
          MODIFY (is_view-tabname) FROM TABLE <lt_view>.
          IF sy-subrc = 0.
            COMMIT WORK.

* Reseto los datos para utilizalos como si vienen del diccionario
            reset_data( CHANGING ct_datos = ct_datos
                                 ct_datos_del = ct_datos_del ).

          ELSE.
            ROLLBACK WORK.
            es_return = zcl_al30_data=>fill_return( iv_type = 'E' iv_number = '028' iv_message_v1 = is_view-tabname ).
          ENDIF.

* Miro si ha ido bien o mal la grabacion.
          IF es_return IS INITIAL.
            ld_save_error = abap_false.
          ELSE.
            ld_save_error = abap_true.
          ENDIF.

* Llamo a la exit después de grabar.
          CALL METHOD exit_after_save_data
            EXPORTING
              it_datos      = <lt_view>
              iv_exit_class = is_view-exit_class
              iv_error_save = ld_save_error.

        ELSE.
* Si no hay datos a insertar/modificar pero se han borrado datos actualizo la base de datos
          IF ld_hay_borrado = abap_true.
            COMMIT WORK.

* Reseto los datos para utilizalos como si vienen del diccionario
            reset_data( CHANGING ct_datos = ct_datos
                                 ct_datos_del = ct_datos_del ).

          ENDIF.
        ENDIF.

      ELSE.
        es_return = zcl_al30_data=>fill_return( iv_type = 'E' iv_number = '028' iv_message_v1 = is_view-tabname ).
      ENDIF.

    ENDIF.



  ENDMETHOD.


  METHOD transport_entries.
    DATA lt_e071k TYPE STANDARD TABLE OF e071k.
    DATA ls_e071k TYPE e071k.
    DATA lt_e071 TYPE STANDARD TABLE OF e071.
    DATA ls_e071 TYPE e071.
    DATA ld_contflag TYPE dd02l-contflag.
    DATA ld_category TYPE e070-korrdev.
    DATA ld_order TYPE e070-trkorr.
    DATA ld_task  TYPE e070-trkorr.
    FIELD-SYMBOLS <ls_key> TYPE any.

* Lleno los datos de la cabecera del transporte
    ls_e071-pgmid    = 'R3TR'.
    ls_e071-object   = 'TABU'.
    ls_e071-obj_name = iv_name_view.
    ls_e071-objfunc  = 'K'.
    APPEND ls_e071 TO lt_e071.

* Añado las entradas de los campos clave
    LOOP AT it_keys ASSIGNING <ls_key>.
      ls_e071k-pgmid      = 'R3TR'.
      ls_e071k-mastertype = 'TABU'.
      ls_e071k-object     = 'TABU'.
      ls_e071k-mastername = iv_name_view.
      ls_e071k-objname    = iv_name_view.
      ls_e071k-tabkey = <ls_key>.
      APPEND ls_e071k TO lt_e071k.
    ENDLOOP.

*..check category of table
    SELECT SINGLE contflag FROM dd02l INTO ld_contflag
             WHERE tabname  = iv_name_view
               AND as4local = 'A'.
    IF sy-subrc = 0 AND
       ( ld_contflag = 'C' OR ld_contflag = 'G').
      ld_category = 'CUST'.
    ELSE.
      ld_category = 'SYST'.
    ENDIF.

* Seleccion de la orden de transporte
    CALL FUNCTION 'TR_ORDER_CHOICE_CORRECTION'
      EXPORTING
        iv_category = ld_category
      IMPORTING
        ev_order    = ld_order
        ev_task     = ld_task
      EXCEPTIONS
        OTHERS      = 3.
    IF sy-subrc <> 0.
      EXIT.
    ENDIF.

* Añado los objetos a la orden
    CALL FUNCTION 'TR_APPEND_TO_COMM_OBJS_KEYS'
      EXPORTING
        wi_simulation         = ' '
        wi_suppress_key_check = ' '
        wi_trkorr             = ld_task
      TABLES
        wt_e071               = lt_e071
        wt_e071k              = lt_e071k
      EXCEPTIONS
        OTHERS                = 68.
    IF sy-subrc = 0.
      rs_return = zcl_al30_data=>fill_return( iv_type = 'S' iv_number = '036' iv_message_v1 = ld_order ).
    ELSE.
      rs_return = zcl_al30_data=>fill_return( iv_type = 'E' iv_number = '037' iv_message_v1 = ld_order ).
    ENDIF.

  ENDMETHOD.


  METHOD verify_change_row_data.

    CLEAR es_return.

* ----->
* Codigo mio
* <-----

* Si no hay error lanzo el cambio y verificacion de datos.
    IF es_return IS INITIAL.
      CALL METHOD exit_verify_change_row_data
        EXPORTING
          iv_exit_class = iv_exit_class
        IMPORTING
          es_return     = es_return
        CHANGING
          cs_row_data   = cs_row_data.

    ENDIF.
  ENDMETHOD.


  METHOD verify_field_data.
    FIELD-SYMBOLS <ls_fields> TYPE LINE OF zal30_i_fields_alv.

    CLEAR rs_return.

* Me posiciono en la configuración del campo pasado. Si no existe, en principio no deberia pasar, configuracion no se hace nada
    READ TABLE it_fields ASSIGNING <ls_fields> WITH KEY fieldname = iv_fieldname.
    IF sy-subrc = 0.

* Validación: Campo obligatorio
      IF <ls_fields>-mandatory = abap_true AND iv_value IS INITIAL.
        rs_return = zcl_al30_data=>fill_return( iv_type = 'E' iv_number = '030' ).
      ENDIF.

* Si no hay errores entra la verificacion de cliente
      IF rs_return IS INITIAL.
        rs_return = exit_verify_field_data(  iv_exit_class = iv_exit_class
                                            iv_fieldname = iv_fieldname
                                            iv_value = iv_value ).
      ENDIF.

    ENDIF.

  ENDMETHOD.
ENDCLASS.
