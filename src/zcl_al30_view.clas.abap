"! <p class="shorttext synchronized">AL30 - Operations of view</p>
CLASS zcl_al30_view DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
*"* public components of class ZCL_AL30_VIEW
*"* do not include other source files here!!!
    TYPE-POOLS adbc .

    TYPES:
      BEGIN OF ts_view_list,
        view_name TYPE zal30_t_view-tabname,
        view_desc TYPE dd02t-ddtext,
      END OF ts_view_list .
    TYPES:
      tt_view_list TYPE STANDARD TABLE OF ts_view_list WITH EMPTY KEY .

    "! <p class="shorttext synchronized">View List created</p>
    "! View list created. Useful for use in the F4.
    "! @parameter iv_langu | <p class="shorttext synchronized">Language</p>
    "! @parameter it_r_views | <p class="shorttext synchronized">Views to filter</p>
    "! @parameter et_view_list | <p class="shorttext synchronized">View list</p>
    METHODS view_list
      IMPORTING
        !iv_langu     TYPE sylangu DEFAULT sy-langu
        !it_r_views   TYPE zif_al30_data=>tt_r_tabname OPTIONAL
      EXPORTING
        !et_view_list TYPE tt_view_list .
    "! <p class="shorttext synchronized">Read data of view</p>
    METHODS read_data
      IMPORTING
        !is_filters TYPE zif_al30_data=>ts_filter_read_data
      EXPORTING
        !es_return  TYPE bapiret2
      CHANGING
        !co_data    TYPE REF TO data .
    "! <p class="shorttext synchronized">Get fieldcat for the display the view</p>
    METHODS get_fieldcat_view
      IMPORTING
        !iv_mode     TYPE char1
      EXPORTING
        !es_return   TYPE bapiret2
        !et_fieldcat TYPE lvc_t_fcat .
    "! <p class="shorttext synchronized">Create the internal table for display data of the view</p>
    METHODS create_it_data_view
      IMPORTING
        !iv_mode   TYPE char1
      EXPORTING
        !et_data   TYPE REF TO data
        !es_return TYPE bapiret2 .
    "! <p class="shorttext synchronized">Save the data in the view.</p>
    METHODS save_data
      IMPORTING
        !iv_allow_request TYPE sap_bool DEFAULT abap_false
      EXPORTING
        !et_return        TYPE bapiret2_t
      CHANGING
        !ct_datos_del     TYPE STANDARD TABLE
        !ct_datos         TYPE STANDARD TABLE
        !cv_order         TYPE e070-trkorr .
    "! <p class="shorttext synchronized">Check the contents of field</p>
    METHODS verify_field_data
      IMPORTING
        !iv_fieldname TYPE any
        !iv_value     TYPE any
      EXPORTING
        !es_return    TYPE bapiret2 .
    "! <p class="shorttext synchronized">Verify change row data + execute exit</p>
    "!
    "! @parameter et_return | <p class="shorttext synchronized">Return table</p>
    METHODS verify_change_row_data
      IMPORTING
        !iv_row          TYPE bapi_line
      EXPORTING
        VALUE(et_return) TYPE bapiret2_t
      CHANGING
        !cs_row_data     TYPE any .
    "! <p class="shorttext synchronized">Verify row data</p>
    "! This method enters both when modifying / inserting fields, and when recording data
    "! @parameter iv_row | <p class="shorttext synchronized">Row number</p>
    "! @parameter is_row_data | <p class="shorttext synchronized">Row data</p>
    "! @parameter iv_save_process | <p class="shorttext synchronized" >Enter in save process</p>
    "! @parameter et_return | <p class="shorttext synchronized">Return table</p>
    METHODS verify_row_data
      IMPORTING
        !iv_row          TYPE bapi_line
        !is_row_data     TYPE any
        !iv_save_process TYPE sap_bool DEFAULT abap_false
        !no_exit         TYPE sap_bool DEFAULT abap_false
      EXPORTING
        VALUE(et_return) TYPE bapiret2_t.
    "! <p class="shorttext synchronized">Verify data to be save</p>
    "! This exit is called when recording data
    "! @parameter it_data | <p class="shorttext synchronized">Data</p>
    "! @parameter it_data_del | <p class="shorttext synchronized">Data deleted</p>
    "! @parameter iv_save_process | <p class="shorttext synchronized" >Enter in save process</p>
    "! @parameter et_return | <p class="shorttext synchronized">return</p>
    METHODS verify_save_data
      IMPORTING
        it_data          TYPE STANDARD TABLE
        it_data_del      TYPE STANDARD TABLE
        !iv_save_process TYPE sap_bool DEFAULT abap_false
      EXPORTING
        et_return        TYPE bapiret2_t.

    "! <p class="shorttext synchronized">Check SAP authorization</p>
    METHODS check_sap_authorization
      IMPORTING
        !iv_view_name   TYPE tabname
        !iv_view_action TYPE any DEFAULT 'U'
      RAISING
        zcx_al30 .
    "! <p class="shorttext synchronized">Transport table entries</p>
    METHODS transport_entries
      IMPORTING
        !it_data         TYPE STANDARD TABLE
      CHANGING
        !cv_order        TYPE e070-trkorr
      RETURNING
        VALUE(rs_return) TYPE bapiret2 .
    "! <p class="shorttext synchronized">Returns if the view has marked authorization per user</p>
    METHODS view_have_user_auth
      IMPORTING
        !iv_view       TYPE tabname
      RETURNING
        VALUE(rv_have) TYPE sap_bool .
    "! <p class="shorttext synchronized">Returns if the view has marked authorization per SAP</p>
    METHODS view_have_sap_auth
      IMPORTING
        !iv_view       TYPE tabname
      RETURNING
        VALUE(rv_have) TYPE sap_bool .
    "! <p class="shorttext synchronized">Get level authorization of the view</p>
    METHODS get_level_auth_view
      IMPORTING
        !iv_view             TYPE tabname
        !iv_user             TYPE syuname DEFAULT sy-uname
      RETURNING
        VALUE(rv_level_auth) TYPE zal30_e_level_auth .
    "! <p class="shorttext synchronized">The view has the auto-tuning option checked</p>
    METHODS view_have_auto_adjust
      IMPORTING
        !iv_view       TYPE tabname
      RETURNING
        VALUE(rv_have) TYPE sap_bool .
    "! <p class="shorttext synchronized">Lock the view and the text table</p>
    METHODS lock_view
      IMPORTING
        !iv_view_name TYPE tabname OPTIONAL
        !iv_view_text TYPE tabname OPTIONAL
      RAISING
        zcx_al30 .
    "! <p class="shorttext synchronized">Instance the exit class</p>
    METHODS instance_exit_class
      IMPORTING
        !iv_exit_class   TYPE zal30_e_exit_class
      RETURNING
        VALUE(rs_return) TYPE bapiret2 .
    "! <p class="shorttext synchronized">Set the data configuration of the view</p>
    METHODS set_data_conf_view
      IMPORTING
        !it_fields_view      TYPE zif_al30_data=>tt_fields_view
        !it_fields_text_view TYPE zif_al30_data=>tt_fields_text_view
        !is_view             TYPE zal30_t_view
        !it_fields_ddic      TYPE dd03ptab .
    "! <p class="shorttext synchronized">Set editable mode the ALV Data</p>
    "!
    "! @parameter it_data | <p class="shorttext synchronized">Data</p>
    "! @parameter ev_edit_mode | <p class="shorttext synchronized">Edit mode</p>
    METHODS set_edit_mode_alv
      IMPORTING
        !it_data      TYPE STANDARD TABLE
      EXPORTING
        !ev_edit_mode TYPE cdchngind.

  PROTECTED SECTION.

    TYPES:
*"* protected components of class ZCL_AL30_VIEW
*"* do not include other source files here!!!
      BEGIN OF ts_main_change_log,
        tabname   TYPE tabname,
        tabkey    TYPE cdtabkey,
        chngind   TYPE cdchngind,
        fieldname TYPE fieldname,
        value_new TYPE cdfldvaln,
        value_old TYPE cdfldvalo,
      END OF ts_main_change_log .
    TYPES:
      tt_main_change_log TYPE STANDARD TABLE OF ts_main_change_log .

    DATA mo_exit_class TYPE REF TO object .
    DATA mt_fields TYPE zif_al30_data=>tt_fields_view .
    DATA mt_fields_text TYPE zif_al30_data=>tt_fields_text_view .
    DATA mt_fields_ddic TYPE dd03ptab .
    DATA ms_view TYPE zal30_t_view .
    DATA mo_original_data TYPE REF TO data .

    "! <p class="shorttext synchronized">Exit after save data</p>
    METHODS exit_after_save_data
      IMPORTING
        !it_data       TYPE STANDARD TABLE
        !it_data_del   TYPE STANDARD TABLE
        !iv_error_save TYPE sap_bool .
    "! <p class="shorttext synchronized">Exit before save data</p>
    METHODS exit_before_save_data
      EXPORTING
        !ev_abort_save TYPE sap_bool
        !et_return     TYPE bapiret2_t
      CHANGING
        !ct_data       TYPE STANDARD TABLE
        !ct_data_del   TYPE STANDARD TABLE .
    "! <p class="shorttext synchronized">update reset fields</p>
    METHODS reset_data
      CHANGING
        !ct_datos     TYPE STANDARD TABLE
        !ct_datos_del TYPE STANDARD TABLE .
    "! <p class="shorttext synchronized">Exit for check authorization in data read</p>
    METHODS exit_check_auth_data_read
      IMPORTING
        !is_row_data   TYPE any
      RETURNING
        VALUE(rv_auth) TYPE sap_bool .
    "! <p class="shorttext synchronized">Exit in data read process</p>
    METHODS exit_in_process_data_read
      CHANGING
        !cs_row_data TYPE any .
    "! <p class="shorttext synchronized">Exit after read data</p>
    METHODS exit_after_read_data
      CHANGING
        !ct_data TYPE STANDARD TABLE .
    "! <p class="shorttext synchronized">Exit before read data</p>
    METHODS exit_before_read_data
      CHANGING
        !ct_data TYPE STANDARD TABLE .
    "! <p class="shorttext synchronized">Exit for verify field data</p>
    METHODS exit_verify_field_data
      IMPORTING
        !iv_fieldname TYPE any
        !iv_value     TYPE any
      EXPORTING
        !es_return    TYPE bapiret2 .
    "! <p class="shorttext synchronized">Exit for verify and change row data</p>
    METHODS exit_verify_change_row_data
      IMPORTING
        !iv_row          TYPE bapi_line OPTIONAL
      EXPORTING
        VALUE(et_return) TYPE bapiret2_t
      CHANGING
        !cs_row_data     TYPE any .
    "! <p class="shorttext synchronized">Exit for verify row data</p>
    METHODS exit_verify_row_data
      IMPORTING
        !iv_row          TYPE bapi_line OPTIONAL
        !is_row_data     TYPE any
        !iv_save_process TYPE sap_bool DEFAULT abap_false
      EXPORTING
        VALUE(et_return) TYPE bapiret2_t.


    "! <p class="shorttext synchronized">Add fields of text table</p>
    METHODS add_fields_texttable
      IMPORTING
        !iv_view   TYPE tabname
        !it_fields TYPE zif_al30_data=>tt_fields_view
      CHANGING
        !ct_fcat   TYPE lvc_t_fcat .
    "! <p class="shorttext synchronized">Add edit fiels to the internal table</p>
    METHODS add_edit_fields
      CHANGING
        !ct_fcat TYPE lvc_t_fcat .
    "! <p class="shorttext synchronized">Get fieldcatalog of table and text view</p>
    METHODS get_lvc_fieldcat
      IMPORTING
        VALUE(is_view) TYPE zal30_t_view
      RETURNING
        VALUE(rt_fcat) TYPE lvc_t_fcat .
    "! <p class="shorttext synchronized">Exit for the field catalog</p>
    METHODS exit_process_catalog_of_field
      CHANGING
        VALUE(cs_fieldcat) TYPE lvc_s_fcat .
    "! <p class="shorttext synchronized">Create SQL for read</p>
    METHODS create_sql_read
      IMPORTING
        !is_filters TYPE zif_al30_data=>ts_filter_read_data
      EXPORTING
        !et_cols    TYPE adbc_column_tab
        !ev_sql     TYPE string .
    "! <p class="shorttext synchronized">Create join for the select to the text table</p>
    METHODS create_join_sql_texttable
      RETURNING
        VALUE(rv_result) TYPE string .
    "! <p class="shorttext synchronized">Lock table</p>
    METHODS lock_table
      IMPORTING
        !iv_table TYPE ocus-table
      RAISING
        zcx_al30 .
    "! <p class="shorttext synchronized">Unlock table</p>
    METHODS unlock_table
      IMPORTING
        !iv_table TYPE tabname .
    "! <p class="shorttext synchronized">Save data deleted</p>
    METHODS save_data_erased
      IMPORTING
        !iv_allow_request TYPE sap_bool
      EXPORTING
        !es_return        TYPE bapiret2
      CHANGING
        !ct_datos_del     TYPE STANDARD TABLE
        !cv_order         TYPE e070-trkorr .
    "! <p class="shorttext synchronized">Save changelog</p>
    METHODS save_change_log
      IMPORTING
        !it_datos     TYPE STANDARD TABLE
        !it_datos_del TYPE STANDARD TABLE .
    "! <p class="shorttext synchronized">Fill the change log</p>
    METHODS fill_change_log
      IMPORTING
        !it_data       TYPE STANDARD TABLE
      CHANGING
        !ct_change_log TYPE tt_main_change_log .
    "! <p class="shorttext synchronized">Convert data to changelog key</p>
    METHODS conv_data_2_changelog_key
      IMPORTING
        !is_data        TYPE any
        !it_fields_ddic TYPE dd03ptab
      RETURNING
        VALUE(rv_key)   TYPE cdtabkey .
    "! <p class="shorttext synchronized">Save the new data or update data</p>
    METHODS save_data_inup
      IMPORTING
        !it_data           TYPE STANDARD TABLE
        !iv_save_transport TYPE sap_bool
      EXPORTING
        !es_return         TYPE bapiret2
      CHANGING
        !cv_order          TYPE e070-trkorr
        !cv_save_error     TYPE sap_bool .
    "! <p class="shorttext synchronized">Fill change log values</p>
    METHODS fill_changelog_values
      IMPORTING
        !iv_tabname    TYPE zal30_t_view-tabname
        !iv_chngind    TYPE cdchngind
        !it_fields     TYPE dd03ptab
        !is_data       TYPE any
      CHANGING
        !ct_change_log TYPE zcl_al30_view=>tt_main_change_log .
    "! <p class="shorttext synchronized">Search row of the original data</p>
    METHODS search_row_original_data
      IMPORTING
        !is_data       TYPE any
        !iv_tabname    TYPE zal30_t_view-tabname
      EXPORTING
        VALUE(es_data) TYPE any .
    "! <p class="shorttext synchronized">Create where to the sql</p>
    METHODS create_where_sql
      IMPORTING
        !is_filters     TYPE zif_al30_data=>ts_filter_read_data
      RETURNING
        VALUE(rv_where) TYPE string .

    "! <p class="shorttext synchronized">Add virtual fields</p>
    "!
    "! @parameter it_fields | <p class="shorttext synchronized">Fields of view</p>
    "! @parameter ct_fcat | <p class="shorttext synchronized">Fieldcatalog</p>
    METHODS add_fields_virtual
      IMPORTING
        it_fields TYPE zif_al30_data=>tt_fields_view
      CHANGING
        ct_fcat   TYPE lvc_t_fcat .
    "! <p class="shorttext synchronized">field value ajust for SQL Where</p>
    "!
    "! @parameter iv_tabname | <p class="shorttext synchronized">Table name</p>
    "! @parameter iv_fieldname | <p class="shorttext synchronized">Field name</p>
    "! @parameter cv_value | <p class="shorttext synchronized">Value</p>
    METHODS field_value_adjust_sql
      IMPORTING
        iv_tabname   TYPE zal30_t_view-tabname
        iv_fieldname TYPE rsdstabs-prim_fname
      CHANGING
        cv_value     TYPE any.
    "! <p class="shorttext synchronized">Internal verification row data</p>
    "! This method enters both when modifying / inserting fields, and when recording data
    "! @parameter iv_row | <p class="shorttext synchronized">Row number</p>
    "! @parameter is_row_data | <p class="shorttext synchronized">Row data</p>
    "! @parameter et_return | <p class="shorttext synchronized">Return table</p>
    METHODS internal_verify_row_data
      IMPORTING
        iv_row      TYPE bapi_line
        is_row_data TYPE any
      EXPORTING
        et_return   TYPE bapiret2_t.
    "! <p class="shorttext synchronized">Exit for verify the data to be save</p>
    "! It allows the data to be recorded or deleted before performing the recording process
    "! @parameter it_data | <p class="shorttext synchronized">Data to be update or insert</p>
    "! @parameter it_data_del | <p class="shorttext synchronized">Data to be delete</p>
    "! @parameter et_return | <p class="shorttext synchronized">return</p>
    METHODS exit_verify_save_data
      IMPORTING
        it_data     TYPE STANDARD TABLE
        it_data_del TYPE STANDARD TABLE
      EXPORTING
        et_return   TYPE bapiret2_t.
    "! <p class="shorttext synchronized">Exit to set editable mode the ALV Data</p>
    "!
    "! @parameter it_data | <p class="shorttext synchronized">Data</p>
    "! @parameter ev_edit_mode | <p class="shorttext synchronized">Edit mode</p>
    METHODS exit_set_edit_mode_alv
      IMPORTING
        !it_data      TYPE STANDARD TABLE
      EXPORTING
        !ev_edit_mode TYPE cdchngind.
  PRIVATE SECTION.

*"* private components of class ZCL_AL30_VIEW
*"* do not include other source files here!!!
ENDCLASS.



CLASS zcl_al30_view IMPLEMENTATION.


  METHOD add_edit_fields.

* Se añaden los campos de control
    INSERT LINES OF zcl_al30_util=>get_fcat_control_edit_view( ) INTO TABLE ct_fcat.


* Se añade el campo de estilo
    INSERT VALUE #( fieldname = zif_al30_data=>cs_control_fields_alv_data-style rollname = 'LVC_T_STYL' ) INTO TABLE ct_fcat.

  ENDMETHOD.


  METHOD add_fields_texttable.

    " De los campos de la tabla de texto obtenemos sus campos para añadirlos al catalogo de campos
    LOOP AT it_fields ASSIGNING FIELD-SYMBOL(<ls_fields>) WHERE field_texttable = abap_true.
      READ TABLE mt_fields_ddic ASSIGNING FIELD-SYMBOL(<ls_dd03p>)
                                 WITH KEY tabname = iv_view
                                          fieldname = <ls_fields>-fieldname.

      IF sy-subrc = 0.
        INSERT VALUE #( fieldname = <ls_fields>-fieldname rollname = <ls_dd03p>-rollname ) INTO TABLE ct_fcat.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD add_fields_virtual.

    LOOP AT it_fields ASSIGNING FIELD-SYMBOL(<ls_fields>) WHERE virtual = abap_true.
      INSERT VALUE #( fieldname = <ls_fields>-fieldname rollname = <ls_fields>-virtual_dtel ) INTO TABLE ct_fcat.
    ENDLOOP.

  ENDMETHOD.


  METHOD check_sap_authorization.
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


  METHOD conv_data_2_changelog_key.
    DATA lv_max_len TYPE int4.
    DATA lv_start TYPE int4.

    DESCRIBE FIELD rv_key LENGTH DATA(lv_len_key) IN CHARACTER MODE.

    LOOP AT it_fields_ddic ASSIGNING FIELD-SYMBOL(<ls_fields>) WHERE keyflag = abap_true.

      ASSIGN COMPONENT <ls_fields>-fieldname OF STRUCTURE is_data TO FIELD-SYMBOL(<field>).
      IF sy-subrc = 0.

* Sumo la longitud del campo a la actual. Si esta supera la longitud del campo clave pongo
* un asterisco y salgo del proceso.
        lv_max_len = <ls_fields>-leng + lv_start.
        IF lv_max_len > lv_len_key.
          rv_key+lv_start(1) = '*'.
          EXIT.
        ENDIF.

* Si el campo que se lee es el mandante y esta en blanco le pongo el mandante actual para evitar que de errores.
        IF <ls_fields>-fieldname = 'MANDT' AND <field> IS INITIAL.
          rv_key+lv_start(<ls_fields>-leng) = sy-mandt.
        ELSE.
          rv_key+lv_start(<ls_fields>-leng) = <field>.
        ENDIF.

        ADD <ls_fields>-leng TO lv_start.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD create_it_data_view.

    DATA lo_wa_view TYPE REF TO data.
    DATA lo_datos TYPE REF TO data.
    DATA lt_fcat TYPE lvc_t_fcat.
    DATA lt_fcat_control TYPE lvc_t_fcat.
    DATA ls_fcat TYPE LINE OF lvc_t_fcat.

    CLEAR: et_data, es_return.

    " Creo la estructura en base a la vista/tabla indicada
    CALL METHOD zcl_ca_dynamic_tables=>create_wa_from_struc
      EXPORTING
        i_struc    = ms_view-tabname
      IMPORTING
        e_workarea = lo_wa_view.

    IF lo_wa_view IS BOUND.

      " Se añade los campos de la tabla de textos asociada a la vista.
      add_fields_texttable( EXPORTING iv_view = ms_view-texttable
                                      it_fields = mt_fields
                            CHANGING ct_fcat = lt_fcat ).

      " Se añade los campos virtuales
      add_fields_virtual( EXPORTING it_fields = mt_fields
                          CHANGING ct_fcat = lt_fcat ).

* En el modo edición se añaden los campos de control
      IF iv_mode = zif_al30_data=>cv_mode_change.
        add_edit_fields( CHANGING ct_fcat = lt_fcat ).
      ENDIF.

      CALL METHOD zcl_ca_dynamic_tables=>create_it_fields_base_ref
        EXPORTING
          i_base_fields = lo_wa_view
          i_new_fields  = lt_fcat
        IMPORTING
          e_table       = et_data.

      IF et_data IS NOT BOUND.
        es_return = zcl_al30_util=>fill_return( iv_type = zif_al30_data=>cs_msg_type-error iv_number = '018' iv_message_v1 = ms_view-tabname ).
      ENDIF.

    ELSE.

      es_return = zcl_al30_util=>fill_return( iv_type = zif_al30_data=>cs_msg_type-error iv_number = '018' iv_message_v1 = ms_view-tabname ).

    ENDIF.

  ENDMETHOD.


  METHOD create_join_sql_texttable.

    CLEAR rv_result.

    " Se obtiene las claves externas de la tabla de texto
    TRY.
        NEW zcl_al30_conf( )->read_single_view_ddic(
          EXPORTING
            iv_name_view = ms_view-texttable
          IMPORTING
            et_dd05m = DATA(lt_dd05m) ).

        " Se informan la condicion general
        DATA(lv_condition) = REDUCE string( INIT sql TYPE string FOR <ls_dd05m> IN lt_dd05m WHERE ( fortable = ms_view-texttable AND checktable = ms_view-tabname ) NEXT sql = sql &&
                              COND #( LET sep = 'AND' condition = |{ zif_al30_data=>cs_alias_sql-texttable }.{ <ls_dd05m>-forkey } = { zif_al30_data=>cs_alias_sql-view }.{ <ls_dd05m>-checkfield }|
                              IN WHEN sql IS NOT INITIAL THEN | { sep } { condition }| ELSE |{ condition }| ) ).

        " Se le suma el campo idioma de la tabla de textos
        READ TABLE mt_fields ASSIGNING FIELD-SYMBOL(<ls_fields>) WITH KEY lang_texttable = abap_true.
        IF sy-subrc = 0.
          lv_condition = |{ lv_condition } AND { zif_al30_data=>cs_alias_sql-texttable }.{ <ls_fields>-fieldname } = '{ sy-langu }'|.
        ENDIF.

        " Se construye la consulta completa
        rv_result = |LEFT OUTER JOIN { ms_view-texttable } { zif_al30_data=>cs_alias_sql-texttable } ON { lv_condition }|.


      CATCH zcx_al30.
    ENDTRY.

  ENDMETHOD.


  METHOD create_sql_read.

    CLEAR: et_cols.

    " Primero se rellena el nombre de las campos de la vista, excluyendo los virtuales.
    et_cols = VALUE #( FOR <ls_fields> IN mt_fields WHERE ( virtual = abap_false ) ( <ls_fields>-fieldname ) ).

    " Ahora se monta la SQL de búsqueda.
    ev_sql = 'SELECT'.

    " Se monta los campos de la búsqueda. En ambos casos se ignoraran los virtuales porque no existen en ninguna tabla
    DATA(lv_sql_fields) = REDUCE string( INIT sql TYPE string FOR <ls_fields> IN mt_fields WHERE ( virtual = abap_false )
                          NEXT sql = sql &&
                          COND #( LET sep = ',' field = COND string( WHEN <ls_fields>-field_texttable = abap_false THEN |{ zif_al30_data=>cs_alias_sql-view }.{ <ls_fields>-fieldname }|
                          ELSE |{ zif_al30_data=>cs_alias_sql-texttable }.{ <ls_fields>-fieldname }| )
                          IN WHEN sql IS NOT INITIAL THEN |{ sep } { field }| ELSE |{ field }| ) ).

    " Se monta la ordenación, mismo proceso que para montar los campos pero solo queremos los campos claves
    DATA(lv_order_fields) = REDUCE string( INIT sql TYPE string FOR <ls_fields> IN mt_fields WHERE ( key_ddic = abap_true AND virtual = abap_false )
                          NEXT sql = sql &&
                          COND #( LET sep = ',' field = COND string( WHEN <ls_fields>-field_texttable = abap_false THEN |{ zif_al30_data=>cs_alias_sql-view }.{ <ls_fields>-fieldname }|
                          ELSE |{ zif_al30_data=>cs_alias_sql-texttable }.{ <ls_fields>-fieldname }| )
                          IN WHEN sql IS NOT INITIAL THEN |{ sep } { field }| ELSE |{ field }| ) ).


    " Si hay tabla de texto se monta la union entre las dos tablas
    IF ms_view-texttable IS NOT INITIAL.
      DATA(lv_union_texttable) = create_join_sql_texttable( ).
    ENDIF.

    " Si los filtros están informados se genera la parte del where
    IF is_filters-where_clauses IS NOT INITIAL.
      DATA(lv_where) = create_where_sql( is_filters = is_filters ).
    ENDIF.

    " Se monta la SQL
    ev_sql = |{ ev_sql } { lv_sql_fields } FROM { ms_view-tabname } { zif_al30_data=>cs_alias_sql-view } { lv_union_texttable } |.

    " Se añade el where, si esta informado
    ev_sql = COND #( WHEN lv_where IS NOT INITIAL THEN |{ ev_sql } WHERE { lv_where }| ELSE ev_sql ).

    " Se añade el ORDER BY
    ev_sql = |{ ev_sql } ORDER BY { lv_order_fields }|.

  ENDMETHOD.


  METHOD create_where_sql.
    DATA lv_where TYPE string.

    CLEAR rv_where.

    " Los filtros se mueven a una variable local para poderlos manipular
    DATA(ls_filters) = is_filters.

    " Primer nivel son las tablas
    LOOP AT ls_filters-fields_ranges ASSIGNING FIELD-SYMBOL(<ls_tables>).

      " Según la tabla se monta el alias
      IF <ls_tables>-tablename = ms_view-tabname.
        DATA(lv_alias) = |{ zif_al30_data=>cs_alias_sql-view }.|.

        " Se guarda el nombre de la tabla para saber el tipo del campo para aplicar rutinas conversión
        DATA(lv_tabname) = ms_view-tabname.

      ELSE.
        lv_tabname = ms_view-texttable.
        lv_alias = |{ zif_al30_data=>cs_alias_sql-texttable }.|.
      ENDIF.

      " Segundo nivel los campos de la tabla
      LOOP AT <ls_tables>-frange_t ASSIGNING FIELD-SYMBOL(<ls_fields>).

        " Tercer nivel son los valores de los campos
        " Se mira el número de registros por si hay que añadir un OR
        DATA(lv_lines) = lines( <ls_fields>-selopt_t ).
        LOOP AT <ls_fields>-selopt_t ASSIGNING FIELD-SYMBOL(<ls_values>).
          DATA(lv_tabix) = sy-tabix.

          " Primero se monta el campo
          DATA(lv_field) = |{ lv_alias }{ <ls_fields>-fieldname }|.

          " Debido a que los campos con rutinas de conversión no vienen bien formateados por la función de SAP, hay que hacer
          " el ajuste manualmente para que funcione.
          field_value_adjust_sql( EXPORTING iv_tabname = lv_tabname
                                            iv_fieldname = <ls_fields>-fieldname
                                  CHANGING cv_value = <ls_values>-low ).

          " Ahora el operador y valor
          IF <ls_values>-option = 'BT'.
            IF <ls_values>-sign = 'E'.
              DATA(lv_operator) = |NOT BETWEEN '{ <ls_values>-low }' AND '{ <ls_values>-high }'|.
            ELSE.
              lv_operator = |BETWEEN '{ <ls_values>-low }' AND '{ <ls_values>-high }'|.
            ENDIF.
          ELSEIF <ls_values>-sign = 'E' OR <ls_values>-option = 'NE'. " Exclusión y NE se comportan igual
            lv_operator = |<> '{ <ls_values>-low }'|.
          ELSEIF <ls_values>-option = 'LE'.
            lv_operator = |<= '{ <ls_values>-low }'|.
          ELSEIF <ls_values>-option = 'LT'.
            lv_operator = |< '{ <ls_values>-low }'|.
          ELSEIF <ls_values>-option = 'GE'.
            lv_operator = |>= '{ <ls_values>-low }'|.
          ELSEIF <ls_values>-option = 'GT'.
            lv_operator = |> '{ <ls_values>-low }'|.
          ELSEIF <ls_values>-option = 'EQ'.
            lv_operator = |= '{ <ls_values>-low }'|.
          ELSEIF <ls_values>-option = 'CP'.
            lv_operator = |LIKE '{ <ls_values>-low }'|.
            REPLACE ALL OCCURRENCES OF '*' IN lv_operator WITH '%'.
          ENDIF.
          " Se monta la condición
          DATA(lv_cond) = |{ lv_field } { lv_operator }|.

          " Y se pone en el where
          lv_where = COND #( WHEN lv_where IS INITIAL THEN |( { lv_cond }| ELSE |{ lv_where } { lv_cond }| ).

          " Si el numero del registro es inferior al numero de líneas hay que añadir un OR
          IF lv_tabix < lv_lines.
            lv_where = |{ lv_where } OR|.
          ENDIF.
          CLEAR: lv_operator, lv_cond.

        ENDLOOP.
        IF sy-subrc = 0. " Si hay registros se cierra parentesis y se añade al where principal
          rv_where = COND #( WHEN rv_where IS INITIAL THEN |{ lv_where } )| ELSE |{ rv_where } AND { lv_where } ) | ).
          CLEAR: lv_where.
        ENDIF.

      ENDLOOP.
    ENDLOOP.


* Nota Iván: Este código funciona, aunque internamente hay muchos espacios en blanco y parentesis, pero solo en S/4 HANA
* En otras versiones de ABAP/SAP en el campo que tenga el elemento de datos LAND1 esta añadiendo un espacio en blanco en países
* de dos dígitos. Es decir, si se pone ES queda como 'ES ', esto hace que no encuentre datos. Este caso en S/4 HANA no ocurre
* Y como puede ser que este problema este en más campos y como esto tiene que ser multiversión se monta en SQL a manija.
* Nota Iván 2: Aquellos campos con rutinas de conversión no se formatea correctamente haciando que no encuentre datos. Por eso
* también se descarta este código
**    LOOP AT is_filters-where_clauses ASSIGNING FIELD-SYMBOL(<ls_tables>).
**
**      " Primero se construye el alias segun la tabla
**      IF <ls_tables>-tablename = ms_view-tabname.
**        DATA(lv_alias) = |{ zif_al30_data=>cs_alias_sql-view }.|.
**      ELSE.
**        lv_alias = |{ zif_al30_data=>cs_alias_sql-texttable }.|.
**      ENDIF.
**      " Se recorre las líneas del where de la tabla y se concatena en una
**      DATA(lv_where) = ||.
**      LOOP AT <ls_tables>-where_tab ASSIGNING FIELD-SYMBOL(<ls_where>).
**
**        lv_where = COND #( WHEN lv_where IS INITIAL THEN <ls_where>-line ELSE |{ lv_where } { <ls_where>-line }| ).
**
**      ENDLOOP.
**
**      " En la tabla del where los operadores lógicos vienen con código SAP, esto se tiene que cambiar ara
**      " hacer compatible con la sentencia SQL.
**      " Es decir, los EQ pasan a ser =. Lo mismo para los NE, LT, GT, LE o GE.
**      REPLACE ALL OCCURRENCES OF ' NE ' IN lv_where WITH ' <> '.
**      REPLACE ALL OCCURRENCES OF ' EQ ' IN lv_where WITH ' = '.
**      REPLACE ALL OCCURRENCES OF ' LE ' IN lv_where WITH ' <= '.
**      REPLACE ALL OCCURRENCES OF ' LT ' IN lv_where WITH ' < '.
**      REPLACE ALL OCCURRENCES OF ' GE ' IN lv_where WITH ' >= '.
**      REPLACE ALL OCCURRENCES OF ' GT ' IN lv_where WITH ' > '.
**
**      " Ahora recorro los campos que son filtros para reemplazar su valor por un campo que es el alias+campo
**      LOOP AT mt_fields ASSIGNING FIELD-SYMBOL(<ls_fields>) WHERE sel_screen = abap_true.
**        DATA(lv_field) = |{ lv_alias }{ <ls_fields>-fieldname }|.
**        REPLACE ALL OCCURRENCES OF <ls_fields>-fieldname IN lv_where WITH lv_field.
**      ENDLOOP.
**
**      " Si el where es global esta en blanco se informa poniendo un parántesis inicial
**      rv_where = COND #( WHEN rv_where IS INITIAL THEN | { lv_where }| ELSE |{ rv_where } { lv_where }| ).
**
**    ENDLOOP.

  ENDMETHOD.


  METHOD exit_after_read_data.
    DATA ld_metodo TYPE seocpdname.

    IF mo_exit_class IS BOUND.

* Monto el método al cual se llamará de la clase de exit.
      CONCATENATE zif_al30_data=>cv_intf_exit '~EXIT_AFTER_READ_DATA' INTO ld_metodo.

      TRY.
          CALL METHOD mo_exit_class->(ld_metodo)
            CHANGING
              ct_data = ct_data.

        CATCH cx_root.
      ENDTRY.

    ENDIF.

  ENDMETHOD.


  METHOD exit_after_save_data.

    DATA ld_metodo TYPE seocpdname.

    IF mo_exit_class IS BOUND.

* Monto el método al cual se llamará de la clase de exit.
      CONCATENATE zif_al30_data=>cv_intf_exit '~EXIT_AFTER_SAVE_DATA' INTO ld_metodo.

      TRY.
          CALL METHOD mo_exit_class->(ld_metodo)
            EXPORTING
              it_data       = it_data
              it_data_del   = it_data_del
              iv_error_save = iv_error_save.

        CATCH cx_root.
      ENDTRY.

    ENDIF.

  ENDMETHOD.


  METHOD exit_before_read_data.
    DATA ld_metodo TYPE seocpdname.

    IF mo_exit_class IS BOUND.

* Monto el método al cual se llamará de la clase de exit.
      CONCATENATE zif_al30_data=>cv_intf_exit '~EXIT_BEFORE_READ_DATA' INTO ld_metodo.

      TRY.
          CALL METHOD mo_exit_class->(ld_metodo)
            CHANGING
              ct_data = ct_data.

        CATCH cx_root.
      ENDTRY.

    ENDIF.

  ENDMETHOD.


  METHOD exit_before_save_data.

    DATA ld_metodo TYPE seocpdname.

    CLEAR: et_return,
           ev_abort_save.

    IF mo_exit_class IS BOUND.

* Monto el método al cual se llamará de la clase de exit.
      CONCATENATE zif_al30_data=>cv_intf_exit '~EXIT_BEFORE_SAVE_DATA' INTO ld_metodo.

      TRY.
          CALL METHOD mo_exit_class->(ld_metodo)
            IMPORTING
              ev_abort_save = ev_abort_save
              et_return     = et_return
            CHANGING
              ct_data       = ct_data
              ct_data_del   = ct_data_del.

          " Los mensajes de return que no tengan el message informado se llama al proceso para completarlo
          LOOP AT et_return REFERENCE INTO DATA(lo_return) WHERE message IS INITIAL.
            lo_return->message = zcl_al30_util=>fill_return( iv_type = lo_return->type
                                                             iv_id = lo_return->id
                                                             iv_number = lo_return->number
                                                             iv_message_v1 = lo_return->message_v1
                                                             iv_message_v2 = lo_return->message_v2
                                                             iv_message_v3 = lo_return->message_v3
                                                             iv_message_v4 = lo_return->message_v4 )-message.
          ENDLOOP.

        CATCH cx_root.
      ENDTRY.

    ENDIF.

  ENDMETHOD.


  METHOD exit_check_auth_data_read.
    DATA ld_metodo TYPE seocpdname.

* Por defecto tiene autorizacion
    rv_auth = abap_true.

    IF mo_exit_class IS BOUND.

* Monto el método al cual se llamará de la clase de exit.
      CONCATENATE zif_al30_data=>cv_intf_exit '~EXIT_CHECK_AUTH_DATA_READ' INTO ld_metodo.


      TRY.

          CALL METHOD mo_exit_class->(ld_metodo)
            EXPORTING
              is_row_data      = is_row_data
            EXCEPTIONS
              no_authorization = 1
              OTHERS           = 2.
          IF sy-subrc NE 0.
            rv_auth = abap_false.
          ENDIF.
        CATCH cx_root.
      ENDTRY.

    ENDIF.

  ENDMETHOD.


  METHOD exit_in_process_data_read.
    DATA ld_metodo TYPE seocpdname.

    IF mo_exit_class IS BOUND.

* Monto el método al cual se llamará de la clase de exit.
      CONCATENATE zif_al30_data=>cv_intf_exit '~EXIT_IN_PROCESS_DATA_READ' INTO ld_metodo.

      TRY.
          CALL METHOD mo_exit_class->(ld_metodo)
            CHANGING
              cs_row_data = cs_row_data.
        CATCH cx_root.
      ENDTRY.

    ENDIF.

  ENDMETHOD.


  METHOD exit_process_catalog_of_field.

    IF mo_exit_class IS BOUND.
      " Monto el método al cual se llamará de la clase de exit.
      DATA(lv_metodo) = |{ zif_al30_data=>cv_intf_exit }~EXIT_PROCESS_CATALOG_OF_FIELD|.

      TRY.
          CALL METHOD mo_exit_class->(lv_metodo)
            CHANGING
              cs_fieldcat = cs_fieldcat.
        CATCH cx_root.
      ENDTRY.
    ENDIF.

  ENDMETHOD.


  METHOD exit_verify_change_row_data.
    DATA ld_metodo TYPE seocpdname.

    CLEAR: et_return.

    IF mo_exit_class IS BOUND.

* Monto el método al cual se llamará de la clase de exit.
      CONCATENATE zif_al30_data=>cv_intf_exit '~EXIT_VERIFY_CHANGE_ROW_DATA' INTO ld_metodo.

      TRY.
          CALL METHOD mo_exit_class->(ld_metodo)
            EXPORTING
              iv_row      = iv_row
            IMPORTING
              et_return   = et_return
            CHANGING
              cs_row_data = cs_row_data.

        CATCH cx_root.
      ENDTRY.

    ENDIF.

  ENDMETHOD.


  METHOD exit_verify_field_data.
    DATA ld_metodo TYPE seocpdname.

    CLEAR: es_return.

    IF mo_exit_class IS BOUND.

* Monto el método al cual se llamará de la clase de exit.
      CONCATENATE zif_al30_data=>cv_intf_exit '~EXIT_VERIFY_FIELD_DATA' INTO ld_metodo.

      TRY.
          CALL METHOD mo_exit_class->(ld_metodo)
            EXPORTING
              iv_fieldname = iv_fieldname
              iv_value     = iv_value
            IMPORTING
              es_return    = es_return.
        CATCH cx_root.
      ENDTRY.

    ENDIF.

  ENDMETHOD.


  METHOD fill_changelog_values.

    FIELD-SYMBOLS <ls_original_data> TYPE any.
    DATA lo_orig_data TYPE REF TO data.
    DATA ls_changelog TYPE LINE OF zcl_al30_view=>tt_main_change_log .


    " Se crea una estructura del mismo tipo que la estructura de entrada, que es la misma que la de los datos originales
    CREATE DATA lo_orig_data LIKE is_data .
    ASSIGN lo_orig_data->* TO <ls_original_data>.


    " Si la operación es de actualización hay que buscar el registro original al que se esta modificando para poder poner el valor original
    IF iv_chngind = zif_al30_data=>cv_mode_change.
      search_row_original_data( EXPORTING is_data = is_data
                                          iv_tabname = iv_tabname
                                IMPORTING es_data = <ls_original_data> ).
    ENDIF.

    " Se rellenan los campos fijos
    ls_changelog-tabkey = conv_data_2_changelog_key( is_data        = is_data
                                                     it_fields_ddic = it_fields ).
    ls_changelog-tabname = iv_tabname.
    ls_changelog-chngind = iv_chngind.

    IF iv_chngind NE zif_al30_data=>cv_mode_delete.
      " Si no se esta borrando se recorre los campos, ignorando el mandante, para poder informar el valor antiguo y el nuevo
      LOOP AT it_fields ASSIGNING FIELD-SYMBOL(<ls_fields>) WHERE datatype NE zif_al30_data=>cs_datatype-mandt.

        " Se obtiene el valor del campo antiguo y nuevo
        ASSIGN COMPONENT <ls_fields>-fieldname OF STRUCTURE is_data TO FIELD-SYMBOL(<value>).
        ASSIGN COMPONENT <ls_fields>-fieldname OF STRUCTURE <ls_original_data> TO FIELD-SYMBOL(<value_orig>).

        " Si los valores son distintos entonces se añade el campo a la modificación
        IF <value> NE <value_orig>.

          ls_changelog-fieldname = <ls_fields>-fieldname.
          ls_changelog-value_new  = <value>.
          ls_changelog-value_old  = <value_orig>.

          INSERT ls_changelog INTO TABLE ct_change_log.

        ENDIF.

      ENDLOOP.
      IF sy-subrc NE 0. " Si no hay campos
        INSERT ls_changelog INTO TABLE ct_change_log.
      ENDIF.
    ELSE. " En borrado no se compara campos porque se borra el registro
      INSERT ls_changelog INTO TABLE ct_change_log.
    ENDIF.

  ENDMETHOD.


  METHOD fill_change_log.

    DATA ls_changelog TYPE LINE OF zcl_al30_view=>tt_main_change_log .

    " Saco los campos clave de la tabla principal y la de textos, si la tiene.
    DATA(lt_fields) = VALUE dd03ptab( FOR <ls_fields> IN mt_fields_ddic WHERE ( tabname = ms_view-tabname ) ( <ls_fields> ) ).

    IF ms_view-texttable IS NOT INITIAL.
      DATA(lt_fields_texttable) = VALUE dd03ptab( FOR <ls_fields> IN mt_fields_ddic WHERE ( tabname = ms_view-texttable ) ( <ls_fields> ) ).
    ENDIF.

    LOOP AT it_data ASSIGNING FIELD-SYMBOL(<ls_data>).

      ASSIGN COMPONENT zif_al30_data=>cs_control_fields_alv_data-updkz OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<updkz>). " Este campo debe existir, si no existe que pegue dump

      fill_changelog_values( EXPORTING iv_tabname = ms_view-tabname
                                       iv_chngind = CONV #( <updkz> )
                                       it_fields = lt_fields
                                       is_data = <ls_data>
                             CHANGING ct_change_log = ct_change_log ).

      " Si hay tabla de texto se hace lo mismo con ella
      IF ms_view-texttable IS NOT INITIAL.

        fill_changelog_values( EXPORTING iv_tabname = ms_view-texttable
                                         iv_chngind = CONV #( <updkz> )
                                         it_fields = lt_fields_texttable
                                         is_data = <ls_data>
                               CHANGING ct_change_log = ct_change_log ).

      ENDIF.


    ENDLOOP.

  ENDMETHOD.


  METHOD get_fieldcat_view.


    CLEAR: es_return, et_fieldcat.

* Se recupera el catalogo de campos de la tabla, y su posible tabla de texto
    et_fieldcat = get_lvc_fieldcat( is_view = ms_view ).

    IF sy-subrc = 0 AND et_fieldcat IS NOT INITIAL.

      LOOP AT et_fieldcat ASSIGNING FIELD-SYMBOL(<ls_fieldcat>).
* Pongo que las columnas esten optimizadas. <- En el futuro sera un parámetro de la configuración
        <ls_fieldcat>-col_opt = abap_true.

* Los campos que no son claves se tratan como editables. <-- En el futuro sera parámetro de configuración
*      IF <ls_fieldcat>-key = abap_false.
        <ls_fieldcat>-edit = abap_true.
*      ENDIF.

        READ TABLE mt_fields ASSIGNING FIELD-SYMBOL(<ls_fields>) WITH KEY fieldname = <ls_fieldcat>-fieldname.
        IF sy-subrc = 0.
          <ls_fieldcat>-no_out = <ls_fields>-no_output.
          <ls_fieldcat>-tech = <ls_fields>-tech.
          <ls_fieldcat>-col_pos = <ls_fields>-pos_ddic.
          <ls_fieldcat>-col_opt = abap_true.
          <ls_fieldcat>-checkbox = <ls_fields>-checkbox.
*          <ls_fieldcat>-key = <ls_fields>-key_ddic.

          " El campo de idioma de la tabla de texto se pone como técnico porque se autoinformará
          IF <ls_fields>-lang_texttable = abap_true.
            <ls_fieldcat>-tech = abap_true.
          ENDIF.
        ENDIF.

* Pongo la configuración segun los campos de la configuración de la vista
        READ TABLE mt_fields_text ASSIGNING FIELD-SYMBOL(<ls_fields_text>) WITH KEY fieldname = <ls_fieldcat>-fieldname.
        IF sy-subrc = 0.
          <ls_fieldcat>-scrtext_s = <ls_fields_text>-scrtext_s.
          <ls_fieldcat>-scrtext_m = <ls_fields_text>-scrtext_m.
          <ls_fieldcat>-scrtext_l = <ls_fields_text>-scrtext_l.
          <ls_fieldcat>-reptext = <ls_fields_text>-reptext.
        ENDIF.

* Exit para poder cambiar el catalogo de campos
        exit_process_catalog_of_field( CHANGING cs_fieldcat = <ls_fieldcat> ).

      ENDLOOP.

    ELSE.
      es_return = zcl_al30_util=>fill_return( iv_type = zif_al30_data=>cs_msg_type-error iv_number = '012' iv_message_v1 = ms_view-tabname ).
    ENDIF.

  ENDMETHOD.


  METHOD get_level_auth_view.

    rv_level_auth = zif_al30_data=>cs_level_auth_user-non. " Por defecto no se tiene autorizacion

    SELECT SINGLE level_auth INTO rv_level_auth
           FROM zal30_t_usr_auth
           WHERE tabname = iv_view
                 AND username = iv_user.

  ENDMETHOD.


  METHOD get_lvc_fieldcat.
    DATA lt_fcat_text TYPE lvc_t_fcat.

    CLEAR rt_fcat.

* Primero se recupera el de la tabla principal
    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name       = is_view-tabname
        i_bypassing_buffer     = abap_true
      CHANGING
        ct_fieldcat            = rt_fcat[]
      EXCEPTIONS
        inconsistent_interface = 1
        program_error          = 2
        OTHERS                 = 3.

    IF sy-subrc = 0.
      IF is_view-texttable IS NOT INITIAL.

        CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
          EXPORTING
            i_structure_name       = is_view-texttable
            i_bypassing_buffer     = abap_true
          CHANGING
            ct_fieldcat            = lt_fcat_text[]
          EXCEPTIONS
            inconsistent_interface = 1
            program_error          = 2
            OTHERS                 = 3.
        IF sy-subrc = 0.
          " Las posiciones de los campos de la tabla de texto serán una más a la ultima posición de los campos de la tabla
          " principal
          DATA(lv_pos) = lines( rt_fcat ) + 1.

          " Los campos de textos que existen en la tabla principal no se añaden.
          LOOP AT lt_fcat_text ASSIGNING FIELD-SYMBOL(<ls_fcat_text>).
            READ TABLE rt_fcat TRANSPORTING NO FIELDS WITH KEY fieldname = <ls_fcat_text>-fieldname.
            IF sy-subrc NE 0.
              <ls_fcat_text>-col_pos = lv_pos.
              INSERT <ls_fcat_text> INTO TABLE rt_fcat.
              lv_pos = lv_pos + 1.
            ENDIF.
          ENDLOOP.

        ENDIF.

      ENDIF.

      " Se añaden los campos virtual al catalogo
      DATA(lo_conf) = NEW zcl_al30_conf(  ).
      LOOP AT mt_fields ASSIGNING FIELD-SYMBOL(<ls_fields>) WHERE virtual = abap_true.
        TRY.
            lo_conf->read_single_data_element( EXPORTING iv_rollname = <ls_fields>-virtual_dtel
                                                         iv_langu    = sy-langu
                                               IMPORTING es_info     = DATA(ls_info) ).

            " Los campos obtenidos del catalogo del diccionario se pasan a la estructura del catalogo. Practicamente todos
            " los campos necesarios: textos, elementos datos, rutinas conversión, etc.. son iguales
            DATA(ls_fcat) = CORRESPONDING lvc_s_fcat( ls_info ).
            ls_fcat-fieldname = <ls_fields>-fieldname.
            ls_fcat-col_pos = lv_pos.
            INSERT ls_fcat INTO TABLE rt_fcat.

            lv_pos = lv_pos + 1.

            CLEAR ls_info.

          CATCH zcx_al30.
        ENDTRY.

      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD instance_exit_class.

    CLEAR: rs_return.

    " Instancia de la clase para la exit
    TRY.
        CREATE OBJECT mo_exit_class TYPE (iv_exit_class).
      CATCH cx_root.
        rs_return = zcl_al30_util=>fill_return( iv_type = zif_al30_data=>cs_msg_type-error iv_number = '026' iv_message_v1 = iv_exit_class ).
    ENDTRY..

  ENDMETHOD.


  METHOD lock_table.
    CALL FUNCTION 'VIEW_ENQUEUE'
      EXPORTING
        action               = 'E'
        enqueue_mode         = 'E'
        view_name            = iv_table
      EXCEPTIONS
        client_reference     = 1
        foreign_lock         = 2
        invalid_action       = 3
        invalid_enqueue_mode = 4
        system_failure       = 5
        table_not_found      = 6
        OTHERS               = 7.
    IF sy-subrc NE 0.

      " Según la excepción se lanza un mensaje distinto.
      CASE sy-subrc.
        WHEN 2. " Si ya esta bloqueada saco un aviso y se cambio a visualización.
          MESSAGE s049(sv) WITH sy-msgv1(12) INTO DATA(lv_message).
        WHEN 5.
          MESSAGE e050(sv) WITH iv_table INTO lv_message.
        WHEN 6.
          MESSAGE e028(sv) WITH iv_table INTO lv_message.
        WHEN 1.
          MESSAGE e054(sv) WITH iv_table INTO lv_message.
      ENDCASE.

      " Se lanza la excepción con el mensaje
      RAISE EXCEPTION TYPE zcx_al30
        EXPORTING
          textid     = zcx_al30=>view_locked
          mv_message = lv_message
          mv_msgv1   = CONV #( sy-msgv1(12) ).
    ENDIF.
  ENDMETHOD.


  METHOD lock_view.


    TRY.
        DATA(lv_tabname) = COND #( WHEN iv_view_name IS INITIAL THEN ms_view-tabname ELSE iv_view_name  ).
        DATA(lv_texttable) = COND #( WHEN iv_view_text IS INITIAL THEN ms_view-texttable ELSE iv_view_text  ).


        " Se bloquea la tabla principal
        lock_table( iv_table = lv_tabname ).

        " Si falla el bloqueo de la tabla de texto hay que desbloquear la principal para no dejarla bloqueada
        DATA(lv_main_lock) = abap_true.

        " Si hay tabla de textos se bloquea también
        IF lv_texttable IS NOT INITIAL.
          lock_table( iv_table = lv_texttable ).
        ENDIF.


      CATCH zcx_al30 INTO DATA(lx_excep).

        " Si se produce una excepción y la tabla principal se ha bloqueado habrá que desbloquearla
        IF lv_main_lock = abap_true.
          unlock_table( iv_table = lv_tabname ).
        ENDIF.

        DATA(lv_excep) = abap_true.
    ENDTRY.

    " Se vuelvo a lanzar la excepción una vez eliminado los bloqueos
    IF lv_excep = abap_true.

      RAISE EXCEPTION TYPE zcx_al30
        EXPORTING
          textid     = zcx_al30=>view_locked
          mv_message = lx_excep->mv_message.

    ENDIF.

  ENDMETHOD.


  METHOD read_data.
    FIELD-SYMBOLS <lt_datos> TYPE STANDARD TABLE.
    FIELD-SYMBOLS <lt_datos_orig> TYPE STANDARD TABLE.

    CLEAR: es_return.


    ASSIGN co_data->* TO <lt_datos>.

* Se crea la sentencia SQL a ejecutar
    create_sql_read( EXPORTING is_filters = is_filters
                     IMPORTING et_cols = DATA(lt_cols)
                               ev_sql = DATA(lv_sql) ).
* NOTA IRB: Código de ejemplo extraido del report: DEMO_ADBC_QUERY

    TRY.

        " Se lanza la SQL
        DATA(lo_result) = NEW cl_sql_statement( )->execute_query( lv_sql ).

        " Se indica donde se guardarán los datos y que columnas se quiere
        lo_result->set_param_table( itab_ref = co_data corresponding_fields = lt_cols ).

        "Se leen los datos
        IF lo_result->next_package( ) > 0.


          " Exit antes de procesar los registros
          exit_before_read_data( CHANGING ct_data = <lt_datos> ).

          " Se recorren los datos para procesarlo
          LOOP AT <lt_datos> ASSIGNING FIELD-SYMBOL(<ls_datos>).
            DATA(lv_tabix) = sy-tabix.

            " Exit para comprobar que se tenga autorizacion
            IF exit_check_auth_data_read( EXPORTING is_row_data = <ls_datos> ) = abap_true.

              " Para saber que el registro viene del diccionario se le informa la posición original que contiene.
              ASSIGN COMPONENT zif_al30_data=>cs_control_fields_alv_data-is_dict OF STRUCTURE <ls_datos> TO FIELD-SYMBOL(<field>).
              IF sy-subrc = 0.
                <field> = abap_true.
              ENDIF.

              ASSIGN COMPONENT zif_al30_data=>cs_control_fields_alv_data-tabix OF STRUCTURE <ls_datos> TO <field>.
              IF sy-subrc = 0.
                <field> = lv_tabix.
              ENDIF.

              " Exit mientras se procesan los registros.
              exit_in_process_data_read( CHANGING cs_row_data = <ls_datos> ).

            ELSE. " Si no tiene se borra el registro
              DELETE <lt_datos> INDEX lv_tabix.
            ENDIF.

          ENDLOOP.

          " Lectura una vez leídos los datos
          exit_after_read_data( CHANGING ct_data = <lt_datos> ).

        ENDIF.

        " Se crea una tabla con los datos originales de la tabla
        CREATE DATA mo_original_data LIKE <lt_datos>.
        ASSIGN mo_original_data->* TO <lt_datos_orig>.
        INSERT LINES OF <lt_datos> INTO TABLE <lt_datos_orig>.

      CATCH cx_sql_exception INTO DATA(lo_err).
        es_return = zcl_al30_util=>fill_return( iv_type = zif_al30_data=>cs_msg_type-error iv_number = '020' iv_message_v1 = lo_err->get_text( ) ).
      CATCH cx_parameter_invalid  INTO DATA(lo_err2).
        es_return = zcl_al30_util=>fill_return( iv_type = zif_al30_data=>cs_msg_type-error iv_number = '020' iv_message_v1 = lo_err2->get_text( ) ).
    ENDTRY.



  ENDMETHOD.


  METHOD reset_data.


* El reseto consta de borrar el campo de actualización y rellenar
* el campo de línea en el diccionario
    LOOP AT ct_datos ASSIGNING FIELD-SYMBOL(<ls_wa>).

      " Campo de posicion
      ASSIGN COMPONENT zif_al30_data=>cs_control_fields_alv_data-tabix OF STRUCTURE <ls_wa> TO FIELD-SYMBOL(<field>).
      IF sy-subrc = 0.
        <field> = sy-tabix.
      ENDIF.

      " El registro ya no existe en el diccionario
      ASSIGN COMPONENT zif_al30_data=>cs_control_fields_alv_data-is_dict OF STRUCTURE <ls_wa> TO <field>.
      IF sy-subrc = 0.
        <field> = abap_false.
      ENDIF.

* Campo de actualizacion
      ASSIGN COMPONENT zif_al30_data=>cs_control_fields_alv_data-updkz OF STRUCTURE <ls_wa> TO <field>.
      IF sy-subrc = 0.
        <field> = space.
      ENDIF.

    ENDLOOP.

* Borro la tabla de datos
    CLEAR ct_datos_del.

  ENDMETHOD.


  METHOD save_change_log.
    DATA lt_change_log TYPE STANDARD TABLE OF zal30_t_chng_log.

    DATA lt_main_change_log TYPE tt_main_change_log.

    IF it_datos_del IS NOT INITIAL.
      fill_change_log( EXPORTING it_data = it_datos_del
                              CHANGING ct_change_log = lt_main_change_log ).
    ENDIF.

    " Se rellena el log para los campos que se insertan(IN) y/o actualizan
    IF it_datos IS NOT INITIAL.
      fill_change_log( EXPORTING it_data = it_datos
                              CHANGING ct_change_log = lt_main_change_log ).
    ENDIF.


    " Si hay datos se pasán a la tabla definitida para insertar los valores
    IF lt_main_change_log IS NOT INITIAL.
      LOOP AT lt_main_change_log ASSIGNING FIELD-SYMBOL(<ls_main>).
        APPEND INITIAL LINE TO lt_change_log ASSIGNING FIELD-SYMBOL(<ls_change_log>).
        <ls_change_log> = CORRESPONDING #( <ls_main> ).
        <ls_change_log>-viewname = ms_view-tabname. " Vista principal
        <ls_change_log>-seqnr = sy-tabix. " Contador
        <ls_change_log>-udate = sy-datum. " Fecha
        <ls_change_log>-utime = sy-uzeit. " Hora
        <ls_change_log>-username = sy-uname. " Usuario
      ENDLOOP.

      " Se añaden los datos. Como se añaden los datos a la tabla no se hace control verificación porque no fallará.
      MODIFY zal30_t_chng_log FROM TABLE lt_change_log.
      IF sy-subrc = 0.
        COMMIT WORK AND WAIT.
      ENDIF.

    ENDIF.


  ENDMETHOD.


  METHOD save_data.
    FIELD-SYMBOLS <lt_original_data> TYPE STANDARD TABLE.
    FIELD-SYMBOLS <lt_datos> TYPE STANDARD TABLE.
    DATA lo_datos TYPE REF TO data.


    CLEAR: et_return.

    " Se crea una tabla como la de entrada para pasarla con los datos modificados a las exit
    CREATE DATA lo_datos LIKE ct_datos.
    ASSIGN lo_datos->* TO <lt_datos>.

* Se pasan los datos modificados a la tabla temporal para poderla pasar a las exit.
    DATA(lv_cond) = |{ zif_al30_data=>cs_control_fields_alv_data-updkz } IS NOT INITIAL|.
    LOOP AT ct_datos ASSIGNING FIELD-SYMBOL(<ls_datos>) WHERE (lv_cond).
      INSERT <ls_datos> INTO TABLE <lt_datos>.
    ENDLOOP.

    " Exit antes de grabar los datos
    exit_before_save_data(
      IMPORTING
        ev_abort_save = DATA(lv_abort_save)
        et_return     = DATA(lt_return)
      CHANGING
        ct_data = <lt_datos>
        ct_data_del = ct_datos_del ).

    " Cualquier error hará que no se graben los datos
    IF ( lv_abort_save EQ abap_true OR
         ( line_exists( lt_return[ type = zif_al30_data=>cs_msg_type-error ] ) OR
           line_exists( lt_return[ type = zif_al30_data=>cs_msg_type-dump ] ) ) ).

      IF lt_return IS INITIAL. " Si no hay mensaje de error se añade uno por defecto
        " Save aborted!!
        INSERT zcl_al30_util=>fill_return( iv_type = zif_al30_data=>cs_msg_type-success iv_number = '045' ) INTO TABLE et_return.
      ELSE.
        INSERT LINES OF lt_return INTO TABLE et_return.
      ENDIF.

    ELSE.

      " Si hay mensajes en la exit se añaden para que se vea todo lo generado
      INSERT LINES OF lt_return INTO TABLE et_return.

      " El proceso de grabación solo se realiza cuando hay datos para grabar.
      IF <lt_datos> IS NOT INITIAL OR ct_datos_del IS NOT INITIAL.

        " Si hay datos para borrar se llama al método encarlo de hacerlo
        IF ct_datos_del IS NOT INITIAL.
          CALL METHOD save_data_erased
            EXPORTING
              iv_allow_request = iv_allow_request
            IMPORTING
              es_return        = DATA(ls_return)
            CHANGING
              ct_datos_del     = ct_datos_del
              cv_order         = cv_order.
        ENDIF.

        " Si no hay errores, por el borrado, se continua el proceso.
        IF ls_return-type NE zif_al30_data=>cs_msg_type-error.
          " Si el retorno esta informado se añade al mensaje de retorno
          IF ls_return IS NOT INITIAL.
            INSERT ls_return INTO TABLE et_return.
          ENDIF.
          CLEAR ls_return.

          DATA(lv_save_error) = abap_false.

          " Si hay datos modificados se procede a realizar los INSert o UPDate.
          IF <lt_datos> IS NOT INITIAL.

            save_data_inup( EXPORTING it_data = <lt_datos>
                                      iv_save_transport = iv_allow_request
                            IMPORTING es_return = ls_return

                            CHANGING cv_order = cv_order
                                     cv_save_error = lv_save_error ).

          ENDIF.

          IF lv_save_error = abap_false. " Si no hay errores en la grabación

            " Se hace el commit
            COMMIT WORK AND WAIT.

            " Se informa que se han grabado los datos
            INSERT zcl_al30_util=>fill_return( iv_type = zif_al30_data=>cs_msg_type-success iv_number = '023' ) INTO TABLE et_return.

            " Si la tabla tiene marcada la opción de log de modificación se ejecuta el proceso donde se informará de los cambios
            IF ms_view-change_log = abap_true.
              save_change_log( EXPORTING it_datos = <lt_datos>
                                         it_datos_del = ct_datos_del ).
            ENDIF.



          ELSE.
            " Si hay errores pero el es_return esta en blanco le paso un mensaje genérico
            IF ls_return IS INITIAL.
              INSERT zcl_al30_util=>fill_return( iv_type = zif_al30_data=>cs_msg_type-error iv_number = '022' ) INTO TABLE et_return.
            ELSE.
              INSERT ls_return INTO TABLE et_return.
            ENDIF.

            ROLLBACK WORK. " Se deshace todos los cambios, incluidos lo del borrado
          ENDIF.


          " Exit de después de grabar tanto si ha ido bien como si no.
          exit_after_save_data(
              EXPORTING
                it_data      = <lt_datos>
                it_data_del = ct_datos_del
                iv_error_save = lv_save_error ).


          " Si no hay errores se resetea los datos para utilizalos como si viniese del diccionario y se actualiza los datos originales
          IF lv_save_error = abap_false.
            reset_data( CHANGING ct_datos = ct_datos
                                 ct_datos_del = ct_datos_del ).

            " Los datos ya grabados se vuelcan a la tabla de datos originales. Se vuelca la tabla que se pasa por parámetro que es la que contiene todos los datos léidos.
            ASSIGN mo_original_data->* TO <lt_original_data>.
            CLEAR <lt_original_data>.
            INSERT LINES OF ct_datos INTO TABLE <lt_original_data>.

          ENDIF.

        ELSE.
          INSERT ls_return INTO TABLE et_return.
        ENDIF.

      ELSE. " Si no hay datos y no tampoco borrado se informa un mensaje indicandolo
        INSERT zcl_al30_util=>fill_return( iv_type = zif_al30_data=>cs_msg_type-success iv_number = '039' ) INTO TABLE et_return.
      ENDIF.
    ENDIF.


  ENDMETHOD.


  METHOD save_data_erased.
    FIELD-SYMBOLS <lt_view> TYPE STANDARD TABLE.
    FIELD-SYMBOLS <lt_view_texttable> TYPE STANDARD TABLE.
    DATA lo_view TYPE REF TO data.
    DATA lo_view_texttable TYPE REF TO data.


    IF ct_datos_del IS INITIAL. EXIT. ENDIF.

    CLEAR es_return.

    " Se crea una tabla exactamente igual que las del diccionario para mover los datos a borrar.
    CREATE DATA lo_view TYPE STANDARD TABLE OF  (ms_view-tabname).
    ASSIGN lo_view->* TO <lt_view>.
    IF ms_view-texttable IS NOT INITIAL.
      CREATE DATA lo_view_texttable TYPE STANDARD TABLE OF  (ms_view-texttable).
      ASSIGN lo_view_texttable->* TO <lt_view_texttable>.
    ENDIF.

    " Se recorren los datos a borrar y se van añadiendo a las tablas temporales
    LOOP AT ct_datos_del ASSIGNING FIELD-SYMBOL(<ls_datos_del>).
      APPEND INITIAL LINE TO <lt_view> ASSIGNING FIELD-SYMBOL(<ls_view>).
      MOVE-CORRESPONDING <ls_datos_del> TO <ls_view>.
*      <ls_view> = CORRESPONDING #( <ls_datos_del> ).

      IF ms_view-texttable IS NOT INITIAL.
        APPEND INITIAL LINE TO <lt_view_texttable> ASSIGNING FIELD-SYMBOL(<ls_view_texttable>).
*        <ls_view_texttable> = CORRESPONDING #( <ls_datos_del> ).
        MOVE-CORRESPONDING <ls_datos_del> TO <ls_view_texttable>.
      ENDIF.

    ENDLOOP.

    " Se lanza el proceso de borrado
    DATA(lv_borrado_ok) = abap_true. " Por defecto no hay errores
    DELETE (ms_view-tabname) FROM TABLE <lt_view>.
    IF sy-subrc = 0.
      IF ms_view-texttable IS NOT INITIAL.
        DELETE (ms_view-texttable) FROM TABLE <lt_view_texttable>.
        IF sy-subrc NE 0.
          lv_borrado_ok = abap_false.
        ENDIF.
      ENDIF.
    ELSE.
      lv_borrado_ok = abap_false.
    ENDIF.

    IF lv_borrado_ok = abap_false. " Si hay errores en el borrado se indica el error
      es_return = zcl_al30_util=>fill_return( iv_type = zif_al30_data=>cs_msg_type-error iv_number = '038' ).
    ELSE.

      " Si va todo bien y esta permitido guardar en orden de transporte, se inicia el proceso para añadir los registros en la orden. Al liberar
      " la tarea/orden sap detecta que no existe el contenido en la tabla lo que hará será borrarla en el sistema de destino
      IF iv_allow_request = abap_true.
        zcl_al30_util=>values_itab_2_transport_order( EXPORTING it_values  = <lt_view>
                                                                iv_tabname = ms_view-tabname
                                                      IMPORTING es_return  = es_return
                                                      CHANGING cv_order = cv_order ).

        IF es_return-type NE zif_al30_data=>cs_msg_type-error. " Si no hay error se continua el proceso

          " Si hay tabla de textos se hace el mismo proceso
          IF ms_view-texttable IS NOT INITIAL.
            zcl_al30_util=>values_itab_2_transport_order( EXPORTING it_values  = <lt_view_texttable>
                                                            iv_tabname = ms_view-texttable
                                                  IMPORTING es_return  = es_return
                                                  CHANGING cv_order = cv_order ).
          ENDIF.

        ENDIF.

      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD save_data_inup.

    FIELD-SYMBOLS <lt_view> TYPE STANDARD TABLE.
    FIELD-SYMBOLS <lt_view_texttable> TYPE STANDARD TABLE.
    DATA lo_view TYPE REF TO data.
    DATA lo_view_texttable TYPE REF TO data.

    " Se crea una tabla exactamente igual que las del diccionario para mover los datos a borrar.
    CREATE DATA lo_view TYPE STANDARD TABLE OF (ms_view-tabname).
    ASSIGN lo_view->* TO <lt_view>.
    IF ms_view-texttable IS NOT INITIAL.
      CREATE DATA lo_view_texttable TYPE STANDARD TABLE OF (ms_view-texttable).
      ASSIGN lo_view_texttable->* TO <lt_view_texttable>.
    ENDIF.

    " Ahora se pasan los datos a las tablas definitivas
    LOOP AT it_data ASSIGNING FIELD-SYMBOL(<ls_datos>).
      APPEND INITIAL LINE TO <lt_view> ASSIGNING FIELD-SYMBOL(<ls_view>).
      MOVE-CORRESPONDING <ls_datos> TO <ls_view>.

      IF ms_view-texttable IS NOT INITIAL. " Lo mismo para la tabla de textos
        APPEND INITIAL LINE TO <lt_view_texttable> ASSIGNING FIELD-SYMBOL(<ls_view_texttable>).
        MOVE-CORRESPONDING <ls_datos> TO <ls_view_texttable>.
      ENDIF.

    ENDLOOP.

    " Se actualizan los datos
    cv_save_error = abap_false.
    MODIFY (ms_view-tabname) FROM TABLE <lt_view>.
    IF sy-subrc = 0.
      IF ms_view-texttable IS NOT INITIAL.
        MODIFY (ms_view-texttable) FROM TABLE <lt_view_texttable>.
        IF sy-subrc NE 0.
          cv_save_error = abap_true.
        ENDIF.
      ENDIF.
    ELSE.
      cv_save_error = abap_true.
    ENDIF.

    " Si no hay errores y se tiene que transportar
    IF cv_save_error = abap_false AND iv_save_transport = abap_true.

      zcl_al30_util=>values_itab_2_transport_order( EXPORTING it_values  = <lt_view>
                                                iv_tabname = ms_view-tabname
                                      IMPORTING es_return  = es_return
                                      CHANGING cv_order = cv_order ).

      IF es_return-type NE zif_al30_data=>cs_msg_type-error
         AND ms_view-texttable IS NOT INITIAL. " Si no hay error se añade la tabla de textos, si la hubiese
        zcl_al30_util=>values_itab_2_transport_order( EXPORTING it_values  = <lt_view_texttable>
                                                iv_tabname = ms_view-texttable
                                      IMPORTING es_return  = es_return
                                      CHANGING cv_order = cv_order ).
      ENDIF.

      " Si hay errores en la generación de la orden lo que se hace es que se marca el proceso de grabación como erróneo
      cv_save_error = COND #( WHEN es_return-type = zif_al30_data=>cs_msg_type-error THEN abap_true  ).

      " Si no hay errores limpio los mensajes que devuelve el método de ordenes de transporte. Ya que se pondrá el mensaje generico de grabación
      IF cv_save_error = abap_false.
        CLEAR es_return.
      ENDIF.
    ENDIF.




  ENDMETHOD.


  METHOD search_row_original_data.
    FIELD-SYMBOLS <lt_original_data> TYPE STANDARD TABLE.

    CLEAR es_data.

    DATA(lv_cond) = VALUE string( ).
    LOOP AT mt_fields_ddic ASSIGNING FIELD-SYMBOL(<ls_fields>)
                           WHERE tabname = iv_tabname
                                 AND keyflag = abap_true
                                 AND datatype NE zif_al30_data=>cs_datatype-mandt. " El mandate se excluye como calve

      ASSIGN COMPONENT <ls_fields>-fieldname OF STRUCTURE is_data TO FIELD-SYMBOL(<field>).
      IF sy-subrc = 0.
        lv_cond = COND string( LET sep = 'AND' cond = |{ <ls_fields>-fieldname } = '{ <field> }'| IN WHEN lv_cond IS INITIAL THEN cond ELSE |{ lv_cond } { sep } { cond }| ).
      ENDIF.

    ENDLOOP.
    IF sy-subrc = 0. " Si hay campos clave

      " Se pasa a un fields-symbols los datos originales
      ASSIGN mo_original_data->* TO <lt_original_data>.

      " Se busca el registro y se rellena el parámetro de salida y se sale del proceso
      LOOP AT <lt_original_data> INTO es_data WHERE (lv_cond).
        EXIT.
      ENDLOOP.

    ENDIF.
  ENDMETHOD.


  METHOD set_data_conf_view.
    ms_view = is_view.
    mt_fields = it_fields_view.
    mt_fields_text = it_fields_text_view.
    mt_fields_ddic = it_fields_ddic.
  ENDMETHOD.


  METHOD transport_entries.
    FIELD-SYMBOLS <lt_view> TYPE STANDARD TABLE.
    FIELD-SYMBOLS <lt_view_texttable> TYPE STANDARD TABLE.
    DATA lo_view TYPE REF TO data.

    DATA lo_view_texttable TYPE REF TO data.

    CLEAR: rs_return.


    " Se crea una tabla exactamente igual que las del diccionario para mover los datos a borrar.
    CREATE DATA lo_view TYPE STANDARD TABLE OF (ms_view-tabname).
    ASSIGN lo_view->* TO <lt_view>.
    IF ms_view-texttable IS NOT INITIAL.
      CREATE DATA lo_view_texttable TYPE STANDARD TABLE OF (ms_view-texttable).
      ASSIGN lo_view_texttable->* TO <lt_view_texttable>.
    ENDIF.



* Se pasan los datos al equivalente de las tablas de diccionario
    LOOP AT it_data ASSIGNING FIELD-SYMBOL(<ls_datos>).
      APPEND INITIAL LINE TO <lt_view> ASSIGNING FIELD-SYMBOL(<ls_view>).
      MOVE-CORRESPONDING <ls_datos> TO <ls_view>.

      IF ms_view-texttable IS NOT INITIAL. " Lo mismo para la tabla de textos
        APPEND INITIAL LINE TO <lt_view_texttable> ASSIGNING FIELD-SYMBOL(<ls_view_texttable>).
        MOVE-CORRESPONDING <ls_datos> TO <ls_view_texttable>.
      ENDIF.
    ENDLOOP.

    zcl_al30_util=>values_itab_2_transport_order( EXPORTING it_values  = <lt_view>
                                            iv_tabname = ms_view-tabname
                                  IMPORTING es_return  = rs_return
                                  CHANGING cv_order = cv_order ).

    IF rs_return-type NE zif_al30_data=>cs_msg_type-error
       AND ms_view-texttable IS NOT INITIAL. " Si no hay error se añade la tabla de textos, si la hubiese
      zcl_al30_util=>values_itab_2_transport_order( EXPORTING it_values  = <lt_view_texttable>
                                              iv_tabname = ms_view-texttable
                                    IMPORTING es_return  = rs_return
                                    CHANGING cv_order = cv_order ).
    ENDIF.

  ENDMETHOD.


  METHOD unlock_table.

    " Aquí no se gestiona las excepciones como en el bloqueo
    CALL FUNCTION 'VIEW_ENQUEUE'
      EXPORTING
        action               = 'D'
        enqueue_mode         = 'E'
        view_name            = iv_table
      EXCEPTIONS
        client_reference     = 1
        foreign_lock         = 2
        invalid_action       = 3
        invalid_enqueue_mode = 4
        system_failure       = 5
        table_not_found      = 6
        OTHERS               = 7.
  ENDMETHOD.


  METHOD verify_change_row_data.
    CLEAR et_return.

    " Verificación de datos
    verify_row_data(
      EXPORTING
        iv_row      = iv_row
        is_row_data =  cs_row_data
      IMPORTING
        et_return   = et_return ).

* Se lanza la exit de verificación y completado de datos
    CALL METHOD exit_verify_change_row_data
      EXPORTING
        iv_row      = iv_row
      IMPORTING
        et_return   = DATA(lt_return)
      CHANGING
        cs_row_data = cs_row_data.

    " Se añaden los mensajes obtenidos a los existentes
    INSERT LINES OF lt_return INTO TABLE et_return.
  ENDMETHOD.


  METHOD verify_field_data.


    CLEAR es_return.

* Me posiciono en la configuración del campo pasado. Si no existe, en principio no deberia pasar, configuracion no se hace nada
    READ TABLE mt_fields ASSIGNING FIELD-SYMBOL(<ls_fields>) WITH KEY fieldname = iv_fieldname.
    IF sy-subrc = 0.

* Si no hay errores entra la verificacion de cliente
      IF es_return IS INITIAL.
        exit_verify_field_data(  EXPORTING iv_fieldname = iv_fieldname
                                           iv_value = iv_value
                                 IMPORTING es_return = es_return ).
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD view_have_auto_adjust.
    rv_have = abap_false.

    SELECT SINGLE auto_adjust INTO rv_have FROM zal30_t_view WHERE tabname = iv_view.

  ENDMETHOD.


  METHOD view_have_sap_auth.
    rv_have = abap_false.

    SELECT SINGLE check_auth_sap INTO rv_have FROM zal30_t_view WHERE tabname = iv_view.
  ENDMETHOD.


  METHOD view_have_user_auth.
    rv_have = abap_false.

    SELECT SINGLE auth_user INTO rv_have FROM zal30_t_view WHERE tabname = iv_view.

  ENDMETHOD.


  METHOD view_list.

    CLEAR: et_view_list.


    SELECT a~tabname AS view_name b~ddtext AS view_desc INTO TABLE et_view_list
           FROM zal30_t_view AS a LEFT OUTER JOIN dd02t AS b ON
                b~tabname = a~tabname
                AND b~ddlanguage = iv_langu
                WHERE a~tabname IN it_r_views.

  ENDMETHOD.

  METHOD field_value_adjust_sql.
    FIELD-SYMBOLS: <field> TYPE any.
    DATA lo_field TYPE REF TO data.

    " Se recupera la definición del campo
    DATA(ls_field_ddic) = CAST cl_abap_elemdescr( cl_abap_typedescr=>describe_by_name( |{ iv_tabname }-{ iv_fieldname }| ) )->get_ddic_field( ).

    " Si no hay rutina de conversión el valor no cambiará.
    IF ls_field_ddic-convexit IS NOT INITIAL.

      " Se monta la concatenación de la tabla y el campo
      DATA(lv_field) = |{ iv_tabname }-{ iv_fieldname }|.

      " Se crea un tipo igual al del campo informado
      CREATE DATA lo_field TYPE (lv_field).
      ASSIGN lo_field->* TO <field>.

      " Se monta la función de la rutina de conversión y se aplica el valor
      DATA(lv_function) = |CONVERSION_EXIT_{ ls_field_ddic-convexit }_INPUT|.
      CALL FUNCTION lv_function
        EXPORTING
          input  = cv_value
        IMPORTING
          output = <field>
        EXCEPTIONS
          OTHERS = 99.
      IF sy-subrc = 0.
        cv_value = <field>.
      ENDIF.

    ENDIF.

  ENDMETHOD.

  METHOD verify_row_data.
    CLEAR et_return.

    " Verificación interna segun configuración
    internal_verify_row_data( EXPORTING iv_row = iv_row
                                         is_row_data = is_row_data
                              IMPORTING et_return = et_return ) .



    " Exit para la verificación de la línea
    exit_verify_row_data( EXPORTING iv_row      = iv_row
                                    is_row_data = is_row_data
                                    iv_save_process = iv_save_process
                          IMPORTING et_return   = DATA(lt_return) ).

    " Todos los mensajes de la exit se obtiene el texto porque según se llame dicho método es encesario.
    " Ejemplo en el proceso de grabación
    LOOP AT lt_return ASSIGNING FIELD-SYMBOL(<ls_return>).
      <ls_return>-message = zcl_al30_util=>fill_return( iv_type = zif_al30_data=>cs_msg_type-error
                                                                     iv_id = zif_al30_data=>cv_msg_id
                                                                     iv_number = '034'
                                                                     iv_field = <ls_return>-field )-message.

      INSERT <ls_return> INTO TABLE et_return. " Se añaden los mensajes de la exit a los generales
    ENDLOOP.
  ENDMETHOD.

  METHOD verify_save_data.
    FIELD-SYMBOLS: <lt_datos> TYPE STANDARD TABLE.
    DATA lo_datos TYPE REF TO data.
    CLEAR et_return.

    " Se crea una tabla como la de entrada para pasarla con los datos modificados a las exit
    CREATE DATA lo_datos LIKE it_data.
    ASSIGN lo_datos->* TO <lt_datos>.

    " Se hacen las verificacion internas
    DATA(lv_cond) = |{ zif_al30_data=>cs_control_fields_alv_data-updkz } IS NOT INITIAL|.

    LOOP AT it_data ASSIGNING FIELD-SYMBOL(<ls_data>) WHERE (lv_cond).
      DATA(lv_tabix) = sy-tabix.

      INSERT <ls_data> INTO TABLE <lt_datos>.

      internal_verify_row_data( EXPORTING iv_row = lv_tabix
                                           is_row_data = <ls_data>
                                IMPORTING et_return = DATA(lt_return) ) .

      INSERT LINES OF lt_return INTO TABLE et_return.
      CLEAR lt_return.
    ENDLOOP.

    " Exit para la verificación de los datos a grabar
    exit_verify_save_data( EXPORTING it_data = <lt_datos>
                                      it_data_del = it_data_del
                           IMPORTING et_return = lt_return ).
    INSERT LINES OF lt_return INTO TABLE et_return.

  ENDMETHOD.

  METHOD exit_verify_row_data.
    DATA ld_metodo TYPE seocpdname.

    CLEAR: et_return.

    IF mo_exit_class IS BOUND.

* Monto el método al cual se llamará de la clase de exit.
      CONCATENATE zif_al30_data=>cv_intf_exit '~EXIT_VERIFY_ROW_DATA' INTO ld_metodo.

      TRY.
          CALL METHOD mo_exit_class->(ld_metodo)
            EXPORTING
              iv_row          = iv_row
              is_row_data     = is_row_data
              iv_save_process = iv_save_process
            IMPORTING
              et_return       = et_return.

        CATCH cx_root.
      ENDTRY.

    ENDIF.
  ENDMETHOD.


  METHOD internal_verify_row_data.
    LOOP AT mt_fields REFERENCE INTO DATA(lo_fields).

      ASSIGN COMPONENT lo_fields->fieldname OF STRUCTURE is_row_data TO FIELD-SYMBOL(<field>).
      IF sy-subrc = 0.
        " Validación: Campo obligatorio
        IF lo_fields->mandatory = abap_true AND <field> IS INITIAL.
          INSERT zcl_al30_util=>fill_return( iv_type = zif_al30_data=>cs_msg_type-error
                                                               iv_id = zif_al30_data=>cv_msg_id
                                                               iv_number = '034'
                                                               iv_field = lo_fields->fieldname ) INTO TABLE et_return.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD exit_verify_save_data.
    DATA ld_metodo TYPE seocpdname.

    IF mo_exit_class IS BOUND.

* Monto el método al cual se llamará de la clase de exit.
      CONCATENATE zif_al30_data=>cv_intf_exit '~EXIT_VERIFY_SAVE_DATA' INTO ld_metodo.

      TRY.
          CALL METHOD mo_exit_class->(ld_metodo)
            EXPORTING
              it_data     = it_data
              it_data_del = it_data_del
            IMPORTING
              et_return   = et_return.

        CATCH cx_root.
      ENDTRY.

    ENDIF.
  ENDMETHOD.

  METHOD set_edit_mode_alv.

    " Se llama a la exit que pueda cambiar el modo de edición
    exit_set_edit_mode_alv( EXPORTING it_data = it_data
                            IMPORTING ev_edit_mode = DATA(lv_edit_mode) ).

    " Si se ha determinado un modo nuevo tiene prevalencia sobre el determinado
    ev_edit_mode = COND #( WHEN lv_edit_mode IS NOT INITIAL THEN lv_edit_mode ELSE ev_edit_mode ).
  ENDMETHOD.

  METHOD exit_set_edit_mode_alv.
    DATA ld_metodo TYPE seocpdname.

    IF mo_exit_class IS BOUND.

* Monto el método al cual se llamará de la clase de exit.
      CONCATENATE zif_al30_data=>cv_intf_exit '~EXIT_SET_EDIT_MODE_ALV' INTO ld_metodo.

      TRY.
          CALL METHOD mo_exit_class->(ld_metodo)
            EXPORTING
              it_data      = it_data
            IMPORTING
              ev_edit_mode = ev_edit_mode.

        CATCH cx_root.
      ENDTRY.

    ENDIF.
  ENDMETHOD.

ENDCLASS.
