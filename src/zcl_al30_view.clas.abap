CLASS zcl_al30_view DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
*"* public components of class ZCL_AL30_VIEW
*"* do not include other source files here!!!
    TYPE-POOLS adbc .

    METHODS read_data
      IMPORTING
        is_filters TYPE zif_al30_data=>ts_filter_read_data
      EXPORTING
        !es_return TYPE bapiret2
      CHANGING
        !co_data   TYPE REF TO data .
    METHODS get_fieldcat_view
      IMPORTING
        !iv_mode     TYPE char1
      EXPORTING
        !es_return   TYPE bapiret2
        !et_fieldcat TYPE lvc_t_fcat .
    METHODS create_it_data_view
      IMPORTING
        !iv_mode   TYPE char1
      EXPORTING
        !et_data   TYPE REF TO data
        !es_return TYPE bapiret2 .
    METHODS save_data
      IMPORTING
        !iv_allow_request TYPE sap_bool DEFAULT abap_false
      EXPORTING
        !es_return        TYPE bapiret2
      CHANGING
        !ct_datos_del     TYPE STANDARD TABLE
        !ct_datos         TYPE STANDARD TABLE
        !cv_order         TYPE e070-trkorr .
    METHODS verify_field_data
      IMPORTING
        !iv_fieldname TYPE any
        !iv_value     TYPE any
      EXPORTING
        es_return     TYPE bapiret2 .
    METHODS verify_change_row_data
      EXPORTING
        VALUE(es_return) TYPE bapiret2
      CHANGING
        !cs_row_data     TYPE any .
    METHODS check_authorization
      IMPORTING
        !iv_view_name   TYPE tabname
        !iv_view_action TYPE any DEFAULT 'U'
      RAISING
        zcx_al30 .
    METHODS transport_entries
      IMPORTING
        !it_data         TYPE STANDARD TABLE
      CHANGING
        !cv_order        TYPE e070-trkorr
      RETURNING
        VALUE(rs_return) TYPE bapiret2 .
    METHODS view_have_user_auth
      IMPORTING
        !iv_view       TYPE tabname
      RETURNING
        VALUE(rv_have) TYPE sap_bool .
    METHODS view_have_sap_auth
      IMPORTING
        !iv_view       TYPE tabname
      RETURNING
        VALUE(rv_have) TYPE sap_bool .
    METHODS get_level_auth_view
      IMPORTING
        !iv_view             TYPE tabname
        !iv_user             TYPE syuname DEFAULT sy-uname
      RETURNING
        VALUE(rv_level_auth) TYPE zal30_e_level_auth .
    METHODS view_have_auto_adjust
      IMPORTING
        !iv_view       TYPE tabname
      RETURNING
        VALUE(rv_have) TYPE sap_bool .
    METHODS lock_view
      RAISING
        zcx_al30 .
    METHODS instance_exit_class
      IMPORTING
        !iv_exit_class   TYPE zal30_e_exit_class
      RETURNING
        VALUE(rs_return) TYPE bapiret2 .
    METHODS set_data_conf_view
      IMPORTING
        !it_fields_view      TYPE zif_al30_data=>tt_fields_view
        !it_fields_text_view TYPE zif_al30_data=>tt_fields_text_view
        !is_view             TYPE zal30_t_view
        !it_fields_ddic      TYPE dd03ptab .

protected section.

  types:
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
  types:
    tt_main_change_log TYPE STANDARD TABLE OF ts_main_change_log .

  data MO_EXIT_CLASS type ref to OBJECT .
  data MT_FIELDS type ZIF_AL30_DATA=>TT_FIELDS_VIEW .
  data MT_FIELDS_TEXT type ZIF_AL30_DATA=>TT_FIELDS_TEXT_VIEW .
  data MT_FIELDS_DDIC type DD03PTAB .
  data MS_VIEW type ZAL30_T_VIEW .
  data MO_ORIGINAL_DATA type ref to DATA .
  data MV_ABORT_SAVE type SAP_BOOL .

  methods EXIT_AFTER_SAVE_DATA
    importing
      !IT_DATA type STANDARD TABLE
      !IT_DATA_DEL type STANDARD TABLE
      !IV_ERROR_SAVE type SAP_BOOL .
  methods EXIT_BEFORE_SAVE_DATA
    exporting
      !EV_ABORT_SAVE type SAP_BOOL
      !ET_RETURN type BAPIRET2_T
    changing
      !CT_DATA type STANDARD TABLE
      !CT_DATA_DEL type STANDARD TABLE .
  methods RESET_DATA
    changing
      !CT_DATOS type STANDARD TABLE
      !CT_DATOS_DEL type STANDARD TABLE .
  methods EXIT_CHECK_AUTH_DATA_READ
    importing
      !IS_ROW_DATA type ANY
    returning
      value(RV_AUTH) type SAP_BOOL .
  methods EXIT_IN_PROCESS_DATA_READ
    changing
      !CS_ROW_DATA type ANY .
  methods EXIT_BEFORE_READ_DATA
    changing
      !CT_DATA type STANDARD TABLE .
  methods EXIT_VERIFY_FIELD_DATA
    importing
      !IV_FIELDNAME type ANY
      !IV_VALUE type ANY
    exporting
      !ES_RETURN type BAPIRET2 .
  methods EXIT_VERIFY_CHANGE_ROW_DATA
    exporting
      value(ES_RETURN) type BAPIRET2
    changing
      !CS_ROW_DATA type ANY .
  methods ADD_FIELDS_TEXTTABLE
    importing
      !IV_VIEW type TABNAME
      !IT_FIELDS type ZIF_AL30_DATA=>TT_FIELDS_VIEW
    changing
      !CT_FCAT type LVC_T_FCAT .
  methods ADD_EDIT_FIELDS
    changing
      !CT_FCAT type LVC_T_FCAT .
  methods GET_LVC_FIELDCAT
    importing
      value(IS_VIEW) type ZAL30_T_VIEW
    returning
      value(RT_FCAT) type LVC_T_FCAT .
  methods EXIT_PROCESS_CATALOG_OF_FIELD
    changing
      value(CS_FIELDCAT) type LVC_S_FCAT .
  methods CREATE_SQL_READ
    importing
      !IS_FILTERS type ZIF_AL30_DATA=>TS_FILTER_READ_DATA
    exporting
      !ET_COLS type ADBC_COLUMN_TAB
      !EV_SQL type STRING .
  methods CREATE_JOIN_SQL_TEXTTABLE
    returning
      value(RV_RESULT) type STRING .
  methods LOCK_TABLE
    importing
      !IV_TABLE type OCUS-TABLE
    raising
      ZCX_AL30 .
  methods UNLOCK_TABLE
    importing
      !IV_TABLE type TABNAME .
  methods SAVE_DATA_ERASED
    importing
      !IV_ALLOW_REQUEST type SAP_BOOL
    exporting
      !ES_RETURN type BAPIRET2
    changing
      !CT_DATOS_DEL type STANDARD TABLE
      !CV_ORDER type E070-TRKORR .
  methods SAVE_CHANGE_LOG
    importing
      !IT_DATOS type STANDARD TABLE
      !IT_DATOS_DEL type STANDARD TABLE .
  methods FILL_CHANGE_LOG
    importing
      !IT_DATA type STANDARD TABLE
    changing
      !CT_CHANGE_LOG type TT_MAIN_CHANGE_LOG .
  methods CONV_DATA_2_CHANGELOG_KEY
    importing
      !IS_DATA type ANY
      !IT_FIELDS_DDIC type DD03PTAB
    returning
      value(RV_KEY) type CDTABKEY .
  methods SAVE_DATA_INUP
    importing
      !IT_DATA type STANDARD TABLE
      !IV_SAVE_TRANSPORT type SAP_BOOL
    exporting
      !ES_RETURN type BAPIRET2
    changing
      !CV_ORDER type E070-TRKORR
      !CV_SAVE_ERROR type SAP_BOOL .
  methods FILL_CHANGELOG_VALUES
    importing
      !IV_TABNAME type ZAL30_T_VIEW-TABNAME
      !IV_CHNGIND type CDCHNGIND
      !IT_FIELDS type DD03PTAB
      !IS_DATA type ANY
    changing
      !CT_CHANGE_LOG type ZCL_AL30_VIEW=>TT_MAIN_CHANGE_LOG .
  methods SEARCH_ROW_ORIGINAL_DATA
    importing
      !IS_DATA type ANY
      !IV_TABNAME type ZAL30_T_VIEW-TABNAME
    exporting
      value(ES_DATA) type ANY .
  methods CREATE_WHERE_SQL
    importing
      !IS_FILTERS type ZIF_AL30_DATA=>TS_FILTER_READ_DATA
    returning
      value(RV_WHERE) type STRING .
  methods SHOW_POPUP_ERROR_MESSAGES
    changing
      !CT_RETURN type BAPIRET2_T .
  PRIVATE SECTION.

*"* private components of class ZCL_AL30_VIEW
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_AL30_VIEW IMPLEMENTATION.


  METHOD add_edit_fields.

* Se añaden los campos de control
    INSERT LINES OF zcl_al30_util=>get_fcat_control_edit_view( ) INTO TABLE ct_fcat.


* Se añade el campo de estilo
    INSERT VALUE #( fieldname = zif_al30_data=>cv_field_style rollname = 'LVC_T_STYL' ) INTO TABLE ct_fcat.

  ENDMETHOD.


  METHOD add_fields_texttable.

    IF iv_view IS NOT INITIAL.

*      TRY.
*          NEW zcl_al30_conf( )->read_single_view_ddic(
*            EXPORTING
*              iv_name_view = iv_view
*            IMPORTING
*              et_dd03p     = DATA(lt_dd03p) ).
*
*          IF lt_dd03p IS NOT INITIAL.
      " De los campos de la tabla de texto obtenemos sus campos para añadirlos al catalogo de campos
      LOOP AT it_fields ASSIGNING FIELD-SYMBOL(<ls_fields>) WHERE field_texttable = abap_true.
        READ TABLE mt_fields_ddic ASSIGNING FIELD-SYMBOL(<ls_dd03p>)
                                   WITH KEY tabname = iv_view
                                            fieldname = <ls_fields>-fieldname.

        IF sy-subrc = 0.
          INSERT VALUE #( fieldname = <ls_fields>-fieldname rollname = <ls_dd03p>-rollname ) INTO TABLE ct_fcat.
        ENDIF.

      ENDLOOP.
*    ENDIF.

*        CATCH zcx_al30.
*      ENDTRY.
    ENDIF.

  ENDMETHOD.


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

* Creo la estructura en base a la vista/tabla indicada
    CALL METHOD zcl_ca_dynamic_tables=>create_wa_from_struc
      EXPORTING
        i_struc    = ms_view-tabname
      IMPORTING
        e_workarea = lo_wa_view.

    IF lo_wa_view IS BOUND.

* Se añade los campos de la tabla de textos asociada a la vista.
      add_fields_texttable( EXPORTING iv_view = ms_view-texttable
                                      it_fields = mt_fields
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

    " Primero se rellena el nombre de las campos de la vist
    et_cols = VALUE #( FOR <ls_fields> IN mt_fields ( <ls_fields>-fieldname ) ).

    " Ahora se monta la SQL de búsqueda.
    ev_sql = 'SELECT'.

    " Se monta los campos de la búsqueda
    DATA(lv_sql_fields) = REDUCE string( INIT sql TYPE string FOR <ls_fields> IN mt_fields NEXT sql = sql &&
                          COND #( LET sep = ',' field = COND string( WHEN <ls_fields>-field_texttable = abap_false THEN |{ zif_al30_data=>cs_alias_sql-view }.{ <ls_fields>-fieldname }|
                          ELSE |{ zif_al30_data=>cs_alias_sql-texttable }.{ <ls_fields>-fieldname }| )
                          IN WHEN sql IS NOT INITIAL THEN |{ sep } { field }| ELSE |{ field }| ) ).

    " Se monta la ordenación, mismo proceso que para montar los campos pero solo queremos los campos claves
    DATA(lv_order_fields) = REDUCE string( INIT sql TYPE string FOR <ls_fields> IN mt_fields WHERE ( key_ddic = abap_true ) NEXT sql = sql &&
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

    " Primer nivel son las tablas
    LOOP AT is_filters-fields_ranges ASSIGNING FIELD-SYMBOL(<ls_tables>).
      " Según la tabla se monta el alias
      IF <ls_tables>-tablename = ms_view-tabname.
        DATA(lv_alias) = |{ zif_al30_data=>cs_alias_sql-view }.|.
      ELSE.
        lv_alias = |{ zif_al30_data=>cs_alias_sql-texttable }.|.
      ENDIF.

      " Segundo nivel los campos de la tabla
      LOOP AT <ls_tables>-frange_t ASSIGNING FIELD-SYMBOL(<ls_fields>).

        " Tercer nivel son los valores de los campos
        " Se mira el número de registros por si hay que añadir un OR
        DATA(lv_lines) = lines( <ls_fields>-selopt_t ).
        LOOP AT <ls_fields>-selopt_t ASSIGNING FIELD-SYMBOL(<ls_values>).

          " Primero se monta el campo
          DATA(lv_field) = |{ lv_alias }{ <ls_fields>-fieldname }|.

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
          ENDIF.
          " Se monta la condición
          DATA(lv_cond) = |{ lv_field } { lv_operator }|.

          " Y se pone en el where
          lv_where = COND #( WHEN lv_where IS INITIAL THEN |( { lv_cond }| ELSE |{ lv_where } { lv_cond }| ).

          " Si el numero del registro es inferior al numero de líneas hay que añadir un OR
          IF sy-tabix < lv_lines.
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

    CLEAR: es_return.

    IF mo_exit_class IS BOUND.

* Monto el método al cual se llamará de la clase de exit.
      CONCATENATE zif_al30_data=>cv_intf_exit '~EXIT_VERIFY_CHANGE_ROW_DATA' INTO ld_metodo.

      TRY.
          CALL METHOD mo_exit_class->(ld_metodo)
            IMPORTING
              es_return   = es_return
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

      ASSIGN COMPONENT zif_al30_data=>cv_field_updkz OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<updkz>). " Este campo debe existir, si no existe que pegue dump

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
          mv_message = lv_message.
    ENDIF.
  ENDMETHOD.


  METHOD lock_view.


    TRY.
        " Se bloquea la tabla principal
        lock_table( iv_table = ms_view-tabname ).

        " Si falla el bloqueo de la tabla de texto hay que desbloquear la principal para no dejarla bloqueada
        DATA(lv_main_lock) = abap_true.

        " Si hay tabla de textos se bloquea también
        IF ms_view-texttable IS NOT INITIAL.
          lock_table( iv_table = ms_view-texttable ).
        ENDIF.


      CATCH zcx_al30 INTO DATA(lx_excep).

        " Si se produce una excepción y la tabla principal se ha bloqueado habrá que desbloquearla
        IF lv_main_lock = abap_true.
          unlock_table( iv_table = ms_view-tabname ).
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
              ASSIGN COMPONENT zif_al30_data=>cv_field_tabix_ddic OF STRUCTURE <ls_datos> TO FIELD-SYMBOL(<field>).
              IF sy-subrc = 0.
                <field> = lv_tabix.
              ENDIF.

              " Exit mientras se procesan los registros.
              exit_in_process_data_read( CHANGING cs_row_data = <ls_datos> ).

            ELSE. " Si no tiene se borra el registro
              DELETE <lt_datos> INDEX lv_tabix.
            ENDIF.

          ENDLOOP.

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

* Campo de posicion en el dicionario
      ASSIGN COMPONENT zif_al30_data=>cv_field_tabix_ddic OF STRUCTURE <ls_wa> TO FIELD-SYMBOL(<field>).
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

    CLEAR: es_return.



    " Se crea una tabla como la de entrada para pasarla con los datos modificados a las exit
    CREATE DATA lo_datos LIKE ct_datos.
    ASSIGN lo_datos->* TO <lt_datos>.

* Se pasan los datos modificados a la tabla temporal para poderla pasar a las exit.
    DATA(lv_cond) = |{ zif_al30_data=>cv_field_updkz } IS NOT INITIAL|.
    LOOP AT ct_datos ASSIGNING FIELD-SYMBOL(<ls_datos>) WHERE (lv_cond).
      INSERT <ls_datos> INTO TABLE <lt_datos>.
    ENDLOOP.

    " Exit antes de grabar los datos
    exit_before_save_data(
      IMPORTING
        ev_abort_save = mv_abort_save
        et_return     = DATA(lt_return)
      CHANGING
        ct_data = <lt_datos>
        ct_data_del = ct_datos_del ).

    show_popup_error_messages( CHANGING ct_return = lt_return ).

    IF ( mv_abort_save EQ abap_true OR
         ( line_exists( lt_return[ type = 'A' ] ) OR
           line_exists( lt_return[ type = 'X' ] ) OR
           line_exists( lt_return[ type = 'E' ] ) ) ).

      IF lt_return IS INITIAL.

        "No data changed
        es_return = zcl_al30_util=>fill_return( iv_type = zif_al30_data=>cs_msg_type-success iv_number = '039' ).
      ENDIF.

    ELSE.

      " El proceso de grabación solo se realiza cuando hay datos para grabar.
      IF <lt_datos> IS NOT INITIAL OR ct_datos_del IS NOT INITIAL.

* Si hay datos para borrar se llama al método encarlo de hacerlo
        IF ct_datos_del IS NOT INITIAL.
          CALL METHOD save_data_erased
            EXPORTING
              iv_allow_request = iv_allow_request
            IMPORTING
              es_return        = es_return
            CHANGING
              ct_datos_del     = ct_datos_del
              cv_order         = cv_order.
        ENDIF.

        " Si no hay errores, por el borrado, se continua el proceso.
        IF es_return-type NE zif_al30_data=>cs_msg_type-error.
          CLEAR es_return. " Se limpia posibles mensajes succes del proceso de borrado

          DATA(lv_save_error) = abap_false.

          " Si hay datos modificados se procede a realizar los INSert o UPDate.
          IF <lt_datos> IS NOT INITIAL.

            save_data_inup( EXPORTING it_data = <lt_datos>
                                      iv_save_transport = iv_allow_request
                            IMPORTING es_return = es_return

                            CHANGING cv_order = cv_order
                                     cv_save_error = lv_save_error ).

          ENDIF.

          IF lv_save_error = abap_false. " Si no hay errores en la grabación

            " Se hace el commit
            COMMIT WORK AND WAIT.

            " Se informa que se han grabado los datos
            es_return = zcl_al30_util=>fill_return( iv_type = zif_al30_data=>cs_msg_type-success iv_number = '023' ).

            " Si la tabla tiene marcada la opción de log de modificación se ejecuta el proceso donde se informará de los cambios
            IF ms_view-change_log = abap_true.
              save_change_log( EXPORTING it_datos = <lt_datos>
                                         it_datos_del = ct_datos_del ).
            ENDIF.



          ELSE.
            " Si hay errores pero el es_return esta en blanco le paso un mensaje genérico
            IF es_return IS INITIAL.
              es_return = zcl_al30_util=>fill_return( iv_type = zif_al30_data=>cs_msg_type-error iv_number = '022' ).
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


        ENDIF.

      ELSE. " Si no hay datos y no tampoco borrado se informa un mensaje indicandolo
        es_return = zcl_al30_util=>fill_return( iv_type = zif_al30_data=>cs_msg_type-success iv_number = '039' ).
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


  METHOD show_popup_error_messages.

    IF ct_return IS INITIAL.
      RETURN.
    ENDIF.

    TRY.
        cl_salv_table=>factory( IMPORTING r_salv_table = DATA(lo_salv_table)
                                CHANGING  t_table      = ct_return ).

        IF lo_salv_table IS BOUND.

          lo_salv_table->get_columns( )->set_optimize( abap_true ).

          lo_salv_table->get_columns( )->get_column(: 'ID' )->set_visible( if_salv_c_bool_sap=>false ),
                                                      'TYPE' )->set_visible( if_salv_c_bool_sap=>false ),
                                                      'NUMBER' )->set_visible( if_salv_c_bool_sap=>false ),
                                                      'LOG_NO' )->set_visible( if_salv_c_bool_sap=>false ),
                                                      'LOG_MSG_NO' )->set_visible( if_salv_c_bool_sap=>false ),
                                                      'MESSAGE_V1' )->set_visible( if_salv_c_bool_sap=>false ),
                                                      'MESSAGE_V2' )->set_visible( if_salv_c_bool_sap=>false ),
                                                      'MESSAGE_V3' )->set_visible( if_salv_c_bool_sap=>false ),
                                                      'MESSAGE_V4' )->set_visible( if_salv_c_bool_sap=>false ),
                                                      'PARAMETER' )->set_visible( if_salv_c_bool_sap=>false ),
                                                      'FIELD' )->set_visible( if_salv_c_bool_sap=>false ),
                                                      'SYSTEM' )->set_visible( if_salv_c_bool_sap=>false ).

          lo_salv_table->get_columns( )->set_column_position( columnname = 'ROW'
                                                              position   = 1 ).
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
    CLEAR es_return.

* ----->
* Codigo mio
* <-----

* Si no hay error lanzo el cambio y verificacion de datos.
    IF es_return IS INITIAL.
      CALL METHOD exit_verify_change_row_data
        IMPORTING
          es_return   = es_return
        CHANGING
          cs_row_data = cs_row_data.
    ENDIF.
  ENDMETHOD.


  METHOD verify_field_data.


    CLEAR es_return.

* Me posiciono en la configuración del campo pasado. Si no existe, en principio no deberia pasar, configuracion no se hace nada
    READ TABLE mt_fields ASSIGNING FIELD-SYMBOL(<ls_fields>) WITH KEY fieldname = iv_fieldname.
    IF sy-subrc = 0.

* Validación: Campo obligatorio
      IF <ls_fields>-mandatory = abap_true AND iv_value IS INITIAL.
        es_return = zcl_al30_util=>fill_return( iv_type = zif_al30_data=>cs_msg_type-error iv_number = '034' ).
      ENDIF.

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
ENDCLASS.
