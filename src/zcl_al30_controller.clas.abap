CLASS zcl_al30_controller DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
*"* public components of class ZCL_AL30_CONTROLLER
*"* do not include other source files here!!!

    METHODS constructor .
    METHODS read_view
      IMPORTING
        !iv_name_view TYPE tabname
      EXPORTING
        !ev_text_view TYPE as4text
        !es_return    TYPE bapiret2
        !es_view      TYPE zal30_t_view
        !et_fields    TYPE zal30_i_fields_view .
    METHODS read_view_alv
      IMPORTING
        !iv_name_view TYPE tabname
      EXPORTING
        !ev_text_view TYPE as4text
        !es_return    TYPE bapiret2
        !es_view      TYPE zal30_t_view
        !et_fields    TYPE zal30_i_fields_alv .
    METHODS check_view
      IMPORTING
        !iv_name_view TYPE tabname
        !iv_operation TYPE char01
      EXPORTING
        !es_return    TYPE bapiret2
        !ev_text_view TYPE as4text .
    METHODS create_view
      IMPORTING
        !iv_name_view TYPE tabname
      EXPORTING
        !es_return    TYPE bapiret2
        !ev_text_view TYPE as4text .
    METHODS delete_view
      IMPORTING
        !iv_name_view    TYPE tabname
      RETURNING
        VALUE(rs_return) TYPE bapiret2 .
    METHODS save_view
      IMPORTING
        !is_view         TYPE zal30_t_view
        !it_fields       TYPE zal30_i_fields_view
      RETURNING
        VALUE(rs_return) TYPE bapiret2 .
    METHODS save_view_alv
      IMPORTING
        !is_view         TYPE zal30_t_view
        !it_fields       TYPE zal30_i_fields_alv
      RETURNING
        VALUE(rs_return) TYPE bapiret2 .
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
        !iv_view      TYPE zal30_t_view
      EXPORTING
        !es_return    TYPE bapiret2
      CHANGING
        !ct_datos_del TYPE STANDARD TABLE
        !ct_datos     TYPE STANDARD TABLE .
    METHODS verify_field_data
      IMPORTING
        !iv_fieldname    TYPE any
        !it_fields       TYPE zal30_i_fields_alv
        !iv_value        TYPE any
        !iv_exit_class   TYPE zal30_e_exit_class
      RETURNING
        VALUE(rs_return) TYPE bapiret2 .
    TYPE-POOLS abap .
    METHODS adjust_view_dictionary
      IMPORTING
        !iv_view         TYPE zal30_t_view
        !iv_keep_text    TYPE sap_bool DEFAULT abap_true
      EXPORTING
        VALUE(es_return) TYPE bapiret2
        !ev_text_view    TYPE as4text
      CHANGING
        !ct_fields       TYPE zal30_i_fields_alv .
    METHODS check_changes_dict_view
      IMPORTING
        !it_fields     TYPE zal30_i_fields_alv OPTIONAL
        !iv_view_name  TYPE tabname
      RETURNING
        VALUE(rv_diff) TYPE sap_bool .
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
    METHODS check_exit_class
      IMPORTING
        !iv_exit_class   TYPE zal30_e_exit_class
      RETURNING
        VALUE(rs_return) TYPE bapiret2 .
    METHODS verify_change_row_data
      IMPORTING
        !iv_exit_class TYPE zal30_e_exit_class
      EXPORTING
        !es_return     TYPE bapiret2
      CHANGING
        !cs_row_data   TYPE any .
  PROTECTED SECTION.
*"* private components of class ZCL_AL30_CONTROLLER
*"* do not include other source files here!!!

    METHODS conv_view_from_alv
      IMPORTING
         it_fields_alv        TYPE zal30_i_fields_alv
      RETURNING
        VALUE(rt_fields_view) TYPE zal30_i_fields_view .
    METHODS conv_view_to_alv
      IMPORTING
         it_fields_view      TYPE zal30_i_fields_view
      RETURNING
        value(rt_fields_alv) TYPE zal30_i_fields_alv .
*"* protected components of class ZCL_AL30_CONTROLLER
*"* do not include other source files here!!!
  PRIVATE SECTION.


ENDCLASS.



CLASS zcl_al30_controller IMPLEMENTATION.


  METHOD adjust_view_dictionary.
    DATA lt_fields_view TYPE zal30_i_fields_view.

    DATA lo_conf TYPE REF TO zcl_al30_conf.

    CLEAR es_return.

    CREATE OBJECT lo_conf.


* Convierto los datos del alv a un formato compatible con la vista
    lt_fields_view = conv_view_from_alv( ct_fields ).

    CALL METHOD lo_conf->adjust_view_dictionary
      EXPORTING
        iv_view      = iv_view
        iv_keep_text = iv_keep_text
      IMPORTING
        es_return    = es_return
      CHANGING
        ct_fields    = lt_fields_view.

* Al reves una vez modificados los datos
    ct_fields = conv_view_to_alv( lt_fields_view ).

    FREE lo_conf.

  ENDMETHOD.


  METHOD check_authorization.

    DATA lo_view TYPE REF TO zcl_al30_view.

    CREATE OBJECT lo_view.

    TRY.
        CALL METHOD lo_view->check_authorization
          EXPORTING
            iv_view_name   = iv_view_name
            iv_view_action = iv_view_action.
      CATCH zcx_al30 .
        RAISE EXCEPTION TYPE zcx_al30
          EXPORTING
            textid = zcx_al30=>no_authorization.
    ENDTRY.



    FREE lo_view.

  ENDMETHOD.


  METHOD check_changes_dict_view.
    DATA lt_fields_view TYPE zal30_i_fields_view.

    DATA lo_conf TYPE REF TO zcl_al30_conf.

    CREATE OBJECT lo_conf.


* Convierto los datos del alv a un formato compatible con la vista
    lt_fields_view = conv_view_from_alv( it_fields ).

    rv_diff = lo_conf->check_changes_dict( iv_view_name = iv_view_name
                                          it_fields  = lt_fields_view ).

    FREE lo_conf.

  ENDMETHOD.


  METHOD check_exit_class.
    DATA lo_conf TYPE REF TO zcl_al30_conf.

    CLEAR rs_return.

    CREATE OBJECT lo_conf.

    rs_return = lo_conf->check_exit_class( iv_exit_class = iv_exit_class ).

    FREE lo_conf.
  ENDMETHOD.


  METHOD check_view.

    DATA lo_conf TYPE REF TO zcl_al30_conf.

    CLEAR es_return.

    CREATE OBJECT lo_conf.

    CASE iv_operation.

      WHEN 'I'. " Insert new view

        lo_conf->check_view_insert( EXPORTING iv_name_view = iv_name_view
                                     IMPORTING es_return = es_return
                                               ev_text_view = ev_text_view ).

      WHEN 'R'. " Read the view existing

        lo_conf->check_view_read( EXPORTING iv_name_view = iv_name_view
                                     IMPORTING es_return = es_return
                                               ev_text_view = ev_text_view ).

      WHEN OTHERS.
        es_return = zcl_al30_data=>fill_return( iv_type = 'E' iv_number = '019' ).
    ENDCASE.

    FREE lo_conf.

  ENDMETHOD.


  METHOD constructor.
  ENDMETHOD.


  METHOD conv_view_from_alv.
    FIELD-SYMBOLS <ls_fields_alv> TYPE LINE OF zal30_i_fields_alv.
    DATA ls_fields_view TYPE LINE OF zal30_i_fields_view.

    CLEAR rt_fields_view.
    LOOP AT it_fields_alv ASSIGNING <ls_fields_alv>.
      MOVE-CORRESPONDING <ls_fields_alv> TO ls_fields_view.
      APPEND ls_fields_view TO rt_fields_view.
      CLEAR ls_fields_view.
    ENDLOOP.


  ENDMETHOD.


  METHOD conv_view_to_alv.
    DATA ls_fields_alv TYPE LINE OF zal30_i_fields_alv.
    FIELD-SYMBOLS <ls_fields_view> TYPE LINE OF zal30_i_fields_view.

    CLEAR rt_fields_alv.
    LOOP AT it_fields_view ASSIGNING <ls_fields_view>.
      MOVE-CORRESPONDING <ls_fields_view> TO ls_fields_alv.
      APPEND ls_fields_alv TO rt_fields_alv.
      CLEAR ls_fields_alv.
    ENDLOOP.


  ENDMETHOD.


  METHOD create_it_data_view.
    CLEAR es_return.

    DATA lo_view TYPE REF TO zcl_al30_view.

    CLEAR es_return.

    CREATE OBJECT lo_view.

    CALL METHOD lo_view->create_it_data_view
      EXPORTING
        is_view   = is_view
      IMPORTING
        es_return = es_return
        et_data   = et_data.

    FREE lo_view.
  ENDMETHOD.


  METHOD create_it_edit_data_view.
    CLEAR es_return.

    DATA lo_view TYPE REF TO zcl_al30_view.

    CLEAR es_return.

    CREATE OBJECT lo_view.

    CALL METHOD lo_view->create_it_edit_data_view
      EXPORTING
        is_view   = is_view
      IMPORTING
        es_return = es_return
        et_data   = et_data.

    FREE lo_view.
  ENDMETHOD.


  METHOD create_view.

    DATA lo_conf TYPE REF TO zcl_al30_conf.

    CLEAR es_return.

    CREATE OBJECT lo_conf.

* Primero verifico que la vista sea correcta
    check_view( EXPORTING iv_name_view = iv_name_view
                          iv_operation = 'I'
                IMPORTING es_return = es_return
                          ev_text_view = ev_text_view ).

    IF es_return-type NE 'E'.

* Ahora llamo a la creación
      lo_conf->insert_view( EXPORTING iv_name_view = iv_name_view
                                   IMPORTING es_return = es_return ).

    ENDIF.

    FREE lo_conf.

  ENDMETHOD.


  METHOD delete_view.

    DATA lo_conf TYPE REF TO zcl_al30_conf.

    CLEAR rs_return.

    CREATE OBJECT lo_conf.

    rs_return = lo_conf->delete_view( iv_name_view ).


    FREE lo_conf.

  ENDMETHOD.


  METHOD get_fieldcat_edit_view.
    CLEAR es_return.

    DATA lo_view TYPE REF TO zcl_al30_view.

    CLEAR es_return.

    CREATE OBJECT lo_view.

    CALL METHOD lo_view->get_fieldcat_edit_view
      EXPORTING
        is_view         = is_view
        it_fields       = it_fields
      IMPORTING
        es_return       = es_return
        et_fieldcat     = et_fieldcat
        et_fieldcat_key = et_fieldcat_key.

    FREE lo_view.

  ENDMETHOD.


  METHOD get_fieldcat_view.
    CLEAR es_return.

    DATA lo_view TYPE REF TO zcl_al30_view.

    CLEAR es_return.

    CREATE OBJECT lo_view.

    CALL METHOD lo_view->get_fieldcat_view
      EXPORTING
        is_view         = is_view
        it_fields       = it_fields
      IMPORTING
        es_return       = es_return
        et_fieldcat     = et_fieldcat
        et_fieldcat_key = et_fieldcat_key.

    FREE lo_view.

  ENDMETHOD.


  METHOD read_data.
    DATA lo_view TYPE REF TO zcl_al30_view.

    CLEAR es_return.

    CREATE OBJECT lo_view.

    CALL METHOD lo_view->read_data
      EXPORTING
        is_view   = is_view
        it_fields = it_fields
      IMPORTING
        es_return = es_return
        et_data   = et_data.

    FREE lo_view.

  ENDMETHOD.


  METHOD read_view.

    DATA lo_conf TYPE REF TO zcl_al30_conf.

    CLEAR es_return.

    CREATE OBJECT lo_conf.

* Leo los datos de la vista
    CALL METHOD lo_conf->read_view
      EXPORTING
        iv_name_view   = iv_name_view
      IMPORTING
        ev_text_view   = ev_text_view
        es_return      = es_return
        es_view        = es_view
        et_fields_view = et_fields.

    FREE lo_conf.

  ENDMETHOD.


  METHOD read_view_alv.
    DATA lt_fields_view TYPE zal30_i_fields_view.

    CLEAR es_return.

* Leo los datos de la vista
    CALL METHOD read_view
      EXPORTING
        iv_name_view = iv_name_view
      IMPORTING
        ev_text_view = ev_text_view
        es_return    = es_return
        es_view      = es_view
        et_fields    = lt_fields_view.

* Convierto los datos al formato ALV
    et_fields = conv_view_to_alv( lt_fields_view ).


  ENDMETHOD.


  METHOD save_data.

    DATA lo_view TYPE REF TO zcl_al30_view.

    CLEAR es_return.

    CREATE OBJECT lo_view.

    CALL METHOD lo_view->save_data
      EXPORTING
        is_view      = iv_view
      IMPORTING
        es_return    = es_return
      CHANGING
        ct_datos     = ct_datos
        ct_datos_del = ct_datos_del.

    FREE lo_view.

  ENDMETHOD.


  METHOD save_view.
    DATA lo_conf TYPE REF TO zcl_al30_conf.

    CLEAR rs_return.

    CREATE OBJECT lo_conf.

    rs_return = lo_conf->save_view( is_view = is_view
                                   it_fields_view  = it_fields ).

    FREE lo_conf.
  ENDMETHOD.


  METHOD save_view_alv.

    DATA lt_fields_view TYPE zal30_i_fields_view.

    CLEAR rs_return.

* Convierto los datos del alv a un formato compatible con la vista
    lt_fields_view = conv_view_from_alv( it_fields ).

    rs_return = save_view( is_view = is_view
                                       it_fields  = lt_fields_view ).


  ENDMETHOD.


  METHOD transport_entries.

    DATA lo_view TYPE REF TO zcl_al30_view.

    CREATE OBJECT lo_view.

    rs_return = lo_view->transport_entries( iv_name_view = iv_name_view
                                           it_keys      = it_keys ).

    FREE lo_view.

  ENDMETHOD.


  METHOD verify_change_row_data.

    DATA lo_view TYPE REF TO zcl_al30_view.

    CLEAR es_return.

    CREATE OBJECT lo_view.

    CALL METHOD lo_view->verify_change_row_data
      EXPORTING
        iv_exit_class = iv_exit_class
      IMPORTING
        es_return     = es_return
      CHANGING
        cs_row_data   = cs_row_data.

    FREE lo_view.

  ENDMETHOD.


  METHOD verify_field_data.

    DATA lo_view TYPE REF TO zcl_al30_view.

    CLEAR rs_return.

    CREATE OBJECT lo_view.

    rs_return = lo_view->verify_field_data( iv_exit_class = iv_exit_class
                                           iv_fieldname = iv_fieldname
                                           it_fields = it_fields
                                           iv_value = iv_value ).

    FREE lo_view.

  ENDMETHOD.
ENDCLASS.
