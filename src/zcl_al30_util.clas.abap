CLASS zcl_al30_util DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.


*"* public components of class ZCL_AL30_DATA
*"* do not include other source files here!!!
    TYPES: BEGIN OF ts_keys,
             objname    TYPE e071k-objname,
             object     TYPE e071k-object,
             mastertype TYPE e071k-mastertype,
             mastername TYPE e071k-mastername,
             viewname   TYPE e071k-viewname,
             key        TYPE e071k-tabkey,
           END OF ts_keys.
    TYPES: tt_keys TYPE STANDARD TABLE OF ts_keys WITH EMPTY KEY .

    CLASS-METHODS fill_return
      IMPORTING
        !iv_type         TYPE any
        !iv_number       TYPE any
        !iv_message_v1   TYPE any OPTIONAL
        !iv_message_v2   TYPE any OPTIONAL
        !iv_message_v3   TYPE any OPTIONAL
        !iv_message_v4   TYPE any OPTIONAL
        !iv_id           TYPE symsgid OPTIONAL
      RETURNING
        VALUE(rs_return) TYPE bapiret2 .
    CLASS-METHODS f4_view
      IMPORTING
        !iv_program     TYPE syrepid
        !iv_dynpro      TYPE sydynnr
        !iv_dynprofield TYPE help_info-dynprofld
      EXPORTING
        VALUE(ev_view)  TYPE tabname
      RAISING
        zcx_al30 .
    CLASS-METHODS get_fcat_control_edit_view
      RETURNING
        VALUE(rt_fieldcat_control) TYPE lvc_t_fcat .
    CLASS-METHODS get_key_value_domain
      IMPORTING
        !iv_domain TYPE domname
        !iv_langu  TYPE sylangu DEFAULT sy-langu
      EXPORTING
        !et_values TYPE zif_al30_data=>tt_key_value .
    CLASS-METHODS get_fields_struc
      IMPORTING
        !iv_struc        TYPE any
      RETURNING
        VALUE(rt_fields) TYPE string_t .
    CLASS-METHODS allowed_transport
      RETURNING
        VALUE(rv_allowed) TYPE sap_bool .
    CLASS-METHODS allowed_modify_data
      RETURNING
        VALUE(rv_allowed) TYPE sap_bool .
    CLASS-METHODS check_select_transport_order
      IMPORTING
        !iv_category TYPE e070-korrdev
      EXPORTING
        !es_return   TYPE bapiret2
      CHANGING
        !cv_order    TYPE e070-trkorr .
    CLASS-METHODS check_transport_order
      IMPORTING
        !iv_category TYPE e070-korrdev
      EXPORTING
        !es_return   TYPE bapiret2
      CHANGING
        !cv_order    TYPE e070-trkorr .
    CLASS-METHODS values_itab_2_transport_order
      IMPORTING
        !it_values  TYPE ANY TABLE
        !iv_tabname TYPE tabname
        !iv_objfunc TYPE objfunc DEFAULT zif_al30_data=>cs_order_objfunc-key_value
      EXPORTING
        !es_return  TYPE bapiret2
      CHANGING
        !cv_order   TYPE e070-trkorr .
    CLASS-METHODS transport_entries
      IMPORTING
        !iv_tabname TYPE tabname
        !it_keys    TYPE tt_keys
        !iv_objfunc TYPE objfunc
      EXPORTING
        !es_return  TYPE bapiret2
      CHANGING
        !cv_order   TYPE e070-trkorr
      RAISING
        zcx_al30 .
  PROTECTED SECTION.

    CLASS-METHODS conv_data_2_keys_trkorr
      IMPORTING
        it_values      TYPE STANDARD TABLE
        iv_tabname     TYPE tabname
      EXPORTING
        et_keys_trkorr TYPE tt_keys
      RAISING
        zcx_al30 .
    CLASS-METHODS conv_wa_2_keys_trkorr
      IMPORTING
        is_wa          TYPE any
        it_fieldlist   TYPE ddfields
      EXPORTING
        ev_keys_trkorr TYPE e071k-tabkey
      RAISING
        zcx_al30.
*"* protected components of class ZCL_AL30_DATA
*"* do not include other source files here!!!
  PRIVATE SECTION.
*"* private components of class ZCL_AL30_DATA
*"* do not include other source files here!!!
ENDCLASS.



CLASS zcl_al30_util IMPLEMENTATION.


  METHOD allowed_modify_data.
    DATA ld_transp_state  TYPE t000-cccoractiv.
    DATA ld_cliindep_state  TYPE t000-ccnocliind.
    DATA ld_client_state  TYPE t000-cccategory.

    CALL FUNCTION 'VIEW_GET_CLIENT_STATE'
      IMPORTING
        transp_state   = ld_transp_state
        cliindep_state = ld_cliindep_state
        client_state   = ld_client_state.

    IF ld_cliindep_state = space.
      rv_allowed = abap_true.
    ELSE.
      rv_allowed = abap_false.
    ENDIF.
  ENDMETHOD.


  METHOD allowed_transport.
    DATA ld_transp_state  TYPE t000-cccoractiv.
    DATA ld_cliindep_state  TYPE t000-ccnocliind.
    DATA ld_client_state  TYPE t000-cccategory.

    CALL FUNCTION 'VIEW_GET_CLIENT_STATE'
      IMPORTING
        transp_state   = ld_transp_state
        cliindep_state = ld_cliindep_state
        client_state   = ld_client_state.

    IF ld_transp_state = '3' OR ld_transp_state = '2'.
      rv_allowed = abap_false.
* Si el sistema no se puede modificar, tampoco se podra transportar.
    ELSEIF allowed_modify_data( ) = abap_false.
      rv_allowed = abap_false.
    ELSE.
      rv_allowed = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD check_select_transport_order.

* Si se informa orden se valida que sea correcta
    IF cv_order IS NOT INITIAL.
      check_transport_order(
        EXPORTING
          iv_category = iv_category
        IMPORTING
          es_return   = DATA(ls_return)
        CHANGING
          cv_order    = cv_order ).
    ENDIF.

* Si hay un error se mostrará el seleccionable de ordenes
    IF ls_return-type = zif_al30_data=>cs_msg_type-error OR cv_order IS INITIAL.
      CALL FUNCTION 'TR_ORDER_CHOICE_CORRECTION'
        EXPORTING
          iv_category = iv_category
        IMPORTING
          ev_order    = cv_order
*         ev_task     = ld_task
        EXCEPTIONS
          OTHERS      = 3.
      IF sy-subrc NE 0.
        es_return = fill_return( iv_type = 'S' iv_number = '033' ). " Mensaje de cancelacion.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD check_transport_order.
    DATA lt_req_head TYPE trwbo_request_headers.
    DATA lt_req TYPE trwbo_requests.
    DATA lv_trfunction TYPE e070-trfunction.

    CLEAR es_return.

    " Tipo ed orden segun la categoriaw
    CASE iv_category.
      WHEN 'SYST'. " Workbench
        lv_trfunction = 'S'.
      WHEN 'CUST'. " Customizing
        lv_trfunction = 'Q'.
    ENDCASE.

* Se lee las tareas de las ordenes para ver si hay alguna valida.
    CALL FUNCTION 'TR_READ_REQUEST_WITH_TASKS'
      EXPORTING
        iv_trkorr          = cv_order
      IMPORTING
        et_request_headers = lt_req_head
        et_requests        = lt_req
      EXCEPTIONS
        invalid_input      = 1
        OTHERS             = 2.

    IF sy-subrc <> 0.
      es_return =  fill_return( iv_type = 'E'
                                iv_number = sy-msgno
                                iv_message_v1 = sy-msgv1
                                iv_message_v2 = sy-msgv2
                                iv_message_v3 = sy-msgv3
                                iv_message_v4 = sy-msgv4
                                iv_id         = sy-msgid ).

    ELSE.

* Se mira si hay alguna tarea valida para el usuario. Si es asi se reemplaza por la pasada por parámetro.
      READ TABLE lt_req_head ASSIGNING FIELD-SYMBOL(<ls_req_head>) WITH KEY trfunction = lv_trfunction
                                                                         trstatus = 'D' " Modificable
                                                                         as4user = sy-uname.
      IF sy-subrc = 0.
        cv_order = <ls_req_head>-trkorr.
      ELSE.
        " Si no la tiene, se lee el registro de la orden padre para poder crear una tarea a la orden
        LOOP AT lt_req_head ASSIGNING <ls_req_head> WHERE strkorr IS INITIAL.
          EXIT.
        ENDLOOP.
        IF sy-subrc = 0.

          CALL FUNCTION 'TRINT_INSERT_NEW_COMM'
            EXPORTING
              wi_kurztext       = <ls_req_head>-as4text
              wi_trfunction     = lv_trfunction
              iv_username       = sy-uname
              wi_strkorr        = <ls_req_head>-strkorr
              wi_client         = sy-mandt
            IMPORTING
              we_trkorr         = cv_order
            EXCEPTIONS
              no_systemname     = 1
              no_systemtype     = 2
              no_authorization  = 3
              db_access_error   = 4
              file_access_error = 5
              enqueue_error     = 6
              number_range_full = 7
              invalid_input     = 8
              OTHERS            = 9.
          IF sy-subrc <> 0.
            es_return =  fill_return( iv_type = 'E'
                                      iv_number = sy-msgno
                                      iv_message_v1 = sy-msgv1
                                      iv_message_v2 = sy-msgv2
                                      iv_message_v3 = sy-msgv3
                                      iv_message_v4 = sy-msgv4
                                      iv_id         = sy-msgid ).

          ELSE.

          ENDIF.

        ENDIF.

      ENDIF.

    ENDIF.
  ENDMETHOD.


  METHOD conv_data_2_keys_trkorr.
    DATA lo_struct_ref TYPE REF TO cl_abap_structdescr.
    DATA ls_key TYPE LINE OF tt_keys.
    DATA lt_tables TYPE STANDARD TABLE OF tabname.

    CLEAR et_keys_trkorr.

    " Primero hay que mirar el tipo de tabla pasada.
    SELECT SINGLE tabclass FROM dd02l INTO @DATA(lv_tabclass)
             WHERE tabname  = @iv_tabname
               AND as4local = 'A'.

    IF lv_tabclass = 'VIEW'.
      " Si es una vista hay que leer sus tablas
      SELECT tabname INTO TABLE lt_tables
             FROM dd26s
             WHERE viewname = iv_tabname.
*
    ELSE. " Si es una tabla se añade la tabla directamente
      INSERT iv_tabname INTO TABLE lt_tables.
    ENDIF.

* Mediante el RTTS obtengo la definicion de la tabla. Cualquier excepcion o error lanzo una excepcion generica.
    TRY.

        " Se recorre las tablas para ir montando la clave
        LOOP AT lt_tables ASSIGNING FIELD-SYMBOL(<ls_tables>).
          lo_struct_ref ?= cl_abap_typedescr=>describe_by_name( iv_tabname ).

          IF lo_struct_ref IS BOUND.

            CALL METHOD lo_struct_ref->get_ddic_field_list
              EXPORTING
                p_including_substructres = abap_true
              RECEIVING
                p_field_list             = DATA(lt_field_list)
              EXCEPTIONS
                not_found                = 1
                no_ddic_type             = 2
                OTHERS                   = 3.
            IF sy-subrc NE 0.
              RAISE EXCEPTION TYPE zcx_al30
                EXPORTING
                  textid = zcx_al30=>invalid_params.
            ENDIF.

* Se recorre los valores para ir construyendo la clave.
            LOOP AT it_values ASSIGNING FIELD-SYMBOL(<wa>).
              CLEAR: ls_key.

              TRY.
                  CALL METHOD conv_wa_2_keys_trkorr
                    EXPORTING
                      is_wa          = <wa>
                      it_fieldlist   = lt_field_list
                    IMPORTING
                      ev_keys_trkorr = ls_key-key.

                  IF lv_tabclass = 'VIEW'.
                    ls_key-mastername = ls_key-viewname = iv_tabname.
                    ls_key-objname = <ls_tables>.
                    ls_key-mastertype = 'VDAT'.
                    ls_key-object = 'TABU'.
                  ELSE. " En una tabla todo los campos sin iguales
                    ls_key-mastername = ls_key-objname = ls_key-viewname = iv_tabname.
                    ls_key-mastertype = ls_key-object = 'TABU'.
                  ENDIF.

                  APPEND ls_key TO et_keys_trkorr.

                CATCH zcx_al30 .
                  RAISE EXCEPTION TYPE zcx_al30
                    EXPORTING
                      textid = zcx_al30=>invalid_params.
              ENDTRY.


            ENDLOOP.
          ELSE.
            RAISE EXCEPTION TYPE zcx_al30
              EXPORTING
                textid = zcx_al30=>invalid_params.
          ENDIF.
        ENDLOOP.
      CATCH cx_root.
        RAISE EXCEPTION TYPE zcx_al30
          EXPORTING
            textid = zcx_al30=>invalid_params.
    ENDTRY.


  ENDMETHOD.


  METHOD conv_wa_2_keys_trkorr.
    FIELD-SYMBOLS <wa> TYPE any.
    FIELD-SYMBOLS <ls_field_list> TYPE LINE OF ddfields.
    FIELD-SYMBOLS <field> TYPE any.
    DATA lo_struct_ref TYPE REF TO cl_abap_structdescr.

    DATA ld_max_len TYPE int4.
    DATA ld_start TYPE int4.
    DATA ld_len_key TYPE i.

    CLEAR ev_keys_trkorr.

* Obtengo la longitud del campo donde se guarda la clave.
    DESCRIBE FIELD ev_keys_trkorr LENGTH ld_len_key IN CHARACTER MODE.

    LOOP AT it_fieldlist ASSIGNING <ls_field_list> WHERE keyflag = abap_true.
* Si el campo clave no existe en la tabla de valores, se lanza la excepcion generica. Hay incongruencia de datos y no se puede
* continuar.
      ASSIGN COMPONENT <ls_field_list>-fieldname OF STRUCTURE is_wa TO <field>.
      IF sy-subrc NE 0.
        RAISE EXCEPTION TYPE zcx_al30
          EXPORTING
            textid = zcx_al30=>invalid_params.
      ENDIF.

* Sumo la longitud del campo a la actual. Si esta supera la longitud del campo clave pongo
* un asterisco y salgo del proceso.
      ld_max_len = <ls_field_list>-leng + ld_start.
      IF ld_max_len > ld_len_key.
        ev_keys_trkorr+ld_start(1) = '*'.
        EXIT.
      ENDIF.

* Si el campo que se lee es el mandante y esta en blanco le pongo el mandante actual para evitar que de errores.
      IF <ls_field_list>-fieldname = 'MANDT' AND <field> IS INITIAL.
        ev_keys_trkorr+ld_start(<ls_field_list>-leng) = sy-mandt.
      ELSE.
        ev_keys_trkorr+ld_start(<ls_field_list>-leng) = <field>.
      ENDIF.

      ADD <ls_field_list>-leng TO ld_start.

    ENDLOOP.
  ENDMETHOD.


  METHOD f4_view.
    TYPES: BEGIN OF ty_values,
             tabname TYPE tabname,
             text    TYPE as4text,
           END OF ty_values.

    DATA lt_values TYPE STANDARD TABLE OF ty_values.
    DATA lt_return_tab TYPE TABLE OF ddshretval.
    FIELD-SYMBOLS <ls_return_tab> TYPE ddshretval.
    FIELD-SYMBOLS <ls_values> TYPE ty_values.

    SELECT a~tabname b~ddtext INTO TABLE lt_values
           FROM zal30_t_view AS a LEFT OUTER JOIN dd02t AS b ON
                b~tabname = a~tabname
                AND b~ddlanguage = sy-langu.

    IF sy-subrc = 0.
      CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
        EXPORTING
          retfield    = 'TABNAME'
          dynpprog    = iv_program
          dynpnr      = iv_dynpro
          dynprofield = iv_dynprofield
          value_org   = 'S'
        TABLES
          value_tab   = lt_values
          return_tab  = lt_return_tab[].

* Miro que registro se ha seleccionado y lo devuelvo al parámetro.
      READ TABLE lt_return_tab ASSIGNING <ls_return_tab> INDEX 1.
      IF sy-subrc = 0.
        READ TABLE lt_values ASSIGNING <ls_values> INDEX <ls_return_tab>-recordpos.
        IF sy-subrc = 0.
          ev_view = <ls_values>-tabname.
        ENDIF.
      ENDIF.

    ELSE.
      RAISE EXCEPTION TYPE zcx_al30
        EXPORTING
          textid = zcx_al30=>no_values_f4_view.

    ENDIF.

  ENDMETHOD.


  METHOD fill_return.

    CLEAR rs_return.

    rs_return-type = iv_type.

    rs_return-id = COND #( WHEN iv_id IS NOT INITIAL THEN iv_id ELSE zif_al30_data=>cv_msg_id ).
    rs_return-number = iv_number.
    rs_return-message_v1 = iv_message_v1.
    rs_return-message_v2 = iv_message_v2.
    rs_return-message_v3 = iv_message_v3.
    rs_return-message_v4 = iv_message_v4.


    CALL FUNCTION 'BAPI_MESSAGE_GETDETAIL'
      EXPORTING
        id         = rs_return-id
        number     = rs_return-number
        language   = sy-langu
        textformat = 'ASC'
        message_v1 = rs_return-message_v1
        message_v2 = rs_return-message_v2
        message_v3 = rs_return-message_v3
        message_v4 = rs_return-message_v4
      IMPORTING
        message    = rs_return-message.

  ENDMETHOD.


  METHOD get_fcat_control_edit_view.

    DATA ls_fieldcat TYPE LINE OF lvc_t_fcat.

* Operación del registro: 'I' -> Insertar. 'U' -> Actualizar. 'D' -> Borrar.
    ls_fieldcat-fieldname = zif_al30_data=>cv_field_updkz.
    ls_fieldcat-rollname = 'CDCHNGIND'.
    ls_fieldcat-tech = 'X'.
    APPEND ls_fieldcat TO rt_fieldcat_control.
    CLEAR ls_fieldcat.

* Línea del registro según la lectura en el diccionario
* Este campo se usa sobretodo para saber si un registro viene del diccionario, o no.
    ls_fieldcat-fieldname = zif_al30_data=>cv_field_tabix_ddic.
    ls_fieldcat-rollname = 'SYTABIX'.
    ls_fieldcat-ref_table = 'SYST'.
    ls_fieldcat-ref_field = 'TABIX'.
    ls_fieldcat-tech = 'X'.
    APPEND ls_fieldcat TO rt_fieldcat_control.
    CLEAR ls_fieldcat.

  ENDMETHOD.


  METHOD get_fields_struc.

    CLEAR rt_fields.

    DATA(lo_struc) = CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_name( iv_struc ) ).

    rt_fields = VALUE #( FOR <components> IN lo_struc->get_components( ) ( <components>-name ) ).

  ENDMETHOD.


  METHOD get_key_value_domain.
    DATA lt_valores TYPE STANDARD TABLE OF dd07v.

    CLEAR: et_values.

    CALL FUNCTION 'DDIF_DOMA_GET'
      EXPORTING
        name      = iv_domain
        langu     = iv_langu
      TABLES
        dd07v_tab = lt_valores[].

    IF sy-subrc = 0.

      et_values = VALUE #( FOR <ls_valores> IN lt_valores ( key = <ls_valores>-domvalue_l value = <ls_valores>-ddtext ) ).

    ENDIF.
  ENDMETHOD.


  METHOD transport_entries.
    DATA lt_e071k TYPE STANDARD TABLE OF e071k.
    DATA lt_e071 TYPE STANDARD TABLE OF e071.
    DATA lv_category TYPE trcateg.

    CLEAR: es_return.

    IF cv_order IS INITIAL. " La orden es obligatorio
      RAISE EXCEPTION TYPE zcx_al30
        EXPORTING
          textid = zcx_al30=>invalid_params.
    ENDIF.

    SELECT SINGLE tabclass, contflag FROM dd02l INTO @DATA(ls_dd02l)
             WHERE tabname  = @iv_tabname
               AND as4local = 'A'.
    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_al30
        EXPORTING
          textid = zcx_al30=>invalid_params.
    ENDIF.

* La cabecera depende si hablamos de vista o tabla
    IF ls_dd02l-tabclass = 'VIEW'.
      APPEND VALUE #( pgmid = 'R3TR' object = 'VDAT' obj_name = iv_tabname objfunc = iv_objfunc ) TO lt_e071.
    ELSE.
      APPEND VALUE #( pgmid = 'R3TR' object = 'TABU' obj_name = iv_tabname objfunc = iv_objfunc ) TO lt_e071.
    ENDIF.

* Se añade las entradas de los campos clave. Esta estructura ya tiene los campos informados si es una vista o
* o es una tabla normal
    LOOP AT it_keys ASSIGNING FIELD-SYMBOL(<ls_key>).
      APPEND VALUE #( pgmid = 'R3TR' object = <ls_key>-object
                      mastertype = <ls_key>-mastertype mastername = <ls_key>-mastername
                      objname = <ls_key>-objname viewname = <ls_key>-viewname
                      tabkey = <ls_key>-key  ) TO lt_e071k.
    ENDLOOP.

*   Categoria de la tabla
    IF ls_dd02l-contflag = 'C' OR ls_dd02l-contflag = 'G'.
      lv_category = 'CUST'.
    ELSE.
      lv_category = 'SYST'.
    ENDIF.

* Se valida que la orden sea correcta
    check_transport_order(
      EXPORTING
        iv_category = lv_category
      IMPORTING
        es_return   = es_return
      CHANGING
        cv_order    = cv_order ).

    IF es_return IS INITIAL. " Si la orden es corecta se continua el proceso

      CALL FUNCTION 'TR_APPEND_TO_COMM_OBJS_KEYS'
        EXPORTING
          wi_simulation         = ' '
          wi_suppress_key_check = ' '
          wi_trkorr             = cv_order
        TABLES
          wt_e071               = lt_e071
          wt_e071k              = lt_e071k
        EXCEPTIONS
          OTHERS                = 68.
      IF sy-subrc = 0.
        es_return = fill_return( iv_type = zif_al30_data=>cs_msg_type-success iv_number = '028' iv_message_v1 = cv_order ).
      ELSE.
        es_return = fill_return( iv_type = zif_al30_data=>cs_msg_type-error iv_id = sy-msgid iv_number = sy-msgno iv_message_v1 = sy-msgv1
                                             iv_message_v2 = sy-msgv2 iv_message_v3 = sy-msgv3 iv_message_v4 = sy-msgv4 ).
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD values_itab_2_transport_order.

    CLEAR es_return.

    TRY.

        " Se convierte los valores a una clave para poder asignarla a una orden de transporte
        conv_data_2_keys_trkorr( EXPORTING it_values = it_values iv_tabname = iv_tabname IMPORTING et_keys_trkorr = DATA(lt_keys) ).

        transport_entries(
                EXPORTING
                  iv_tabname = iv_tabname
                  it_keys    = lt_keys
                  iv_objfunc = iv_objfunc
                IMPORTING
                  es_return  = es_return
                CHANGING
                cv_order = cv_order   ).

      CATCH zcx_al30.
        es_return = fill_return( iv_type = zif_al30_data=>cs_msg_type-error iv_number = '029' iv_message_v1 = cv_order ).
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
