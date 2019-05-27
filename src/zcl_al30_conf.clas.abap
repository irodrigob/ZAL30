CLASS zcl_al30_conf DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
*"* public components of class ZCL_AL30_CONF
*"* do not include other source files here!!!
    TYPE-POOLS abap .

    METHODS read_view
      IMPORTING
        !iv_name_view   TYPE tabname
      EXPORTING
        !ev_text_view   TYPE as4text
        !es_return      TYPE bapiret2
        !es_view        TYPE zal30_t_view
        !et_fields_view TYPE zal30_i_fields_view .
    METHODS check_view_ddic
      IMPORTING
        !iv_name_view TYPE tabname
      EXPORTING
        !e_return    TYPE bapiret2
        !e_text_view TYPE as4text .
    METHODS insert_view
      IMPORTING
        !iv_name_view TYPE tabname
      EXPORTING
        !es_return    TYPE bapiret2 .
    METHODS delete_view
      IMPORTING
        !iv_name_view    TYPE tabname
      RETURNING
        VALUE(rs_return) TYPE bapiret2 .
    METHODS save_view
      IMPORTING
        !is_view         TYPE zal30_t_view
        !it_fields_view  TYPE zal30_i_fields_view
      RETURNING
        VALUE(rs_return) TYPE bapiret2 .
    METHODS check_view_insert
      IMPORTING
        !iv_name_view TYPE tabname
      EXPORTING
        !es_return    TYPE bapiret2
        !ev_text_view TYPE as4text .
    METHODS check_view_read
      IMPORTING
        !iv_name_view TYPE tabname
      EXPORTING
        !es_return    TYPE bapiret2
        !ev_text_view TYPE as4text .
    METHODS check_changes_dict
      IMPORTING
        !it_fields     TYPE zal30_i_fields_view OPTIONAL
        !iv_view_name  TYPE tabname
      RETURNING
        VALUE(rv_diff) TYPE sap_bool .
    METHODS adjust_view_dictionary
      IMPORTING
        !iv_view         TYPE zal30_t_view
        !iv_keep_text    TYPE sap_bool DEFAULT abap_true
      EXPORTING
        VALUE(es_return) TYPE bapiret2
        !ev_text_view    TYPE as4text
      CHANGING
        !ct_fields       TYPE zal30_i_fields_view .
    METHODS check_exit_class
      IMPORTING
        !iv_exit_class   TYPE zal30_e_exit_class
      RETURNING
        VALUE(rs_return) TYPE bapiret2 .
  PROTECTED SECTION.
*"* protected components of class ZCL_AL30_CONF
*"* do not include other source files here!!!

    METHODS adjust_field_text_view
      IMPORTING
        !iv_name_view   TYPE tabname
      CHANGING
        !ct_fields_text TYPE zal30_i_fieldst .
    METHODS save_text_of_view
      IMPORTING
        !iv_text_fields  TYPE zal30_i_fieldst
      RETURNING
        VALUE(rs_return) TYPE bapiret2 .
    METHODS read_text_of_view
      IMPORTING
        !iv_name_view   TYPE tabname
        !iv_spras       TYPE spras DEFAULT sy-langu
      EXPORTING
        !ev_text_fields TYPE zal30_i_fieldst .
    METHODS save_view_ddic
      IMPORTING
        !is_view        TYPE zal30_t_view
        !it_fields      TYPE zal30_i_fields
        !it_fields_text TYPE zal30_i_fieldst
      RAISING
        zcx_al30 .
    METHODS split_fields_alv
      IMPORTING
        !it_fields_view TYPE zal30_i_fields_view
      EXPORTING
        !et_fields      TYPE zal30_i_fields
        !et_fields_text TYPE zal30_i_fieldst .
    METHODS join_fields_alv
      IMPORTING
        !it_fields            TYPE zal30_i_fields
        !it_fields_text       TYPE zal30_i_fieldst
      RETURNING
        VALUE(et_fields_view) TYPE zal30_i_fields_view .
    METHODS get_fields_view
      IMPORTING
        !iv_name_view   TYPE any
        !iv_langu       TYPE sylangu DEFAULT sy-langu
      EXPORTING
        !es_dd02v       TYPE dd02v
        !et_fields      TYPE zal30_i_fields
        !et_fields_text TYPE zal30_i_fieldst
      RAISING
        zcx_al30 .
    METHODS get_view_ddic
      IMPORTING
        !iv_name_view TYPE any
        !iv_langu     TYPE sylangu DEFAULT sy-langu
      EXPORTING
        !es_dd02v     TYPE dd02v
        !et_dd03p     TYPE dd03ptab
      RAISING
        zcx_al30 .
  PRIVATE SECTION.
*"* private components of class ZCL_AL30_CONF
*"* do not include other source files here!!!
ENDCLASS.



CLASS zcl_al30_conf IMPLEMENTATION.


  METHOD adjust_field_text_view.
    DATA lt_fields_text_ddic TYPE zal30_i_fieldst.
    DATA lt_fields_text_view TYPE zal30_i_fieldst.
    FIELD-SYMBOLS <ls_fields_text_ddic> TYPE LINE OF zal30_i_fieldst.
    FIELD-SYMBOLS <ls_fields_text_view> TYPE LINE OF zal30_i_fieldst.

* Leo los textos de la vista que esta en la tabla cuyo idioma sea distinto al
* del logon
    SELECT * INTO TABLE lt_fields_text_view
           FROM zal30_t_fieldst
           WHERE tabname = iv_name_view
                 AND spras NE sy-langu.

    IF sy-subrc = 0.

* Ahora proceso los textos del resto de idioma.
      LOOP AT lt_fields_text_view ASSIGNING <ls_fields_text_view>.
        AT NEW spras.
          CLEAR lt_fields_text_ddic.
          CALL METHOD read_text_of_view
            EXPORTING
              iv_name_view   = iv_name_view
              iv_spras       = <ls_fields_text_view>-spras
            IMPORTING
              ev_text_fields = lt_fields_text_ddic.

        ENDAT.
* Si hay textos de campos voy procesando campos a campo
        IF lt_fields_text_ddic IS NOT INITIAL.

* Miro si el campo ya existe para el ididioma del logon.
          READ TABLE ct_fields_text TRANSPORTING NO FIELDS
               WITH KEY spras = sy-langu
                        fieldname = <ls_fields_text_view>-fieldname.
          IF sy-subrc = 0.

* Leo los textos para el campo e idioma que estoy tratando y lo aÃ±ado a la tabla
* de textos existente.
            READ TABLE lt_fields_text_ddic ASSIGNING <ls_fields_text_ddic>
               WITH KEY fieldname = <ls_fields_text_view>-fieldname.

            IF sy-subrc = 0.
              APPEND <ls_fields_text_ddic> TO ct_fields_text.
            ENDIF.

          ENDIF.

        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD adjust_view_dictionary.
    DATA ls_view TYPE zal30_t_view.
    DATA lt_fields TYPE zal30_i_fields.
    DATA lt_fields_text TYPE zal30_i_fieldst.
    DATA ls_dd02v TYPE dd02v.
    DATA ls_return TYPE bapiret2.
    FIELD-SYMBOLS <ls_fields> TYPE LINE OF zal30_i_fields_view.
    FIELD-SYMBOLS <ls_fields_ddic> TYPE LINE OF zal30_i_fields.
    FIELD-SYMBOLS <ls_fields_text_ddic> TYPE LINE OF zal30_i_fieldst.
    FIELD-SYMBOLS <ls_fields_text_view> TYPE LINE OF zal30_i_fieldst.

    CLEAR: es_return, ev_text_view.

* Leo los campos de la vista en formato listo para ser tratado
    TRY.
        CALL METHOD get_fields_view
          EXPORTING
            iv_name_view   = iv_view-tabname
          IMPORTING
            es_dd02v       = ls_dd02v
            et_fields      = lt_fields
            et_fields_text = lt_fields_text.

      CATCH zcx_al30 .
        es_return = zcl_al30_data=>fill_return( iv_type = 'E' iv_number = '020' iv_message_v1 = iv_view-tabname ).
        EXIT.
    ENDTRY.

* Recupero el texto
    ev_text_view = ls_dd02v-ddtext.

* A continuaciÃ³n miro que campos del parÃ¡metro C_FIELDS esta en la del diccionario.
* Si esta reemplazo los valores por lo que hay en C_FIELDS
    LOOP AT ct_fields ASSIGNING <ls_fields>.

      READ TABLE lt_fields ASSIGNING <ls_fields_ddic>
                           WITH KEY fieldname = <ls_fields>-fieldname.
      IF sy-subrc = 0.
        MOVE-CORRESPONDING <ls_fields> TO <ls_fields_ddic>.

* Lo mismo para los textos, si la opcion de mantener los textos es verdaderda.
        IF iv_keep_text = abap_true.
          READ TABLE lt_fields_text ASSIGNING <ls_fields_text_ddic>
                             WITH KEY fieldname = <ls_fields>-fieldname.
          IF sy-subrc = 0.
            MOVE-CORRESPONDING <ls_fields> TO <ls_fields_text_ddic>.
          ENDIF.
        ENDIF.

      ENDIF.

    ENDLOOP.

* Ajusto los textos de los idiomas distintos al de logon.
    CALL METHOD adjust_field_text_view
      EXPORTING
        iv_name_view   = iv_view-tabname
      CHANGING
        ct_fields_text = lt_fields_text.


* Borro la vista para insertarla mÃ¡s adelante.
    ls_return = delete_view( iv_name_view = iv_view-tabname ).
    IF ls_return-type NE 'E'.

* Finalmente grabo la vista
      TRY.
          save_view_ddic( is_view = iv_view
                                     it_fields = lt_fields
                                     it_fields_text = lt_fields_text ).

          es_return = zcl_al30_data=>fill_return( iv_type = 'S' iv_number = '034' iv_message_v1 = iv_view-tabname ).

* Finalmente refresco c_fields con los nuevos campos
          ct_fields = join_fields_alv( it_fields = lt_fields
                                      it_fields_text = lt_fields_text ).

        CATCH zcx_al30.
          es_return = zcl_al30_data=>fill_return( iv_type = 'E' iv_number = '033' iv_message_v1 = iv_view-tabname ).
      ENDTRY.

    ELSE.
      es_return = zcl_al30_data=>fill_return( iv_type = 'E' iv_number = '033' iv_message_v1 = iv_view-tabname ).
    ENDIF.

  ENDMETHOD.


  METHOD check_changes_dict.
    DATA ld_name TYPE ddobjname.
    DATA ls_dd02v TYPE dd02v.
    DATA lt_dd03p TYPE TABLE OF dd03p.
    DATA ld_num_fields TYPE i.
    DATA ld_num_fields_ddic TYPE i.
    DATA lt_fields TYPE zal30_i_fields_view.
    FIELD-SYMBOLS <ls_dd03p> TYPE dd03p.
    FIELD-SYMBOLS <ls_fields> TYPE LINE OF zal30_i_fields_view.

    ld_name = iv_view_name.

    rv_diff = abap_false.

* Si no hay campos los leo para comparar.
    IF it_fields IS INITIAL.
      CALL METHOD read_view
        EXPORTING
          iv_name_view   = iv_view_name
        IMPORTING
          et_fields_view = lt_fields.
    ELSE.
      lt_fields = it_fields.
    ENDIF.

* Obtengo informaciÃ³n de la tabla
    CALL FUNCTION 'DDIF_TABL_GET'
      EXPORTING
        name          = ld_name
        state         = 'A' " Active version
        langu         = sy-langu
      IMPORTING
        dd02v_wa      = ls_dd02v
      TABLES
        dd03p_tab     = lt_dd03p[]
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.

    IF sy-subrc = 0 AND lt_dd03p[] IS NOT INITIAL.

* La primer verificaciÃ³n consiste en mirar el numero de campos de la vista actual y la del diccionario
      DESCRIBE TABLE lt_fields LINES ld_num_fields.
      DESCRIBE TABLE lt_dd03p LINES ld_num_fields_ddic.
      IF ld_num_fields_ddic = ld_num_fields.

* Si no son iguales miro campo por campo.
        LOOP AT lt_fields ASSIGNING <ls_fields>.
          READ TABLE lt_dd03p ASSIGNING <ls_dd03p> WITH KEY fieldname = <ls_fields>-fieldname.
          IF sy-subrc = 0.

* Si el existe el mismo campo miro los textos
            IF <ls_fields>-scrtext_s NE <ls_dd03p>-scrtext_s
               OR <ls_fields>-scrtext_m NE <ls_dd03p>-scrtext_m
               OR <ls_fields>-scrtext_l NE <ls_dd03p>-scrtext_l
               OR <ls_fields>-reptext  NE <ls_dd03p>-reptext.
              rv_diff = abap_true.
              EXIT.
            ENDIF.

          ELSE.
* A la primera diferencia salgo, no tiene sentido leer el resto de campos.
            rv_diff = abap_true.
            EXIT.
          ENDIF.
        ENDLOOP.


      ELSE.
        rv_diff = abap_true.
      ENDIF.


    ENDIF.

  ENDMETHOD.


  METHOD check_exit_class.
    DATA ld_not_active TYPE  sap_bool.
    DATA ls_impl TYPE seorelkey.
    DATA ld_deleted TYPE sap_bool.
    CLEAR rs_return.

* Primero se mira que la clase exista
    CALL FUNCTION 'SEO_CLASS_EXISTENCE_CHECK'
      EXPORTING
        clskey        = iv_exit_class
      IMPORTING
        not_active    = ld_not_active
      EXCEPTIONS
        not_specified = 1
        not_existing  = 2
        is_interface  = 3
        no_text       = 4
        inconsistent  = 5
        OTHERS        = 6.

    IF sy-subrc NE 0 OR ld_not_active = abap_true.
      rs_return = zcl_al30_data=>fill_return( iv_type = 'E' iv_number = '038' iv_message_v1 = iv_exit_class ).
    ELSE.

* Segundo se mira si tiene la interface exit
      ls_impl-clsname = iv_exit_class.
      ls_impl-refclsname = zif_al30_data=>cv_intf_exit.
      CALL FUNCTION 'SEO_IMPLEMENTG_EXISTENCE_CHECK'
        EXPORTING
          impkey             = ls_impl
        IMPORTING
          deleted            = ld_deleted
        EXCEPTIONS
          class_not_existing = 1
          not_specified      = 2
          not_existing       = 3
          is_inheritance     = 4
          is_comprising      = 5
          inconsistent       = 6
          OTHERS             = 7.

      IF ld_deleted = abap_true OR sy-subrc NE 0.
        rs_return = zcl_al30_data=>fill_return( iv_type = 'E' iv_number = '039' iv_message_v1 = zif_al30_data=>cv_intf_exit ).
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD check_view_ddic.
    DATA ld_mainflag TYPE dd02v-mainflag.
    DATA ls_dd02v TYPE dd02v.
    DATA lt_dd03p TYPE TABLE OF dd03p.
    FIELD-SYMBOLS <ls_dd03p> TYPE dd03p.


    CLEAR: e_return, e_text_view.

* Verificacion similar que se hace cuando se crea un diÃ¡logo de actualizacion. Que esta
* en el include: MSVIMF35 form: get_ddic_info_tabl.
    TRY.
        CALL METHOD get_view_ddic
          EXPORTING
            iv_name_view = iv_name_view
          IMPORTING
            es_dd02v     = ls_dd02v
            et_dd03p     = lt_dd03p[].
      CATCH zcx_al30 .
        e_return = zcl_al30_data=>fill_return( iv_type = 'E' iv_number = '020' iv_message_v1 = iv_name_view ).
        EXIT.
    ENDTRY.


    e_text_view = ls_dd02v-ddtext. " Texto del objeto

* Verifico que en la tabla no tenga campos de tipo string
    READ TABLE lt_dd03p TRANSPORTING NO FIELDS WITH KEY datatype = 'STRG'.
    IF sy-subrc = 0.
      e_return = zcl_al30_data=>fill_return( iv_type = 'E' iv_number = '002' iv_message_v1 = iv_name_view ).

    ELSE.

* Verifico que la tabla tenga el flag de actualizaciÃ³n permitida
      IF ls_dd02v-mainflag = 'N'. " No permitido.
        e_return = zcl_al30_data=>fill_return( iv_type = 'E' iv_number = '003' iv_message_v1 = iv_name_view ).
      ENDIF.

    ENDIF.


  ENDMETHOD.


  METHOD check_view_insert.
    DATA ls_view TYPE zal30_t_view.

* Primero chequeo que la vista no exista ya
    SELECT SINGLE * INTO ls_view FROM zal30_t_view WHERE tabname = iv_name_view.
    IF sy-subrc NE 0.

* DespuÃ©s lanzo los chequeos de la vista/tabla del diccionario
      CALL METHOD check_view_ddic
        EXPORTING
          iv_name_view = iv_name_view
        IMPORTING
          e_return    = es_return
          e_text_view = ev_text_view.

    ELSE.
      es_return = zcl_al30_data=>fill_return( iv_type = 'E' iv_number = '011' iv_message_v1 = iv_name_view ).
    ENDIF.

  ENDMETHOD.


  METHOD check_view_read.
    DATA ls_view TYPE zal30_t_view.

* Primero chequeo que la vista no exista ya
    SELECT SINGLE * INTO ls_view FROM zal30_t_view WHERE tabname = iv_name_view.
    IF sy-subrc = 0.

* DespuÃ©s lanzo los chequeos de la vista/tabla del diccionario
      CALL METHOD check_view_ddic
        EXPORTING
          iv_name_view = iv_name_view
        IMPORTING
          e_return    = es_return
          e_text_view = ev_text_view.

    ELSE.
      es_return = zcl_al30_data=>fill_return( iv_type = 'E' iv_number = '001' iv_message_v1 = iv_name_view ).
    ENDIF.

  ENDMETHOD.


  METHOD delete_view.
    CLEAR rs_return.

* Borro los datos principales de la vista
    DELETE FROM zal30_t_view WHERE tabname = iv_name_view.
    IF sy-subrc = 0.
      DELETE FROM zal30_t_fields WHERE tabname = iv_name_view.
      IF sy-subrc = 0.
        DELETE FROM zal30_t_fieldst WHERE tabname = iv_name_view.
        IF sy-subrc = 0.
          COMMIT WORK.
          rs_return = zcl_al30_data=>fill_return( iv_type = 'S' iv_number = '010' iv_message_v1 = iv_name_view ).
        ELSE.
          ROLLBACK WORK.
          rs_return = zcl_al30_data=>fill_return( iv_type = 'E' iv_number = '017' iv_message_v1 = iv_name_view ).
        ENDIF.
      ELSE.
        ROLLBACK WORK.
        rs_return = zcl_al30_data=>fill_return( iv_type = 'E' iv_number = '009' iv_message_v1 = iv_name_view ).
      ENDIF.
    ELSE.
      rs_return = zcl_al30_data=>fill_return( iv_type = 'E' iv_number = '008' iv_message_v1 = iv_name_view ).
    ENDIF.

  ENDMETHOD.


  METHOD get_fields_view.
    DATA lt_dd03p TYPE TABLE OF dd03p.
    DATA ls_fields TYPE LINE OF zal30_i_fields.
    DATA ls_fields_text TYPE LINE OF zal30_i_fieldst.

    FIELD-SYMBOLS <ls_dd03p> TYPE dd03p.

    CLEAR: es_dd02v, et_fields, et_fields_text.

* Leo la informaciÃ³n de los campos
    TRY.
        CALL METHOD get_view_ddic
          EXPORTING
            iv_name_view = iv_name_view
            iv_langu     = sy-langu
          IMPORTING
            es_dd02v     = es_dd02v
            et_dd03p     = lt_dd03p[].

      CATCH zcx_al30 .

        RAISE EXCEPTION TYPE zcx_al30
          EXPORTING
            textid = zcx_al30=>view_dont_exist.
    ENDTRY.

* Y los adapto al formato de las tablas de la aplicaicon.

* Leo los campos y los voy pasando a una tabla interna que despuÃ©s se
* pasarÃ¡ a la tabla de diccionario
    LOOP AT lt_dd03p ASSIGNING <ls_dd03p>.
      MOVE-CORRESPONDING <ls_dd03p> TO ls_fields.
      MOVE-CORRESPONDING <ls_dd03p> TO ls_fields_text.

* El campo de posiciÃ³n es importante para saber el orden posterior de los campos. Ya que sin este
* campo la lectura de los campos se hace de manera desordenada y la lectura de datos no son correctas
      ls_fields-pos_ddic = <ls_dd03p>-position.

* Me guardo si el campo es clave, ya que lo usarÃ© para evitar que los campos sean editables.
      ls_fields-key_ddic = <ls_dd03p>-keyflag.


* El campo mandante lo pongo por defecto como campo "tÃ©cnico"
      IF <ls_dd03p>-rollname = 'MANDT'.
        ls_fields-tech = abap_true.
      ENDIF.
      APPEND ls_fields TO et_fields.
      CLEAR ls_fields.

      ls_fields_text-spras = sy-langu.
      APPEND ls_fields_text TO et_fields_text.
      CLEAR ls_fields_text.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_view_ddic.
    DATA ld_name TYPE ddobjname.

    CLEAR: es_dd02v, et_dd03p.

    ld_name = iv_name_view.

* Leo los campos de la tabla
    CALL FUNCTION 'DDIF_TABL_GET'
      EXPORTING
        name          = ld_name
        state         = 'A' " Active version
        langu         = iv_langu
      IMPORTING
        dd02v_wa      = es_dd02v
      TABLES
        dd03p_tab     = et_dd03p[]
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.

    IF sy-subrc NE 0 OR et_dd03p[] IS INITIAL.

      RAISE EXCEPTION TYPE zcx_al30
        EXPORTING
          textid = zcx_al30=>view_dont_exist.
    ENDIF.

  ENDMETHOD.


  METHOD insert_view.
    DATA ls_view TYPE zal30_t_view.
    DATA lt_fields TYPE zal30_i_fields.
    DATA lt_fields_text TYPE zal30_i_fieldst.

* Leo los campos de la vista en formato listo para ser grabado.
    TRY.
        CALL METHOD get_fields_view
          EXPORTING
            iv_name_view   = iv_name_view
          IMPORTING
            et_fields      = lt_fields
            et_fields_text = lt_fields_text.

      CATCH zcx_al30 .
        es_return = zcl_al30_data=>fill_return( iv_type = 'E' iv_number = '020' iv_message_v1 = iv_name_view ).
        EXIT.
    ENDTRY.


* Pongo el nombre de la vista a la estructura principal de la vista
    ls_view-tabname = iv_name_view.

* Grabo los datos de la vista
    TRY.

        save_view_ddic( is_view = ls_view
                                   it_fields = lt_fields
                                   it_fields_text = lt_fields_text ).

        es_return = zcl_al30_data=>fill_return( iv_type = 'S' iv_number = '007' iv_message_v1 = iv_name_view ).

      CATCH zcx_al30 .
        es_return = zcl_al30_data=>fill_return( iv_type = 'S' iv_number = '006' iv_message_v1 = iv_name_view ).
    ENDTRY.

  ENDMETHOD.


  METHOD join_fields_alv.

    DATA ls_fields_view TYPE LINE OF zal30_i_fields_view.
    FIELD-SYMBOLS <ls_fields> TYPE zal30_t_fields.
    FIELD-SYMBOLS <ls_fields_text> TYPE zal30_t_fieldst.

    CLEAR et_fields_view.

    LOOP AT it_fields ASSIGNING <ls_fields>.
      MOVE-CORRESPONDING <ls_fields> TO ls_fields_view.

* Leo los textos y si hay para el campo los paso a la estructura global
      READ TABLE it_fields_text ASSIGNING <ls_fields_text> WITH KEY fieldname = <ls_fields>-fieldname.
      IF sy-subrc = 0.
        MOVE-CORRESPONDING <ls_fields_text> TO ls_fields_view.
      ENDIF.


      APPEND ls_fields_view TO et_fields_view.
      CLEAR ls_fields_view.
    ENDLOOP.

  ENDMETHOD.


  METHOD read_text_of_view.

    DATA ld_name TYPE ddobjname.
    DATA ls_dd02v TYPE dd02v.
    DATA lt_dd03p TYPE TABLE OF dd03p.
    DATA ls_fields_text TYPE zal30_t_fieldst.
    FIELD-SYMBOLS <ls_dd03p> TYPE dd03p.

    CLEAR ev_text_fields.

    TRY.
        CALL METHOD get_view_ddic
          EXPORTING
            iv_name_view = iv_name_view
            iv_langu     = iv_spras
          IMPORTING
            et_dd03p     = lt_dd03p[].
      CATCH zcx_al30 .
        EXIT.
    ENDTRY.


* Leo los campos y los voy pasando a una tabla interna que despuÃ©s se
* pasarÃ¡ a la tabla de diccionario
    LOOP AT lt_dd03p ASSIGNING <ls_dd03p>.
      MOVE-CORRESPONDING <ls_dd03p> TO ls_fields_text.

      ls_fields_text-spras = iv_spras.
      APPEND ls_fields_text TO ev_text_fields.
      CLEAR ls_fields_text.

    ENDLOOP.

  ENDMETHOD.


  METHOD read_view.
    DATA lt_fields_text TYPE TABLE OF zal30_t_fieldst.
    DATA lt_fields TYPE TABLE OF zal30_t_fields.
    DATA ls_return TYPE bapiret2.

    CLEAR: es_return, es_view, et_fields_view.

* Primero busco si existe la apliacion
    SELECT SINGLE * INTO es_view
           FROM zal30_t_view
           WHERE tabname = iv_name_view.
    IF sy-subrc = 0.

* Busco el texto de la vista
      SELECT SINGLE ddtext INTO ev_text_view
             FROM dd02t
             WHERE tabname = iv_name_view
                   AND ddlanguage = sy-langu.

* Busco la informacion de los campos
      SELECT * INTO TABLE lt_fields
             FROM zal30_t_fields
             WHERE tabname = iv_name_view.

      SORT lt_fields BY pos_ddic.

* Ahora los textos de los campos
      SELECT * INTO TABLE lt_fields_text
                 FROM zal30_t_fieldst
                 WHERE tabname = iv_name_view
                       AND spras = sy-langu.
      IF sy-subrc NE 0.
* Si no hay textos en el idioma filtrado recupero para ese idioma los textos de la vista
        read_text_of_view( EXPORTING iv_name_view = iv_name_view
                           IMPORTING ev_text_fields = lt_fields_text ).

* Si hay textos los grabo en la tabla para la siguiente vez no haya que recuperarlos.
        IF lt_fields_text IS NOT INITIAL.
          es_return = save_text_of_view( lt_fields_text ).
        ENDIF.
      ENDIF.

* Paso las dos tablas a la tabla de ALV.
      et_fields_view = join_fields_alv( it_fields = lt_fields
                                       it_fields_text = lt_fields_text ).

    ELSE.
      es_return = zcl_al30_data=>fill_return( iv_type = 'E' iv_number = '001' iv_message_v1 = iv_name_view ).
    ENDIF.
  ENDMETHOD.


  METHOD save_text_of_view.
    FIELD-SYMBOLS <ls_fields_text> TYPE zal30_t_fieldst.
    CLEAR rs_return.

    IF iv_text_fields[] IS NOT INITIAL.

      INSERT zal30_t_fieldst FROM TABLE iv_text_fields.
      IF sy-subrc = 0.
        COMMIT WORK.
      ELSE.
        READ TABLE iv_text_fields ASSIGNING <ls_fields_text> INDEX 1.
        rs_return = zcl_al30_data=>fill_return( iv_type = 'E' iv_number = '013' iv_message_v1 = <ls_fields_text>-tabname ).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD save_view.
    DATA lt_fields_text TYPE TABLE OF zal30_t_fieldst.
    DATA lt_fields TYPE TABLE OF zal30_t_fields.

* Primero separo los datos de los campos en las dos tabla de datos
    split_fields_alv( EXPORTING it_fields_view = it_fields_view
                      IMPORTING et_fields = lt_fields
                                et_fields_text = lt_fields_text ).

* Grabo los datos de la vista
    TRY.
        save_view_ddic( is_view = is_view
                                   it_fields = lt_fields
                                   it_fields_text = lt_fields_text ).

        rs_return = zcl_al30_data=>fill_return( iv_type = 'S' iv_number = '007' iv_message_v1 = is_view-tabname ).
      CATCH zcx_al30 .
        rs_return = zcl_al30_data=>fill_return( iv_type = 'S' iv_number = '006' iv_message_v1 = is_view-tabname ).
    ENDTRY.

  ENDMETHOD.


  METHOD save_view_ddic.

* AÃ±ado los campos
    MODIFY zal30_t_fields FROM TABLE it_fields.
    IF sy-subrc = 0.

* Si no hay error inserto los textos
      MODIFY zal30_t_fieldst FROM TABLE it_fields_text.
      IF sy-subrc = 0.

        MODIFY zal30_t_view FROM is_view.
        IF sy-subrc = 0.
          COMMIT WORK.
        ELSE.
          ROLLBACK WORK.
          RAISE EXCEPTION TYPE zcx_al30
            EXPORTING
              textid = zcx_al30=>error_save_view.
        ENDIF.

      ELSE.
        ROLLBACK WORK.
        RAISE EXCEPTION TYPE zcx_al30
          EXPORTING
            textid = zcx_al30=>error_save_view.
      ENDIF.

    ELSE.
      RAISE EXCEPTION TYPE zcx_al30
        EXPORTING
          textid = zcx_al30=>error_save_view.
    ENDIF.


  ENDMETHOD.


  METHOD split_fields_alv.
    DATA ls_fields TYPE zal30_t_fields.
    DATA ls_fields_text TYPE zal30_t_fieldst.
    FIELD-SYMBOLS <ls_fields_view> TYPE LINE OF zal30_i_fields_view.

    CLEAR: et_fields, et_fields_text.

    LOOP AT it_fields_view ASSIGNING <ls_fields_view>.
      MOVE-CORRESPONDING <ls_fields_view> TO ls_fields.
      MOVE-CORRESPONDING <ls_fields_view> TO ls_fields_text.
      APPEND ls_fields TO et_fields.
      APPEND ls_fields_text TO et_fields_text.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
