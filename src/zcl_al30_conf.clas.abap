"! <p class="shorttext synchronized">AL30 - View configuration</p>
CLASS zcl_al30_conf DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPE-POOLS abap .

    TYPES:
*"* public components of class ZCL_AL30_CONF
*"* do not include other source files here!!!
      tt_info_data TYPE STANDARD TABLE OF dfies WITH EMPTY KEY .

    "! <p class="shorttext synchronized">Read view</p>
    METHODS read_view
      IMPORTING
        !iv_name_view         TYPE tabname
        !iv_all_language      TYPE sap_bool DEFAULT abap_false
        !iv_langu             TYPE sylangu DEFAULT sy-langu
        !iv_read_ddic         TYPE sap_bool DEFAULT abap_true
      EXPORTING
        !ev_text_view         TYPE as4text
        !es_return            TYPE bapiret2
        !es_view              TYPE zal30_t_view
        !et_fields            TYPE zif_al30_data=>tt_fields_view
        !et_fields_text       TYPE zif_al30_data=>tt_fields_text_view
        !et_fields_ddic       TYPE dd03ptab
        !et_foreign_key_ddic TYPE dd05mttyp .
    "! <p class="shorttext synchronized">Checks if view is correct in DDIC</p>
    METHODS check_view_ddic
      IMPORTING
        !iv_name_view TYPE tabname
      EXPORTING
        !e_return     TYPE bapiret2
        !e_text_view  TYPE as4text .
    "! <p class="shorttext synchronized">Create a view</p>
    METHODS insert_view
      IMPORTING
        !iv_name_view          TYPE tabname
        !iv_use_default_values TYPE sap_bool DEFAULT abap_false
        !is_default_values     TYPE zif_al30_data=>ts_default_values_create OPTIONAL
      EXPORTING
        !es_return             TYPE bapiret2
        !et_fields             TYPE zif_al30_data=>tt_fields_view
        !et_fields_text        TYPE zif_al30_data=>tt_fields_text_view
        !es_view               TYPE zal30_t_view
        !et_fields_ddic        TYPE dd03ptab .
    "! <p class="shorttext synchronized">Delete a view</p>
    METHODS delete_view
      IMPORTING
        !iv_name_view    TYPE tabname
      RETURNING
        VALUE(rs_return) TYPE bapiret2 .
    "! <p class="shorttext synchronized">Save view</p>
    METHODS save_view
      IMPORTING
        !is_view         TYPE zal30_t_view
        !it_fields       TYPE zif_al30_data=>tt_fields_view
        !it_fields_text  TYPE zif_al30_data=>tt_fields_text_view OPTIONAL
      RETURNING
        VALUE(rs_return) TYPE bapiret2 .
    "! <p class="shorttext synchronized">Check view for insert</p>
    METHODS check_view_insert
      IMPORTING
        !iv_name_view TYPE tabname
      EXPORTING
        !es_return    TYPE bapiret2
        !ev_text_view TYPE as4text .
    "! <p class="shorttext synchronized">Check view when is reading</p>
    METHODS check_view_read
      IMPORTING
        !iv_name_view TYPE tabname
      EXPORTING
        !es_return    TYPE bapiret2
        !ev_text_view TYPE as4text .
    "! <p class="shorttext synchronized">Check if there are changes between the view and the dictiona</p>
    METHODS check_changes_dict
      IMPORTING
        !it_fields      TYPE zif_al30_data=>tt_fields_view OPTIONAL
        !it_fields_text TYPE zif_al30_data=>tt_fields_text_view OPTIONAL
        !is_view        TYPE zal30_t_view
        !iv_langu       TYPE sylangu DEFAULT sy-langu
      EXPORTING
        !ev_diff_fields TYPE sap_bool
        !ev_diff_text   TYPE sap_bool .
    "! <p class="shorttext synchronized">Adjusts the view with the data dictionary.</p>
    METHODS adjust_view_dictionary
      IMPORTING
        !iv_keep_text        TYPE sap_bool DEFAULT abap_true
        !iv_langu            TYPE sylangu DEFAULT sy-langu
      EXPORTING
        VALUE(es_return)     TYPE bapiret2
        !ev_text_view        TYPE as4text
      CHANGING
        !ct_fields_view      TYPE zif_al30_data=>tt_fields_view
        !ct_fields_text_view TYPE zif_al30_data=>tt_fields_text_view
        !cs_view             TYPE zal30_t_view .
    "! <p class="shorttext synchronized">Check exit class</p>
    METHODS check_exit_class
      IMPORTING
        !iv_exit_class   TYPE zal30_e_exit_class
      RETURNING
        VALUE(rs_return) TYPE bapiret2 .
    "! <p class="shorttext synchronized">Get logon languages</p>
    METHODS get_logon_languages
      IMPORTING
        !iv_langu  TYPE sylangu DEFAULT sy-langu
      EXPORTING
        !et_lang   TYPE zif_al30_data=>tt_logon_lang
        !et_r_lang TYPE zif_al30_data=>tt_r_lang .
    "! <p class="shorttext synchronized">Get fields of data dictionary</p>
    METHODS get_fields_view_ddic
      IMPORTING
        !iv_name_view         TYPE any
        !iv_langu             TYPE sylangu DEFAULT sy-langu
        !iv_add_texttable     TYPE sap_bool DEFAULT abap_false
        !iv_all_language      TYPE sap_bool DEFAULT abap_false
      EXPORTING
        !es_dd02v             TYPE dd02v
        !et_fields            TYPE zif_al30_data=>tt_fields_view
        !et_fields_text       TYPE zif_al30_data=>tt_fields_text_view
        !ev_texttable         TYPE tabname
        !et_fields_ddic       TYPE dd03ptab
        !et_foreign_key_ddic TYPE dd05mttyp
      RAISING
        zcx_al30 .
    "! <p class="shorttext synchronized">Transport View</p>
    METHODS transport_view
      IMPORTING
        !is_view             TYPE zal30_t_view
        !it_fields_view      TYPE zif_al30_data=>tt_fields_view
        !it_fields_text_view TYPE zif_al30_data=>tt_fields_text_view OPTIONAL
      EXPORTING
        !es_return           TYPE bapiret2
      CHANGING
        !cv_order            TYPE e070-trkorr .
    "! <p class="shorttext synchronized">Read single view ddic</p>
    METHODS read_single_view_ddic
      IMPORTING
        !iv_name_view TYPE any
        !iv_langu     TYPE sylangu DEFAULT sy-langu
      EXPORTING
        !es_dd02v     TYPE dd02v
        !et_dd03p     TYPE dd03ptab
        !et_dd05m     TYPE dd05mttyp
      RAISING
        zcx_al30 .
    "! <p class="shorttext synchronized">Read info of a data element</p>
    "!
    "! @parameter iv_rollname | <p class="shorttext synchronized">Data element</p>
    "! @parameter es_info | <p class="shorttext synchronized">info</p>
    METHODS read_single_data_element
      IMPORTING
        !iv_rollname TYPE rollname
        !iv_langu    TYPE sylangu DEFAULT sy-langu
      EXPORTING
        !es_info     TYPE dfies
      RAISING
        zcx_al30 .
    "! <p class="shorttext synchronized">Read info in all languages</p>
    "!
    "! @parameter iv_rollname | <p class="shorttext synchronized">Data element</p>
    "! @parameter et_info | <p class="shorttext synchronized">Information in each language</p>
    METHODS read_data_element_all_lang
      IMPORTING
        !iv_rollname TYPE rollname
      EXPORTING
        !et_info     TYPE tt_info_data
      RAISING
        zcx_al30 .
  PROTECTED SECTION.

*"* protected components of class ZCL_AL30_CONF
*"* do not include other source files here!!!
    DATA mt_logon_lang TYPE zif_al30_data=>tt_logon_lang .
    DATA mt_fields TYPE zif_al30_data=>tt_fields_view .
    DATA mt_fields_text TYPE zif_al30_data=>tt_fields_text_view .
    DATA mt_fields_ddic TYPE dd03ptab .
    DATA mt_foreign_key_ddic TYPE dd05mttyp.
    DATA ms_view TYPE zal30_t_view .

    "! <p class="shorttext synchronized">Save the data in ddic format</p>
    METHODS save_view_ddic
      IMPORTING
        !is_view        TYPE zal30_t_view
        !it_fields      TYPE zif_al30_data=>tt_fields_view
        !it_fields_text TYPE zif_al30_data=>tt_fields_text_view OPTIONAL
      RAISING
        zcx_al30 .
    "! <p class="shorttext synchronized">Read view in all languages</p>
    METHODS read_view_ddic_all_lang
      IMPORTING
        !iv_name_view TYPE any
        !iv_langu     TYPE sylangu DEFAULT sy-langu
      EXPORTING
        !es_dd02v     TYPE dd02v
        !et_dd03p     TYPE dd03ptab
        !et_dd05m     TYPE dd05mttyp .
    "! <p class="shorttext synchronized">Read text view</p>
    METHODS read_text_view
      IMPORTING
        !iv_name_view    TYPE tabname
        !iv_all_language TYPE sap_bool DEFAULT abap_false
        !iv_langu        TYPE sylangu DEFAULT sy-langu
      EXPORTING
        !et_fields_text  TYPE zif_al30_data=>tt_fields_text_view .
    "! <p class="shorttext synchronized">Get text table of view</p>
    METHODS get_texttable_view
      IMPORTING
        !iv_name_view    TYPE any
        !iv_langu        TYPE sylangu DEFAULT sy-langu
        !iv_all_language TYPE sap_bool DEFAULT abap_false
      EXPORTING
        !ev_texttable    TYPE tabname
        !et_fields       TYPE zif_al30_data=>tt_fields_view
        !et_fields_text  TYPE zif_al30_data=>tt_fields_text_view
        !et_fields_ddic  TYPE dd03ptab .
    "! <p class="shorttext synchronized">Adjust fields of view</p>
    METHODS adjust_fields_view
      IMPORTING
        !iv_all_language TYPE sap_bool DEFAULT abap_false
        !iv_langu        TYPE sylangu DEFAULT sy-langu
        !it_fields_ddic  TYPE dd03ptab
      CHANGING
        !ct_fields       TYPE zif_al30_data=>tt_fields_view
        !ct_fields_text  TYPE zif_al30_data=>tt_fields_text_view .
    "! <p class="shorttext synchronized">Fill default values of fields</p>
    METHODS fill_default_values_fields
      CHANGING
        !cs_fields TYPE zif_al30_data=>ts_fields_view .
    "! <p class="shorttext synchronized">Fill default values of view</p>
    METHODS fill_default_values_view
      IMPORTING
        !is_default_values     TYPE zif_al30_data=>ts_default_values_create OPTIONAL
        !iv_use_default_values TYPE sap_bool DEFAULT abap_false
      CHANGING
        !cs_view               TYPE zal30_t_view .
    "! <p class="shorttext synchronized">Is fields a checkbox?</p>
    METHODS is_field_checkbox
      IMPORTING
        !iv_rollname     TYPE domname
      RETURNING
        VALUE(rv_result) TYPE zal30_s_fields_attr_general-checkbox .
  PRIVATE SECTION.

*"* private components of class ZCL_AL30_CONF
*"* do not include other source files here!!!
ENDCLASS.



CLASS zcl_al30_conf IMPLEMENTATION.


  METHOD adjust_fields_view.
    DATA ls_fields_text TYPE LINE OF zif_al30_data=>tt_fields_text_view.

    LOOP AT ct_fields ASSIGNING FIELD-SYMBOL(<ls_fields>).

      " Si el texto se introduce manual solo tendré que actualizar la posición donde debe salir, que será la misma que la del campo
      CASE <ls_fields>-source_text.
        WHEN zif_al30_data=>cs_source_text-manual.
          LOOP AT ct_fields_text ASSIGNING FIELD-SYMBOL(<ls_fields_text>) WHERE fieldname = <ls_fields>-fieldname.
            <ls_fields_text>-pos_ddic = <ls_fields>-pos_ddic.
          ENDLOOP.

          " Si viene del diccionario se añade el texto que proviene del mismo
        WHEN zif_al30_data=>cs_source_text-dictionary.

          " Si el campo es virtual no vendrá del diccionario por lo tanto hay llamar a otros método para recuperar sus textos
          IF <ls_fields>-virtual = abap_true.
            TRY.
                " La búsqueda del texto del elemento de datos también depende si se quiere para todos los idiomas o solo
                " para el idioma pasado por parámetro. El primero se usa en la configuración de la vista, el segundo cuando se
                " mantienee los datos.
                IF iv_all_language = abap_true.
                  read_data_element_all_lang( EXPORTING iv_rollname = <ls_fields>-virtual_dtel
                                              IMPORTING et_info     = DATA(lt_info) ).
                ELSE.
                  read_single_data_element( EXPORTING iv_rollname = <ls_fields>-virtual_dtel
                                                      iv_langu = iv_langu
                                            IMPORTING es_info = DATA(ls_info) ).
                  INSERT ls_info INTO TABLE lt_info.
                ENDIF.
                LOOP AT lt_info ASSIGNING FIELD-SYMBOL(<ls_info>).
                  ls_fields_text = CORRESPONDING #( <ls_info> ).
                  ls_fields_text = CORRESPONDING #( BASE ( ls_fields_text ) <ls_fields> ).
                  ls_fields_text-spras = <ls_info>-langu.
                  INSERT ls_fields_text INTO TABLE ct_fields_text.
                  CLEAR ls_fields_text.
                ENDLOOP.
                CLEAR lt_info.                     .
              CATCH zcx_al30.
            ENDTRY.
          ENDIF.

          LOOP AT it_fields_ddic ASSIGNING FIELD-SYMBOL(<ls_fields_ddic>) WHERE fieldname = <ls_fields>-fieldname.

            MOVE-CORRESPONDING <ls_fields_ddic> TO ls_fields_text.
            ls_fields_text-spras = <ls_fields_ddic>-ddlanguage.
            ls_fields_text-pos_ddic = <ls_fields>-pos_ddic.
            INSERT ls_fields_text INTO TABLE ct_fields_text.
            CLEAR ls_fields_text.

          ENDLOOP.
      ENDCASE.
    ENDLOOP.

    " Obtenemos la última posición usada
    DATA(lv_position) = REDUCE #( INIT x = 0 FOR <wa> IN ct_fields NEXT x = COND #( WHEN <wa>-pos_ddic > x THEN <wa>-pos_ddic ELSE x ) ).
    lv_position = lv_position + 1.

    " Se ajusta aquellos campos que no tienen posición.
    LOOP AT ct_fields ASSIGNING <ls_fields> WHERE pos_ddic IS INITIAL.
      <ls_fields>-pos_ddic = lv_position.
      lv_position = lv_position + 1.
    ENDLOOP.


  ENDMETHOD.


  METHOD adjust_view_dictionary.
    DATA lt_fields_old TYPE zif_al30_data=>tt_fields_view.
    DATA lt_fields_text_old TYPE zif_al30_data=>tt_fields_text_view.


    CLEAR: es_return, ev_text_view.

* Si los campos a nivel de parámetro están informados tienen prioridad sobre los globales
    IF ct_fields_view IS NOT INITIAL.
      DATA(lt_fields) = ct_fields_view.
    ELSE.
      lt_fields = mt_fields.
    ENDIF.

    IF ct_fields_text_view IS NOT INITIAL.
      DATA(lt_fields_text) = ct_fields_text_view.
    ELSE.
      lt_fields_text = mt_fields_text.
    ENDIF.

    IF cs_view IS INITIAL.
      cs_view = ms_view.
    ENDIF.

* Se pasa los parámetros de entrada a tablas auxiliares
    lt_fields_old = lt_fields.
    lt_fields_text_old = lt_fields_text.

* Limpio los parámetros changing.
    CLEAR: ct_fields_view, ct_fields_text_view.

* Leo los campos de la vista en formato listo para ser tratado y los pongo en los parámetros changing.
    TRY.
        CALL METHOD get_fields_view_ddic
          EXPORTING
            iv_name_view     = cs_view-tabname
            iv_add_texttable = abap_true
          IMPORTING
            es_dd02v         = DATA(ls_dd02v)
            et_fields        = ct_fields_view
            et_fields_text   = ct_fields_text_view
            ev_texttable     = cs_view-texttable.

      CATCH zcx_al30 .
        es_return = zcl_al30_util=>fill_return( iv_type = 'E' iv_number = '013' iv_message_v1 = cs_view-tabname ).
        EXIT.
    ENDTRY.

* Recupero el texto
    ev_text_view = ls_dd02v-ddtext.
*
    LOOP AT ct_fields_view ASSIGNING FIELD-SYMBOL(<ls_fields>).

      " Si el campo existe en la tabla que toca (puede ser que se pueden mover campos entre la tabla principal y la de textos) paso los valores que pudiera tener.
      READ TABLE lt_fields_old ASSIGNING FIELD-SYMBOL(<ls_fields_old>) WITH KEY fieldname = <ls_fields>-fieldname
                                                                                field_texttable = <ls_fields>-field_texttable.
      IF sy-subrc = 0.
        <ls_fields> = <ls_fields_old>.
      ELSE.
        " El nuevo campo se le rellena con sus valores por defecto
        fill_default_values_fields( CHANGING cs_fields = <ls_fields> ).
      ENDIF.

      " Si el campo tiene como origen del texto manual y se quiere mantener el texto entonces voy moviendo los textos antiguos al nuevo
      IF <ls_fields>-source_text = zif_al30_data=>cs_source_text-manual AND iv_keep_text = abap_true.
        LOOP AT ct_fields_text_view ASSIGNING FIELD-SYMBOL(<ls_fields_text>) WHERE fieldname = <ls_fields>-fieldname.
          READ TABLE lt_fields_text_old ASSIGNING FIELD-SYMBOL(<ls_fields_text_old>)
                                        WITH KEY fieldname = <ls_fields>-fieldname
                                                 spras = <ls_fields_text>-spras.
          IF sy-subrc = 0.
            <ls_fields_text> = <ls_fields_text_old>.
          ENDIF.

        ENDLOOP.
      ENDIF.

    ENDLOOP.

    " Se añaden los campos virtuales que había en la tabla antigua
    LOOP AT lt_fields_old ASSIGNING <ls_fields_old> WHERE virtual = abap_true.
      INSERT <ls_fields_old> INTO TABLE ct_fields_view.
      LOOP AT lt_fields_text_old ASSIGNING <ls_fields_text_old> WHERE fieldname = <ls_fields_old>-fieldname.
        INSERT <ls_fields_text_old> INTO TABLE ct_fields_text_view.
      ENDLOOP.
    ENDLOOP.

* Finalmente grabo la vista
    es_return = save_view( EXPORTING is_view             = cs_view
                                     it_fields      =  ct_fields_view
                                     it_fields_text =  ct_fields_text_view ).

* Se pasan los nuevos campos ajustados a las variables globales
    mt_fields = ct_fields_view.
    mt_fields_text = ct_fields_text_view.
    ms_view = cs_view.

  ENDMETHOD.


  METHOD check_changes_dict.
    FIELD-SYMBOLS <ls_dd03p> TYPE dd03p.

    DATA ld_name TYPE ddobjname.
    DATA ls_dd02v TYPE dd02v.
    DATA lt_dd03p TYPE TABLE OF dd03p.
    DATA ld_num_fields TYPE i.
    DATA ld_num_fields_ddic TYPE i.
    DATA lv_texttable_ddic TYPE tabname.

    ev_diff_fields = ev_diff_text = abap_false.

* Si los campos a nivel de parámetro están informados tienen prioridad sobre los globales
    IF it_fields IS NOT INITIAL.
      DATA(lt_fields) = it_fields.
    ELSE.
      lt_fields = mt_fields.
    ENDIF.

    IF it_fields_text IS NOT INITIAL.
      DATA(lt_fields_text) = it_fields_text.
    ELSE.
      lt_fields_text = mt_fields_text.
    ENDIF.


* Obtengo informaciÃ³n de la tabla
    TRY.
        CALL METHOD get_fields_view_ddic
          EXPORTING
            iv_name_view     = is_view-tabname
            iv_add_texttable = abap_true
          IMPORTING
            et_fields        = DATA(lt_fields_ddic)
            et_fields_text   = DATA(lt_fields_text_ddic)
            ev_texttable     = lv_texttable_ddic.
      CATCH zcx_al30.
    ENDTRY.

    IF lt_fields_ddic[] IS NOT INITIAL.

* Se mira campo por campo las posibles diferencias, los virtuales no se procesan porque nunca existen en el diccionario.
      LOOP AT lt_fields ASSIGNING FIELD-SYMBOL(<ls_fields>) WHERE virtual = abap_false.

        " Se mira que el campo existe en la tabla que le pertoca. Si un campo esta en la principal y pasa a la de texto, o la inversa,
        " se tiene que detectar porque afectaría a la actualización.
        READ TABLE lt_fields_ddic TRANSPORTING NO FIELDS WITH KEY fieldname = <ls_fields>-fieldname
                                                                  field_texttable = <ls_fields>-field_texttable.
        IF sy-subrc = 0.
* Nota IRB 04/06/2019: No tiene sentido validar el texto porque si se introduce manualmente da igual el que pueda tener
* en el diccionario. Y si se escoge el diccionario siempre tomará el texto del mismo. Aún asi, la dejo la validación por
* si hay que implementar dicha casuística.
*            " La validación de texto solo se efectua cuando el origen del texto del campo será manual.
*            IF <ls_fields>-source_text = zif_al30_data=>cs_source_text-manual.
*              LOOP AT it_fields_text ASSIGNING FIELD-SYMBOL(<ls_fields_text>).
*
*                READ TABLE lt_fields_text_ddic ASSIGNING FIELD-SYMBOL(<ls_fields_text_ddic>)
*                                               WITH KEY fieldname = <ls_fields_text>-fieldname
*                                                        spras = <ls_fields_text>-spras.
*                IF sy-subrc = 0.
** Si el existe el mismo campo miro los textos
*                  IF <ls_fields_text>-scrtext_s NE <ls_dd03p>-scrtext_s
*                     OR <ls_fields_text>-scrtext_m NE <ls_dd03p>-scrtext_m
*                     OR <ls_fields_text>-scrtext_l NE <ls_dd03p>-scrtext_l
*                     OR <ls_fields_text>-reptext  NE <ls_dd03p>-reptext.
*                    ev_diff_text = abap_true.
*                    EXIT.
*                  ENDIF.
*
*                ELSE.
** A la primera diferencia salgo, no tiene sentido leer el resto de campos.
*                  ev_diff_fields = abap_true.
*                  EXIT.
*                ENDIF.
*              ENDLOOP.
*            ENDIF.
        ELSE.
          " A la primera diferencia salgo, no tiene sentido leer el resto de campos.
          ev_diff_fields = abap_true.
          EXIT.
        ENDIF.
      ENDLOOP.

      " Si no hay diferencias se busca a la inversa, que haya campos nuevos en el diccionario que no esten en la configuración
      IF ev_diff_fields = abap_false.
        LOOP AT lt_fields_ddic ASSIGNING FIELD-SYMBOL(<ls_fields_ddic>).
          READ TABLE lt_fields TRANSPORTING NO FIELDS WITH KEY fieldname = <ls_fields_ddic>-fieldname
                                                                      field_texttable = <ls_fields_ddic>-field_texttable
                                                                      virtual = abap_false.
          IF sy-subrc NE 0.
            " A la primera diferencia salgo, no tiene sentido leer el resto de campos.
            ev_diff_fields = abap_true.
            EXIT.
          ENDIF.
        ENDLOOP.
      ENDIF.

    ELSE.
      ev_diff_fields = abap_true.
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
      rs_return = zcl_al30_util=>fill_return( iv_type = 'E' iv_number = '030' iv_message_v1 = iv_exit_class ).
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
        rs_return = zcl_al30_util=>fill_return( iv_type = 'E' iv_number = '031' iv_message_v1 = zif_al30_data=>cv_intf_exit ).
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
        CALL METHOD read_single_view_ddic
          EXPORTING
            iv_name_view = iv_name_view
          IMPORTING
            es_dd02v     = ls_dd02v
            et_dd03p     = lt_dd03p.
      CATCH zcx_al30 .
        e_return = zcl_al30_util=>fill_return( iv_type = 'E' iv_number = '013' iv_message_v1 = iv_name_view ).
        EXIT.
    ENDTRY.

    e_text_view = ls_dd02v-ddtext. " Texto del objeto

** Verifico que en la tabla no tenga campos de tipo string
*    READ TABLE lt_dd03p TRANSPORTING NO FIELDS WITH KEY datatype = 'STRG'.
*    IF sy-subrc = 0.
*      e_return = zcl_al30_util=>fill_return( iv_type = 'E' iv_number = '002' iv_message_v1 = iv_name_view ).
*
*    ELSE.

* Verifico que la tabla tenga el flag de actualizaciÃ³n permitida
    IF ls_dd02v-mainflag = 'N'. " No permitido.
      e_return = zcl_al30_util=>fill_return( iv_type = 'E' iv_number = '003' iv_message_v1 = iv_name_view ).
    ENDIF.

*    ENDIF.


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
          e_return     = es_return
          e_text_view  = ev_text_view.

    ELSE.
      es_return = zcl_al30_util=>fill_return( iv_type = 'E' iv_number = '009' iv_message_v1 = iv_name_view ).
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
          e_return     = es_return
          e_text_view  = ev_text_view.

    ELSE.
      es_return = zcl_al30_util=>fill_return( iv_type = 'E' iv_number = '001' iv_message_v1 = iv_name_view ).
    ENDIF.

  ENDMETHOD.


  METHOD delete_view.
    CLEAR rs_return.

* Borro los datos principales de la vista
    DELETE FROM zal30_t_view WHERE tabname = iv_name_view.
    IF sy-subrc = 0.
      DELETE FROM zal30_t_fields WHERE tabname = iv_name_view.
      IF sy-subrc = 0.
        " Los textos no se controla si va bien o va mal porque puede que no tenga datos, si todos los campos tienen el campo de origen
        " del texto con el valor diccionario.
        DELETE FROM zal30_t_fieldst WHERE tabname = iv_name_view.

        COMMIT WORK AND WAIT.
        rs_return = zcl_al30_util=>fill_return( iv_type = 'S' iv_number = '008' iv_message_v1 = iv_name_view ).

      ELSE.
        ROLLBACK WORK.
        rs_return = zcl_al30_util=>fill_return( iv_type = 'E' iv_number = '007' iv_message_v1 = iv_name_view ).
      ENDIF.
    ELSE.
      rs_return = zcl_al30_util=>fill_return( iv_type = 'E' iv_number = '006' iv_message_v1 = iv_name_view ).
    ENDIF.

  ENDMETHOD.


  METHOD fill_default_values_fields.
    cs_fields-source_text = zif_al30_data=>cs_source_text-dictionary.

    IF cs_fields-tech = abap_false. " Los campos técnicos no se mostrarán en la pantalla selección
      cs_fields-sel_screen = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD fill_default_values_view.

    " Si el parámetro de valores por defecto esta informado los usaré para informar la estructura de la vista
    IF iv_use_default_values = abap_true.
      cs_view = CORRESPONDING #( is_default_values ).
    ELSE.
      cs_view-auth_user = abap_false. " El acceso a la tabla se validará por usuario
      cs_view-change_log = abap_true. " Se guardará log de cambios en las modificaciones
    ENDIF.
  ENDMETHOD.


  METHOD get_fields_view_ddic.
    DATA ls_fields LIKE LINE OF et_fields.
    DATA ls_fields_text LIKE LINE OF et_fields_text.

    FIELD-SYMBOLS <ls_dd03p> TYPE dd03p.

    CLEAR: es_dd02v, et_fields, et_fields_text, et_fields_ddic.

* Leo la informacion de los campos

    IF iv_all_language = abap_true.
      CALL METHOD read_view_ddic_all_lang
        EXPORTING
          iv_name_view = iv_name_view
          iv_langu     = iv_langu
        IMPORTING
          es_dd02v     = es_dd02v
          et_dd03p     = et_fields_ddic
          et_dd05m = et_foreign_key_ddic.
    ELSE.
      CALL METHOD read_single_view_ddic
        EXPORTING
          iv_name_view = iv_name_view
          iv_langu     = iv_langu
        IMPORTING
          es_dd02v     = es_dd02v
          et_dd03p     = et_fields_ddic
          et_dd05m = et_foreign_key_ddic.
    ENDIF.

    IF et_fields_ddic IS INITIAL.
      RAISE EXCEPTION TYPE zcx_al30
        EXPORTING
          textid = zcx_al30=>view_dont_exist.
    ENDIF.

* Y los adapto al formato de las tablas de la aplicaicon.

* Leo los campos y los voy pasando a una tabla interna que despuÃ©s se
* pasarÃ¡ a la tabla de diccionario
    LOOP AT et_fields_ddic ASSIGNING <ls_dd03p>.

      " Sin lenguaje no se añade a la tabla de textos. Eso ocurre cuando hay idiomas configurados en el sistema pero
      " no se ha cargado su libreria de textos. Sin ese idioma luego se producen errores al grabar.
      IF <ls_dd03p>-ddlanguage IS NOT INITIAL.
        MOVE-CORRESPONDING <ls_dd03p> TO ls_fields_text.
        ls_fields_text-spras = <ls_dd03p>-ddlanguage.
        ls_fields_text-pos_ddic = <ls_dd03p>-position.
        APPEND ls_fields_text TO et_fields_text.
        CLEAR ls_fields_text.
      ENDIF.

      " En la tabla de campos solo aparecerá una vez
      READ TABLE et_fields TRANSPORTING NO FIELDS WITH KEY fieldname = <ls_dd03p>-fieldname.
      IF sy-subrc NE 0.
        MOVE-CORRESPONDING <ls_dd03p> TO ls_fields.
* El campo de posiciÃ³n es importante para saber el orden posterior de los campos. Ya que sin este
* campo la lectura de los campos se hace de manera desordenada y la lectura de datos no son correctas
        ls_fields-pos_ddic = <ls_dd03p>-position.

* Me guardo si el campo es clave, ya que lo usarÃ© para evitar que los campos sean editables.
        ls_fields-key_ddic = <ls_dd03p>-keyflag.


* El campo mandante lo pongo por defecto como campo "tÃ©cnico"
        IF <ls_dd03p>-rollname = 'MANDT'.
          ls_fields-tech = abap_true.
        ENDIF.


        " Si se esta procesando un campo que es un char de una posición y además tiene dominio, miro si ese campo
        " es sensible a ser un checkbox
        IF <ls_dd03p>-datatype = 'CHAR' AND <ls_dd03p>-leng = 1 AND <ls_dd03p>-rollname IS NOT INITIAL.
          ls_fields-checkbox = is_field_checkbox( <ls_dd03p>-rollname ).
        ENDIF.

        APPEND ls_fields TO et_fields.
        CLEAR ls_fields.

      ENDIF.

    ENDLOOP.

    " Se buscará y se incluirán los campos de la tabla de texto asociada a la tabla obtenida
    IF iv_add_texttable = abap_true.
      get_texttable_view(
        EXPORTING
          iv_name_view   = iv_name_view
          iv_langu       = iv_langu
          iv_all_language = iv_all_language
      IMPORTING
        ev_texttable   = ev_texttable
        et_fields      = DATA(lt_textttable_fields)
        et_fields_text = DATA(lt_texttable_fields_text)
        et_fields_ddic = DATA(lt_texttable_ddic) ).

      " Se devuelven todos los campos del diccionario de la tabla de textos
      INSERT LINES OF lt_texttable_ddic INTO TABLE et_fields_ddic.

      " Numero de campos que hay en la tabla principal. Porque el campo de los textos comenzará en la posición + 1.
      DESCRIBE TABLE et_fields LINES DATA(lv_num_fields).
      lv_num_fields = lv_num_fields + 1.

      LOOP AT lt_textttable_fields ASSIGNING FIELD-SYMBOL(<ls_fields>).
        DATA(lv_add_field) = abap_true.

        " Los campos clave de la tabla de textos no se pueden añadir para evitar duplicados, salvo el del idioma que es necesario para
        " validaciones y orden de transporte
        IF <ls_fields>-key_ddic = abap_true.

          READ TABLE lt_texttable_ddic TRANSPORTING NO FIELDS
                                       WITH KEY fieldname = <ls_fields>-fieldname
                                                datatype = 'LANG'.
          IF sy-subrc = 0.
            <ls_fields>-lang_texttable = abap_true. " Se marca que es el campo de idioma
          ELSE.
            lv_add_field = abap_false. " El campo no se añade
            " Se borra el campo de la tabla de textos porque no se tiene que procesar.
            DELETE lt_texttable_fields_text WHERE fieldname = <ls_fields>-fieldname.
          ENDIF.
        ENDIF.

        IF lv_add_field = abap_true.

          <ls_fields>-tabname = iv_name_view. " La tabla será la principal
          <ls_fields>-pos_ddic = lv_num_fields. " Posición nueva
          <ls_fields>-field_texttable = abap_true. " Campo de la tabla de texto
          INSERT <ls_fields> INTO TABLE et_fields.
          lv_num_fields = lv_num_fields + 1.

          " Se añaden los textos
          LOOP AT lt_texttable_fields_text ASSIGNING FIELD-SYMBOL(<ls_fields_text>) WHERE fieldname = <ls_fields>-fieldname.
            <ls_fields_text>-tabname = iv_name_view.
            <ls_fields_text>-pos_ddic = <ls_fields>-pos_ddic.
            INSERT <ls_fields_text> INTO TABLE et_fields_text.
          ENDLOOP.

        ENDIF.


      ENDLOOP.


    ENDIF.

    " Se orden los campos por su posición
    SORT et_fields_text BY pos_ddic.
    SORT et_fields BY pos_ddic.

  ENDMETHOD.


  METHOD get_logon_languages.

    CLEAR et_lang.

    IF mt_logon_lang IS INITIAL.

      SELECT t002t~sprsl t002t~sptxt
        INTO TABLE mt_logon_lang
        FROM     t002t JOIN  t002c ON
                 t002t~sprsl = t002c~spras
        WHERE    t002t~spras = iv_langu
        AND      t002c~lainst = abap_true
        ORDER BY t002t~sptxt.

    ENDIF.

    et_lang = mt_logon_lang.

    et_r_lang = VALUE #( FOR <ls_logon_lang> IN mt_logon_lang ( sign = 'I' option = 'EQ' low = <ls_logon_lang>-lang ) ).

  ENDMETHOD.


  METHOD get_texttable_view.
    DATA lv_tabname TYPE tabname.
    DATA lv_texttable TYPE tabname.

    CLEAR: ev_texttable, et_fields, et_fields_text.

    lv_tabname = iv_name_view.

    CALL FUNCTION 'DDUT_TEXTTABLE_GET'
      EXPORTING
        tabname   = lv_tabname
      IMPORTING
        texttable = lv_texttable.

    ev_texttable = lv_texttable.

    IF ev_texttable IS NOT INITIAL.

      TRY.
          get_fields_view_ddic(
            EXPORTING
              iv_name_view     = ev_texttable
              iv_langu         = iv_langu
              iv_all_language = iv_all_language
            IMPORTING
              et_fields        = et_fields
              et_fields_text   =  et_fields_text
              et_fields_ddic = et_fields_ddic ).


        CATCH zcx_al30.
      ENDTRY.

    ENDIF.

  ENDMETHOD.


  METHOD insert_view.


    CLEAR: et_fields, es_return, et_fields_ddic, et_fields_text.

* En la creación solo se recuperarán los campos, los textos no se recuperán porque por defecto se tomarán
* del diccionario de datos
    TRY.
        CALL METHOD get_fields_view_ddic
          EXPORTING
            iv_name_view     = iv_name_view
            iv_add_texttable = abap_true
            iv_all_language  = abap_true
          IMPORTING
            et_fields        = et_fields
            et_fields_text   = et_fields_text
            et_fields_ddic   = et_fields_ddic
            ev_texttable     = es_view-texttable.

      CATCH zcx_al30 .
        es_return = zcl_al30_util=>fill_return( iv_type = 'E' iv_number = '013' iv_message_v1 = iv_name_view ).
        EXIT.
    ENDTRY.

    " Nota IRB: Es importante que primero se rellen los posibles valores por defecto. El motivo es que
    " la estructura de valores por defecto es la misma que la tabla y el nombre del campo de la vista estará en blanco
    " y se perderá en el camino.
* Valores por defecto en la cabecera
    fill_default_values_view( EXPORTING is_default_values = is_default_values
                                        iv_use_default_values = iv_use_default_values
                              CHANGING cs_view = es_view ).


* Pongo el nombre de la vista a la estructura principal de la vista
    es_view-tabname = iv_name_view.



* Por defecto los campos de la vista su texto se tomará del diccionario
    LOOP AT et_fields ASSIGNING FIELD-SYMBOL(<ls_fields>).
      fill_default_values_fields( CHANGING cs_fields = <ls_fields> ).
    ENDLOOP.

* Finalmente grabo la vista.
    es_return = save_view( EXPORTING is_view             = es_view
                                     it_fields      =  et_fields
                                     it_fields_text = et_fields_text ).

  ENDMETHOD.


  METHOD is_field_checkbox.

    rv_result = abap_false.
    " Se recupera los valores del dominio asociado al alemento de datos
    DATA(lt_values) = CAST cl_abap_elemdescr( cl_abap_typedescr=>describe_by_name( iv_rollname ) )->get_ddic_fixed_values( ).

    IF lines( lt_values ) = 2. " Tiene que tener dos valores para el ' ' y 'X'.
      " Para el false o el no tanto el campo low como el high ha de estar en blanco. Nota, como los campos tienen nombre esopecial para SAP
      " hay que ponerlos de esa manera para que funcione.
      READ TABLE lt_values TRANSPORTING NO FIELDS WITH KEY ('LOW') = space
                                                           ('HIGH') = space.
      IF sy-subrc = 0.
        " El true o si tiene que tener el campo low una X y el high en blanco.
        READ TABLE lt_values TRANSPORTING NO FIELDS WITH KEY ('LOW') = 'X'
                                                                 ('HIGH') = space.
        IF sy-subrc = 0.
          rv_result = abap_true.
        ENDIF.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD read_data_element_all_lang.

    CLEAR et_info.

    get_logon_languages( IMPORTING et_lang = DATA(lt_lang) ).

    LOOP AT lt_lang ASSIGNING FIELD-SYMBOL(<ls_lang>).

      TRY.
          read_single_data_element( EXPORTING iv_rollname = iv_rollname
                                              iv_langu = <ls_lang>-lang
                                    IMPORTING es_info = DATA(ls_info) ).

          INSERT ls_info INTO TABLE et_info.

        CATCH zcx_al30.
      ENDTRY.

    ENDLOOP.

    " Si al final de la búsqueda no existen datos en ningun idioma se lanza la excepción que el
    " elemento de datos no existe
    IF et_info IS INITIAL.
      RAISE EXCEPTION TYPE zcx_al30
        EXPORTING
          textid = zcx_al30=>data_element_not_exist.
    ENDIF.

  ENDMETHOD.


  METHOD read_single_data_element.

    CLEAR: es_info.


    CALL METHOD cl_abap_typedescr=>describe_by_name(
      EXPORTING
        p_name         = iv_rollname
      RECEIVING
        p_descr_ref    = DATA(lo_ref)
      EXCEPTIONS
        type_not_found = 1
        OTHERS         = 2 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_al30
        EXPORTING
          textid = zcx_al30=>data_element_not_exist.
    ELSE.
      DATA(lo_rollname) = CAST cl_abap_elemdescr( lo_ref  ).

      es_info = lo_rollname->get_ddic_field( p_langu = iv_langu ).
      es_info-langu = iv_langu.

    ENDIF.

  ENDMETHOD.


  METHOD read_single_view_ddic.
    DATA lv_name TYPE ddobjname .

    CLEAR: es_dd02v, et_dd03p, et_dd05m.

    lv_name = iv_name_view.

* Leo los campos de la tabla
    CALL FUNCTION 'DDIF_TABL_GET'
      EXPORTING
        name          = lv_name
        state         = 'A' " Active version
        langu         = iv_langu
      IMPORTING
        dd02v_wa      = es_dd02v
      TABLES
        dd03p_tab     = et_dd03p[]
        dd05m_tab     = et_dd05m[]
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.

    IF sy-subrc NE 0 OR et_dd03p IS INITIAL.

      RAISE EXCEPTION TYPE zcx_al30
        EXPORTING
          textid = zcx_al30=>view_dont_exist.

    ELSE.
      " Pongo el campo idioma siempre porque si no esta traducido no se informa. Y es necesario para el resto de procesos.
      LOOP AT et_dd03p ASSIGNING FIELD-SYMBOL(<ls_dd03p>).
        <ls_dd03p>-ddlanguage = iv_langu.
      ENDLOOP.

* Funcion para leer vistas: DD_VIEW_GET
      " Si es una vista la descripción que se recupera no es correcta.
      IF es_dd02v-tabclass = 'VIEW'.
        SELECT SINGLE ddtext INTO es_dd02v-ddtext
               FROM dd25t
               WHERE viewname = lv_name
                     AND ddlanguage = iv_langu.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD read_text_view.

    CLEAR et_fields_text.

    "
    IF iv_all_language = abap_true. " Se obtienen todos los idiomas de logon
      get_logon_languages( IMPORTING et_r_lang = DATA(lt_r_lang) ).
    ELSE.
      INSERT VALUE #( sign = 'I' option = 'EQ' low = iv_langu ) INTO TABLE lt_r_lang.
    ENDIF.

    SELECT * INTO CORRESPONDING FIELDS OF TABLE et_fields_text FROM zal30_t_fieldst WHERE spras IN lt_r_lang
                                                                                          AND tabname = iv_name_view.

  ENDMETHOD.


  METHOD read_view.
    DATA ls_return TYPE bapiret2.

    CLEAR: es_return, es_view, et_fields, et_fields_text, et_fields_ddic.
    CLEAR: mt_fields, ms_view, mt_fields_text, mt_fields_ddic.

* Primero busco si existe la apliacion
    SELECT SINGLE * INTO ms_view
           FROM zal30_t_view
           WHERE tabname = iv_name_view.
    IF sy-subrc = 0.


* Busco la informacion de los campos
      SELECT * INTO CORRESPONDING FIELDS OF TABLE mt_fields
             FROM zal30_t_fields
             WHERE tabname = iv_name_view.


      " Lectura de los textos en todos los idioma de logon
      read_text_view( EXPORTING iv_name_view = iv_name_view
                                iv_all_language = iv_all_language
                                iv_langu = iv_langu
                         IMPORTING et_fields_text = mt_fields_text ).

      " Lectura de los campos del diccionario.
      IF iv_read_ddic = abap_true.

        TRY.
            get_fields_view_ddic(
              EXPORTING
                iv_name_view     = iv_name_view
                iv_langu         = iv_langu
                iv_add_texttable = abap_true
                iv_all_language  = iv_all_language
              IMPORTING
                es_dd02v = DATA(ls_dd02v)
                et_fields_ddic   = mt_fields_ddic
                et_foreign_key_ddic = mt_foreign_key_ddic ).


          CATCH zcx_al30.
        ENDTRY.
      ENDIF.

      ev_text_view = ls_dd02v-ddtext. " Texto de la tabla

      " Se completan los textos en base a los datos del diccionario
      adjust_fields_view( EXPORTING it_fields_ddic = mt_fields_ddic
                          CHANGING ct_fields = mt_fields
                                   ct_fields_text = mt_fields_text  ).


      " Se orden los campos por su posición
      SORT mt_fields_text BY pos_ddic.
      SORT mt_fields BY pos_ddic.

      " Solo se devuelven los campos solicitados
      IF et_fields IS SUPPLIED.
        et_fields = mt_fields.
      ENDIF.
      IF et_fields_text IS SUPPLIED.
        et_fields_text = mt_fields_text.
      ENDIF.
      IF es_view IS SUPPLIED.
        es_view = ms_view.
      ENDIF.
      IF et_fields_ddic IS SUPPLIED.
        et_fields_ddic = mt_fields_ddic.
      ENDIF.
      IF et_foreign_key_ddic IS SUPPLIED.
        et_foreign_key_ddic = mt_foreign_key_ddic.
      ENDIF.


    ELSE.
      es_return = zcl_al30_util=>fill_return( iv_type = 'E' iv_number = '001' iv_message_v1 = iv_name_view ).
    ENDIF.
  ENDMETHOD.


  METHOD read_view_ddic_all_lang.

    CLEAR: es_dd02v, et_dd03p, et_dd05m.

* Se obtiene todos los idioma de logon
    get_logon_languages( IMPORTING et_lang = DATA(lt_lang) ).

    LOOP AT lt_lang ASSIGNING FIELD-SYMBOL(<ls_lang>).

      TRY.
          read_single_view_ddic(
            EXPORTING
              iv_name_view = iv_name_view
              iv_langu     = <ls_lang>-lang
            IMPORTING
              es_dd02v     = DATA(ls_dd02v)
              et_dd03p     = DATA(lt_dd03p)
              et_dd05m = DATA(lt_dd05m) ).

          " Los datos de cabecera pongo el idioma de logon. De esta manera el nombre de la tabla saldrá en el idioma de conexion.
          " Hago lo mismo para las claves externas de la tabla.
          IF <ls_lang>-lang = iv_langu.
            es_dd02v = ls_dd02v.
            et_dd05m = lt_dd05m.
          ENDIF.

          INSERT LINES OF lt_dd03p INTO TABLE et_dd03p.

        CATCH zcx_al30.
      ENDTRY.
    ENDLOOP.

  ENDMETHOD.


  METHOD save_view.
    DATA lt_fields_text TYPE zif_al30_data=>tt_fields_text_view.


* Solo se guardarán los textos de aquellos campos que cuyo origen del texto no sea diccionario
    LOOP AT it_fields ASSIGNING FIELD-SYMBOL(<ls_fields>) WHERE source_text = zif_al30_data=>cs_source_text-manual.
      LOOP AT it_fields_text ASSIGNING FIELD-SYMBOL(<ls_fields_text>) WHERE fieldname = <ls_fields>-fieldname.
        INSERT <ls_fields_text> INTO TABLE lt_fields_text.
      ENDLOOP.
    ENDLOOP..

* Grabo los datos de la vista
    TRY.
        save_view_ddic( is_view = is_view
                                   it_fields = it_fields
                                   it_fields_text = lt_fields_text ).

        rs_return = zcl_al30_util=>fill_return( iv_type = 'S' iv_number = '005' iv_message_v1 = is_view-tabname ).
      CATCH zcx_al30 .
        rs_return = zcl_al30_util=>fill_return( iv_type = 'S' iv_number = '037' iv_message_v1 = is_view-tabname ).
    ENDTRY.

  ENDMETHOD.


  METHOD save_view_ddic.
    DATA lt_fields_ddic TYPE STANDARD TABLE OF zal30_t_fields.
    DATA lt_fieldst_ddic TYPE STANDARD TABLE OF zal30_t_fieldst.

* Se borra los campos previos de la tabla ya sé puede ser que no sean los mismos, ni en cantidad ni en nombre
    DELETE FROM zal30_t_fields WHERE tabname = is_view-tabname.
    DELETE FROM zal30_t_fieldst WHERE tabname = is_view-tabname.

    " Se convierte las tablas de entrada al formato de tabla de diccionario.
    lt_fields_ddic = CORRESPONDING #( it_fields ).
    lt_fieldst_ddic = CORRESPONDING #( it_fields_text ).

* AÃ±ado los campos
    MODIFY zal30_t_fields FROM TABLE lt_fields_ddic.
    IF sy-subrc = 0.

* Los textos solo se informán si la tabla esta rellena. Puede no estarlo porque los textos se tomán del diccionario
      IF lt_fieldst_ddic IS NOT INITIAL.
        MODIFY zal30_t_fieldst FROM TABLE lt_fieldst_ddic.
      ENDIF.
      IF sy-subrc = 0.

        MODIFY zal30_t_view FROM is_view.
        IF sy-subrc = 0.
          COMMIT WORK AND WAIT.
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


  METHOD transport_view.
    DATA lt_view TYPE STANDARD TABLE OF zal30_t_view.
    DATA lt_fields_ddic TYPE STANDARD TABLE OF zal30_t_fields.
    DATA lt_fieldst_ddic TYPE STANDARD TABLE OF zal30_t_fieldst.

    CLEAR es_return.

    " Datos cabecera
    INSERT is_view INTO TABLE lt_view.
    zcl_al30_util=>values_itab_2_transport_order(
      EXPORTING
        it_values  = lt_view
        iv_tabname = 'ZAL30_T_VIEW'
      IMPORTING
        es_return  = es_return
      CHANGING cv_order = cv_order  ).

    IF es_return IS INITIAL OR es_return-type NE zif_al30_data=>cs_msg_type-error.
      " Se convierte las tablas de entrada al formato de tabla de diccionario.
      lt_fields_ddic = CORRESPONDING #( it_fields_view ).

      " Solo se guardan los textos de los campos que se quiere el texto manual
      IF it_fields_text_view IS NOT INITIAL.
        LOOP AT lt_fields_ddic ASSIGNING FIELD-SYMBOL(<ls_fields_ddic>) WHERE source_text = zif_al30_data=>cs_source_text-manual.
          LOOP AT it_fields_text_view ASSIGNING FIELD-SYMBOL(<ls_fields_text_view>) WHERE fieldname = <ls_fields_ddic>-fieldname.
            DATA(ls_fieldst_ddic) = CORRESPONDING zal30_t_fieldst( <ls_fields_text_view> ).
            INSERT ls_fieldst_ddic INTO TABLE lt_fieldst_ddic.
          ENDLOOP.
        ENDLOOP.
      ENDIF.

* Campos de la vista
      zcl_al30_util=>values_itab_2_transport_order(
        EXPORTING
          it_values  = lt_fields_ddic
          iv_tabname = 'ZAL30_T_FIELDS'
        IMPORTING
          es_return  = es_return
        CHANGING cv_order = cv_order ).

      IF es_return IS INITIAL OR es_return-type NE zif_al30_data=>cs_msg_type-error.
        IF lt_fieldst_ddic IS NOT INITIAL.
          zcl_al30_util=>values_itab_2_transport_order(
            EXPORTING
              it_values  = lt_fieldst_ddic
              iv_tabname = 'ZAL30_T_FIELDST'
            IMPORTING
              es_return  = es_return
             CHANGING cv_order = cv_order ).
        ENDIF.

      ENDIF.

    ENDIF.
  ENDMETHOD.
ENDCLASS.
