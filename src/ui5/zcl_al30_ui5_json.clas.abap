class ZCL_AL30_UI5_JSON definition
  public
  inheriting from /UI2/CL_JSON
  create public .

public section.

  class-methods ZSERIALIZE
    importing
      !DATA type DATA
      !COMPRESS type BOOL default C_BOOL-FALSE
      !NAME type STRING optional
      !PRETTY_NAME type PRETTY_NAME_MODE default PRETTY_MODE-NONE
      !TYPE_DESCR type ref to CL_ABAP_TYPEDESCR optional
      !ASSOC_ARRAYS type BOOL default C_BOOL-FALSE
      !TS_AS_ISO8601 type BOOL default C_BOOL-FALSE
      !EXPAND_INCLUDES type BOOL default C_BOOL-TRUE
      !ASSOC_ARRAYS_OPT type BOOL default C_BOOL-FALSE
    returning
      value(R_JSON) type JSON .

  methods SERIALIZE_INT
    redefinition .
protected section.

  methods ZDUMP_INT
  final
    importing
      !DATA type DATA
      !TYPE_DESCR type ref to CL_ABAP_TYPEDESCR optional
    returning
      value(R_JSON) type JSON .
private section.
ENDCLASS.



CLASS ZCL_AL30_UI5_JSON IMPLEMENTATION.


  method SERIALIZE_INT.
  " **********************************************************************
  "! Usage examples and documentation can be found on SCN:
  " http://wiki.scn.sap.com/wiki/display/Snippets/One+more+ABAP+to+JSON+Serializer+and+Deserializer
  " **********************************************************************  "

  DATA: lo_descr   TYPE REF TO cl_abap_typedescr.

  IF type_descr IS INITIAL.
    lo_descr = cl_abap_typedescr=>describe_by_data( data ).
  ELSE.
    lo_descr = type_descr.
  ENDIF.
" INIMOD IRB Change: Se cambia la llamada al método estándar, que esta marcado final, por el nuevo
  r_json = zdump_int( data = data type_descr = lo_descr ).
"  r_json = dump_int( data = data type_descr = lo_descr ).
" ENDMOD IRB

  " we do not do escaping of every single string value for white space characters,
  " but we do it on top, to replace multiple calls by 3 only, while we do not serialize
  " outlined/formatted JSON this shall not produce any harm
  REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf          IN r_json WITH `\r\n`.
  REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline        IN r_json WITH `\n`.
  REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>horizontal_tab IN r_json WITH `\t`.
* REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>form_feed      IN r_json WITH `\f`.
* REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>backspace      IN r_json WITH `\b`.

  IF name IS NOT INITIAL AND ( mv_compress IS INITIAL OR r_json IS NOT INITIAL ).
    CONCATENATE `"` name `":` r_json INTO r_json.
  ENDIF.
  endmethod.


METHOD zdump_int.

  DATA: lo_typedesc   TYPE REF TO cl_abap_typedescr,
        lo_elem_descr TYPE REF TO cl_abap_elemdescr,
        lo_classdesc  TYPE REF TO cl_abap_classdescr,
        lo_structdesc TYPE REF TO cl_abap_structdescr,
        lo_tabledescr TYPE REF TO cl_abap_tabledescr,
        lt_symbols    TYPE t_t_symbol,
        lt_keys       LIKE lt_symbols,
        lt_properties TYPE STANDARD TABLE OF string,
        lt_fields     TYPE STANDARD TABLE OF string,
        lo_obj_ref    TYPE REF TO object,
        lo_data_ref   TYPE REF TO data,
        ls_skip_key   TYPE LINE OF abap_keydescr_tab,
        lv_array_opt  TYPE abap_bool,
        lv_prop_name  TYPE string,
        lv_keyval     TYPE string,
        lv_itemval    TYPE string.

  FIELD-SYMBOLS: <line>   TYPE any,
                 <value>  TYPE any,
                 <data>   TYPE data,
                 <key>    TYPE LINE OF abap_keydescr_tab,
                 <symbol> LIKE LINE OF lt_symbols,
                 <table>  TYPE ANY TABLE.

  " we need here macro instead of method calls because of the performance reasons.
  " based on SAT measurements.

  CASE type_descr->kind.
    WHEN cl_abap_typedescr=>kind_ref.

      IF data IS INITIAL.
        r_json = `null`.                                    "#EC NOTEXT
      ELSEIF type_descr->type_kind EQ cl_abap_typedescr=>typekind_dref.
        lo_data_ref ?= data.
        lo_typedesc = cl_abap_typedescr=>describe_by_data_ref( lo_data_ref ).
        ASSIGN lo_data_ref->* TO <data>.
        " INIMOD IRB
        r_json = zdump_int( data = <data> type_descr = lo_typedesc ).
*        r_json = dump_int( data = <data> type_descr = lo_typedesc ).
        " ENDMOD IRB
      ELSE.
        lo_obj_ref ?= data.
        lo_classdesc ?= cl_abap_typedescr=>describe_by_object_ref( lo_obj_ref ).
        lt_symbols = get_symbols( type_descr = lo_classdesc object = lo_obj_ref ).
        r_json = dump_symbols( lt_symbols ).
      ENDIF.

    WHEN cl_abap_typedescr=>kind_elem.
      lo_elem_descr ?= type_descr.
      "r_json = dump_type( data = data ts_as_iso8601 = mv_ts_as_iso8601 type_descr = lo_elem_descr ).
      dump_type data lo_elem_descr r_json.

    WHEN cl_abap_typedescr=>kind_struct.

      lo_structdesc ?= type_descr.
      GET REFERENCE OF data INTO lo_data_ref.
      lt_symbols = get_symbols( type_descr = lo_structdesc data = lo_data_ref ).
      r_json = dump_symbols( lt_symbols ).

    WHEN cl_abap_typedescr=>kind_table.

      lo_tabledescr ?= type_descr.
      lo_typedesc = lo_tabledescr->get_table_line_type( ).

      ASSIGN data TO <table>.

      " optimization for structured tables
      IF lo_typedesc->kind EQ cl_abap_typedescr=>kind_struct.
        lo_structdesc ?= lo_typedesc.
        " INIMOD IRB
        " Se cambia la sentencia para poder hacer que funciona con tipos de datos generados dinámicamente
        CREATE DATA lo_data_ref TYPE HANDLE lo_structdesc.
        "CREATE DATA lo_data_ref TYPE (lo_structdesc->absolute_name).
        " ENDMOD IRB
        ASSIGN lo_data_ref->* TO <line>.
        lt_symbols = get_symbols( type_descr = lo_structdesc data = lo_data_ref ).

        " here we have differentiation of output of simple table to JSON array
        " and sorted or hashed table with unique key into JSON associative array
        IF lo_tabledescr->has_unique_key IS NOT INITIAL AND mv_assoc_arrays IS NOT INITIAL.

          IF lo_tabledescr->key_defkind EQ lo_tabledescr->keydefkind_user.
            LOOP AT lo_tabledescr->key ASSIGNING <key>.
              READ TABLE lt_symbols WITH KEY name = <key>-name ASSIGNING <symbol>.
              APPEND <symbol> TO lt_keys.
            ENDLOOP.
          ENDIF.

          IF lines( lo_tabledescr->key ) EQ 1.
            READ TABLE lo_tabledescr->key INDEX 1 INTO ls_skip_key.
            DELETE lt_symbols WHERE name EQ ls_skip_key-name.
            " remove object wrapping for simple name-value tables
            IF mv_assoc_arrays_opt EQ abap_true AND lines( lt_symbols ) EQ 1.
              lv_array_opt = abap_true.
            ENDIF.
          ENDIF.

          LOOP AT <table> INTO <line>.
            CLEAR: lt_fields, lv_prop_name.
            LOOP AT lt_symbols ASSIGNING <symbol>.
              ASSIGN <symbol>-value->* TO <value>.
              IF mv_compress IS INITIAL OR <value> IS NOT INITIAL OR <symbol>-compressable EQ abap_false.
                IF <symbol>-type->kind EQ cl_abap_typedescr=>kind_elem.
                  lo_elem_descr ?= <symbol>-type.
                  "lv_itemval = dump_type( data = <value> type_descr = l_elem_descr ).
                  dump_type <value> lo_elem_descr lv_itemval.
                ELSE.
                  " INIMOD IRB
                  lv_itemval = zdump_int( data = <value> type_descr = <symbol>-type ).
*                  lv_itemval = dump_int( data = <value> type_descr = <symbol>-type ).
                  " ENDMOD IRB
                ENDIF.
                IF lv_array_opt EQ abap_false.
                  CONCATENATE <symbol>-header lv_itemval INTO lv_itemval.
                ENDIF.
                APPEND lv_itemval TO lt_fields.
              ENDIF.
            ENDLOOP.

            IF lo_tabledescr->key_defkind EQ lo_tabledescr->keydefkind_user.
              LOOP AT lt_keys ASSIGNING <symbol>.
                ASSIGN <symbol>-value->* TO <value>.
                MOVE <value> TO lv_keyval.
                CONDENSE lv_keyval.
                IF lv_prop_name IS NOT INITIAL.
                  CONCATENATE lv_prop_name mc_key_separator lv_keyval INTO lv_prop_name.
                ELSE.
                  lv_prop_name = lv_keyval.
                ENDIF.
              ENDLOOP.
            ELSE.
              LOOP AT lt_symbols ASSIGNING <symbol>.
                ASSIGN <symbol>-value->* TO <value>.
                MOVE <value> TO lv_keyval.
                CONDENSE lv_keyval.
                IF lv_prop_name IS NOT INITIAL.
                  CONCATENATE lv_prop_name mc_key_separator lv_keyval INTO lv_prop_name.
                ELSE.
                  lv_prop_name = lv_keyval.
                ENDIF.
              ENDLOOP.
            ENDIF.

            CONCATENATE LINES OF lt_fields INTO lv_itemval SEPARATED BY `,`.
            IF lv_array_opt EQ abap_false.
              CONCATENATE `"` lv_prop_name `":{` lv_itemval `}` INTO lv_itemval.
            ELSE.
              CONCATENATE `"` lv_prop_name `":` lv_itemval `` INTO lv_itemval.
            ENDIF.
            APPEND lv_itemval TO lt_properties.

          ENDLOOP.

          CONCATENATE LINES OF lt_properties INTO r_json SEPARATED BY `,`.
          CONCATENATE `{` r_json `}` INTO r_json.

        ELSE.

          LOOP AT <table> INTO <line>.
            CLEAR lt_fields.
            LOOP AT lt_symbols ASSIGNING <symbol>.
              ASSIGN <symbol>-value->* TO <value>.
              IF mv_compress IS INITIAL OR <value> IS NOT INITIAL OR <symbol>-compressable EQ abap_false.
                IF <symbol>-type->kind EQ cl_abap_typedescr=>kind_elem.
                  lo_elem_descr ?= <symbol>-type.
                  "lv_itemval = dump_type( data = <value> type_descr = l_elem_descr ).
                  dump_type <value> lo_elem_descr lv_itemval.
                ELSE.
                  " INIMOD IRB
                  lv_itemval = zdump_int( data = <value> type_descr = <symbol>-type ).
*                  lv_itemval = dump_int( data = <value> type_descr = <symbol>-type ).
                  " ENDMOD IRB
                ENDIF.
                CONCATENATE <symbol>-header lv_itemval INTO lv_itemval.
                APPEND lv_itemval TO lt_fields.
              ENDIF.
            ENDLOOP.

            CONCATENATE LINES OF lt_fields INTO lv_itemval SEPARATED BY `,`.
            CONCATENATE `{` lv_itemval `}` INTO lv_itemval.
            APPEND lv_itemval TO lt_properties.
          ENDLOOP.

          CONCATENATE LINES OF lt_properties INTO r_json SEPARATED BY `,`.
          CONCATENATE `[` r_json `]` INTO r_json.

        ENDIF.
      ELSE.
        LOOP AT <table> ASSIGNING <value>.
          " INIMOD IRB
          lv_itemval = dump_int( data = <value> type_descr = lo_typedesc ).
*          lv_itemval = dump_int( data = <value> type_descr = lo_typedesc ).
          " ENDMOD IRB
          APPEND lv_itemval TO lt_properties.
        ENDLOOP.

        CONCATENATE LINES OF lt_properties INTO r_json SEPARATED BY `,`.
        CONCATENATE `[` r_json `]` INTO r_json.
      ENDIF.

  ENDCASE.

ENDMETHOD.                    "dump


METHOD zserialize.

  " **********************************************************************
  "! Usage examples and documentation can be found on SCN:
  " http://wiki.scn.sap.com/wiki/display/Snippets/One+more+ABAP+to+JSON+Serializer+and+Deserializer
  " **********************************************************************  "

  DATA: lo_json  TYPE REF TO zcl_al30_ui5_json.

  CREATE OBJECT lo_json
    EXPORTING
      compress         = compress
      pretty_name      = pretty_name
      assoc_arrays     = assoc_arrays
      assoc_arrays_opt = assoc_arrays_opt
      expand_includes  = expand_includes
      ts_as_iso8601    = ts_as_iso8601.

  r_json = lo_json->serialize_int( name = name data = data type_descr = type_descr ).

ENDMETHOD.                    "serialize
ENDCLASS.
