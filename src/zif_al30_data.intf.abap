INTERFACE zif_al30_data
  PUBLIC .


  TYPES:
    tv_operation TYPE c LENGTH 1 .
  TYPES: tt_r_tabname TYPE RANGE OF tabname.
  TYPES: tv_action_auth TYPE c LENGTH 1.
  TYPES:
    BEGIN OF ts_key_value,
      key   TYPE string,
      value TYPE string,
    END OF ts_key_value .
  TYPES:
    tt_key_value TYPE STANDARD TABLE OF ts_key_value .
  TYPES:
    BEGIN OF ts_logon_lang,
      lang        TYPE sylangu,
      description TYPE string,
    END OF ts_logon_lang .
  TYPES:
    tt_logon_lang TYPE STANDARD TABLE OF ts_logon_lang .
  TYPES:
    tt_r_lang TYPE RANGE OF sylangu .
  TYPES:
    BEGIN OF ts_fields_view,
      tabname   TYPE tabname,
      fieldname TYPE fieldname.
      INCLUDE TYPE zal30_s_fields_attr_general.
      INCLUDE TYPE zal30_s_fields_info_ddic.
  TYPES:
  END OF ts_fields_view .
  TYPES:
    tt_fields_view TYPE STANDARD TABLE OF ts_fields_view .
  TYPES:
    BEGIN OF ts_fields_text_view,
      tabname   TYPE tabname,
      fieldname TYPE fieldname,
      pos_ddic  TYPE tabfdpos,
      spras     TYPE spras.
      INCLUDE TYPE zal30_s_fields_attr_text.
  TYPES:
  END OF ts_fields_text_view .
  TYPES:
    tt_fields_text_view TYPE STANDARD TABLE OF ts_fields_text_view .
  TYPES:
    BEGIN OF ts_fields_view_alv.
      INCLUDE TYPE ts_fields_view.
  TYPES:
    reptext TYPE reptext,
    celltab TYPE lvc_t_styl,
    END OF ts_fields_view_alv .
  TYPES:
    tt_fields_view_alv TYPE STANDARD TABLE OF ts_fields_view_alv .
  TYPES:
    BEGIN OF ts_fields_text_view_alv.
      INCLUDE TYPE ts_fields_text_view.
  TYPES:
    lbl_type_header TYPE zal30_e_label_type_for_header,
    celltab         TYPE lvc_t_styl,
    END OF ts_fields_text_view_alv .
  TYPES:
    tt_fields_text_view_alv TYPE STANDARD TABLE OF ts_fields_text_view_alv .
  TYPES:
    BEGIN OF ts_default_values_create.
      INCLUDE TYPE zal30_t_view.
  TYPES:
  END OF ts_default_values_create .
  TYPES:
    BEGIN OF ts_filter_read_data,
      fields_ranges TYPE rsds_trange,
      where_clauses TYPE rsds_twhere,
      expressions   TYPE rsds_texpr,
    END OF ts_filter_read_data .
  TYPES: tt_strings TYPE STANDARD TABLE OF string WITH EMPTY KEY.
  TYPES: BEGIN OF ts_row_status_msg,
           type    TYPE bapi_mtype,
           message TYPE string,
         END OF ts_row_status_msg.
  TYPES: tt_row_status_msg TYPE STANDARD TABLE OF ts_row_status_msg WITH EMPTY KEY.
  TYPES: BEGIN OF ts_allowed_transport_view,
           view_name TYPE tabname,
           allowed   TYPE sap_bool,
         END OF ts_allowed_transport_view.
  TYPES: tt_allowed_transport_view TYPE SORTED TABLE OF ts_allowed_transport_view WITH UNIQUE KEY view_name.

  CONSTANTS:
    BEGIN OF cs_internal_tables,
      auth_user TYPE tabname VALUE 'ZAL30_T_USR_AUTH',
    END OF cs_internal_tables .
  CONSTANTS cv_ddic_fields TYPE tabname VALUE 'ZAL30_S_FIELDS_ALV' ##NO_TEXT.
  CONSTANTS cv_fields_attr_text TYPE tabname VALUE 'ZAL30_S_FIELDS_ATTR_TEXT' ##NO_TEXT.
  CONSTANTS: BEGIN OF cs_control_fields_alv_data,
               origen_data    TYPE fieldname VALUE 'ZAL30_ORIGEN' ##NO_TEXT,
               style          TYPE fieldname VALUE 'ZAL30_STYLE' ##NO_TEXT,
               tabix          TYPE fieldname VALUE 'ZAL30_TABIX' ##NO_TEXT,
               updkz          TYPE fieldname VALUE 'ZAL30_UPDKZ' ##NO_TEXT,
               is_dict        TYPE fieldname VALUE 'ZAL30_IS_DICT',
               row_status     TYPE fieldname VALUE 'ZAL30_ROW_STATUS',
               row_status_msg TYPE fieldname VALUE 'ZAL30_ROW_STATUS_MSG',
               actions        TYPE fieldname VALUE 'ZAL30_ACTIONS',
             END OF cs_control_fields_alv_data.

  CONSTANTS cv_intf_exit TYPE seoclsname VALUE 'ZIF_AL30_EXIT_CLASS' ##NO_TEXT.
  CONSTANTS cv_mode_change TYPE cdchngind VALUE 'U' ##NO_TEXT.
  CONSTANTS cv_mode_delete TYPE cdchngind VALUE 'D' ##NO_TEXT.
  CONSTANTS cv_mode_insert TYPE cdchngind VALUE 'I' ##NO_TEXT.
  CONSTANTS cv_mode_view TYPE cdchngind VALUE 'V' ##NO_TEXT.
  CONSTANTS cv_operation_insert TYPE char01 VALUE 'I' ##NO_TEXT.
  CONSTANTS cv_operation_read TYPE char01 VALUE 'R' ##NO_TEXT.
  CONSTANTS cv_msg_id TYPE arbgb VALUE 'ZCA_AL30' ##NO_TEXT.
  CONSTANTS:
    BEGIN OF cs_msg_type,
      error   TYPE bapi_mtype VALUE 'E',
      dump    TYPE bapi_mtype VALUE 'X',
      success TYPE bapi_mtype VALUE 'S',
      warning TYPE bapi_mtype VALUE 'W',
      info    TYPE bapi_mtype VALUE 'I',
    END OF cs_msg_type .
  CONSTANTS:
    BEGIN OF cs_source_text,
      dictionary TYPE zal30_e_source_text VALUE 'D',
      manual     TYPE zal30_e_source_text VALUE 'M',
    END OF cs_source_text .
  CONSTANTS: BEGIN OF cs_domain,
               source_text       TYPE domname VALUE 'ZAL30_D_SOURCE_TEXT' ##NO_TEXT,
               label_type_header TYPE domname VALUE 'ZAL30_D_LABEL_TYPE_FOR_HEADER',
             END OF cs_domain.
  CONSTANTS: BEGIN OF cs_data_element,
               actions TYPE rollname VALUE 'ZAL30_E_ACTIONS',
             END OF cs_data_element.
  CONSTANTS:
    BEGIN OF cs_order_category,
      workbench   TYPE e070-korrdev VALUE 'SYST',
      customizing TYPE e070-korrdev VALUE 'CUST',
    END OF cs_order_category .
  CONSTANTS:
    BEGIN OF cs_level_auth_user,
      full TYPE zal30_e_level_auth VALUE 'F',
      read TYPE zal30_e_level_auth VALUE 'R',
      non  TYPE zal30_e_level_auth VALUE 'N',
    END OF cs_level_auth_user .
  CONSTANTS:
    BEGIN OF cs_action_auth,
      update TYPE tv_action_auth VALUE 'U',
      show   TYPE tv_action_auth VALUE 'S',
    END OF cs_action_auth .
  CONSTANTS:
    BEGIN OF cs_alias_sql,
      view      TYPE string VALUE 'T_00',
      texttable TYPE string VALUE 'T_01',
    END OF cs_alias_sql .
  CONSTANTS:
    BEGIN OF cs_order_objfunc,
      normal    TYPE objfunc VALUE space,
      delete    TYPE objfunc VALUE 'D',
      key_value TYPE objfunc VALUE 'K',
    END OF cs_order_objfunc .
  CONSTANTS:
    BEGIN OF cs_datatype,
      mandt TYPE datatype_d  VALUE 'CLNT',
    END OF cs_datatype .
  CONSTANTS:
    BEGIN OF cs_prog_tcode,
      configuration TYPE sytcode VALUE 'ZAL30_MAIN_CONF',
      view          TYPE sytcode VALUE 'ZAL30_MAIN_VIEW',
    END OF cs_prog_tcode .
  CONSTANTS: BEGIN OF cs_fix_field_conf,
               sel_screen        TYPE fieldname VALUE 'SEL_SCREEN',
               source_text       TYPE fieldname VALUE 'SOURCE_TEXT',
               tech              TYPE fieldname VALUE 'TECH',
               mandatory         TYPE fieldname VALUE 'MANDATORY',
               no_output         TYPE fieldname VALUE 'NO_OUTPUT',
               checkbox          TYPE fieldname VALUE 'CHECKBOX',
               virtual           TYPE fieldname VALUE 'VIRTUAL',
               virtual_dtel      TYPE fieldname VALUE 'VIRTUAL_DTEL',
               label_type_header TYPE fieldname VALUE 'LBL_TYPE_HEADER',
             END OF cs_fix_field_conf.
  CONSTANTS: BEGIN OF cs_selection_screen_view,
               data_element_text_field TYPE string VALUE 'RSDSTEXTS-TEXT',
             END OF cs_selection_screen_view.
  CONSTANTS: BEGIN OF cs_virtual_fields,
               prefix TYPE fieldname VALUE 'VIR_',
             END OF cs_virtual_fields.
  CONSTANTS: BEGIN OF cs_alv_toolbar_type,
               separator TYPE tb_btype VALUE 3,
               button    TYPE tb_btype VALUE 0,
             END OF cs_alv_toolbar_type.
  CONSTANTS: BEGIN OF cs_semaphor_alv_excep,
               ok      TYPE c VALUE 3,
               warning TYPE c VALUE 2,
               error   TYPE c VALUE 1,
             END OF cs_semaphor_alv_excep.
  CONSTANTS: BEGIN OF cs_row_status,
               error TYPE string VALUE 'ERROR',
               valid TYPE string VALUE 'VALID',
             END OF cs_row_status.
  CONSTANTS: BEGIN OF cs_label_type_col_header,
               short   TYPE zal30_e_label_type_for_header VALUE 'S',
               medium  TYPE zal30_e_label_type_for_header VALUE 'M',
               long    TYPE zal30_e_label_type_for_header VALUE 'L',
               header  TYPE zal30_e_label_type_for_header VALUE 'H',
               auto    TYPE zal30_e_label_type_for_header VALUE 'A',
               default TYPE zal30_e_label_type_for_header VALUE 'A',
             END OF cs_label_type_col_header.
ENDINTERFACE.
