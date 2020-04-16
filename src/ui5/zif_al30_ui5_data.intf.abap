INTERFACE zif_al30_ui5_data
  PUBLIC .

  TYPES: BEGIN OF ts_view_list_auth.
      INCLUDE TYPE zcl_al30_view=>ts_view_list.
  TYPES: level_auth TYPE zal30_e_level_auth,
         END OF ts_view_list_auth.
  TYPES: tt_view_list_auth TYPE STANDARD TABLE OF ts_view_list_auth WITH EMPTY KEY.
  TYPES: BEGIN OF ts_view_fields.
      INCLUDE TYPE zif_al30_data=>ts_fields_view.
      INCLUDE TYPE zal30_s_fields_attr_text.
  TYPES: edit      TYPE lvc_edit,
         type      TYPE inttype,
         len       TYPE intlen,
         decimals  TYPE decimals,
         lowercase TYPE lowercase,
         datatype  TYPE datatype_d,
         END OF ts_view_fields.
  TYPES: tt_view_fields TYPE STANDARD TABLE OF ts_view_fields WITH EMPTY KEY.

  CONSTANTS: BEGIN OF cs_control_fields_ui5_data,
               style TYPE fieldname VALUE 'ZAL30_UI5_STYLE' ##NO_TEXT,
             END OF cs_control_fields_ui5_data.
  CONSTANTS: BEGIN OF cs_javascript_boolean,
               true  TYPE zal30_e_ui5_js_boolean VALUE 'true' ##NO_TEXT,
               false TYPE zal30_e_ui5_js_boolean VALUE 'false' ##NO_TEXT,
             END OF cs_javascript_boolean.
ENDINTERFACE.
