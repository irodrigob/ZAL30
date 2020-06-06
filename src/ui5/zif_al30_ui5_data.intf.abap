INTERFACE zif_al30_ui5_data
  PUBLIC .

  TYPES: BEGIN OF ts_view_list.
      INCLUDE TYPE zcl_al30_view=>ts_view_list.
  TYPES: level_auth        TYPE zal30_e_level_auth,
         allowed_transport TYPE sap_bool,
         END OF ts_view_list.
  TYPES: tt_view_list TYPE STANDARD TABLE OF ts_view_list WITH EMPTY KEY.
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
  TYPES: BEGIN OF ts_return,
           type    TYPE bapiret2-type,
           message TYPE string,
         END OF ts_return.
  TYPES:tt_return TYPE STANDARD TABLE OF ts_return WITH EMPTY KEY.
  TYPES: BEGIN OF ts_f4_values,
           code        TYPE string,
           description TYPE string,
         END OF ts_f4_values.
  TYPES: tt_f4_values TYPE STANDARD TABLE OF ts_f4_values WITH EMPTY KEY.
  TYPES: BEGIN OF ts_f4_catalog,
           fieldname               TYPE fieldname,
           label_field_code        TYPE string,
           label_field_description TYPE string,
         END OF ts_f4_catalog.
  TYPES tt_f4_catalog TYPE STANDARD TABLE OF ts_f4_catalog WITH EMPTY KEY.
  TYPES: BEGIN OF ts_f4_data,
           code        TYPE string,
           description TYPE string,
         END OF ts_f4_data.
  TYPES: tt_f4_data TYPE STANDARD TABLE OF ts_f4_data WITH EMPTY KEY.

  CONSTANTS: BEGIN OF cs_control_fields_ui5_data,
               style TYPE fieldname VALUE 'ZAL30_UI5_STYLE' ##NO_TEXT,
             END OF cs_control_fields_ui5_data.
  CONSTANTS: BEGIN OF cs_javascript_boolean,
               true  TYPE zal30_e_ui5_js_boolean VALUE 'true' ##NO_TEXT,
               false TYPE zal30_e_ui5_js_boolean VALUE 'false' ##NO_TEXT,
             END OF cs_javascript_boolean.
  CONSTANTS: BEGIN OF cs_user,
               BEGIN OF default_configuration,
                 date_format        TYPE usr01-datfm VALUE '1',
                 decimal_separator  TYPE c LENGTH 1 VALUE ',',
                 thousand_separator TYPE c LENGTH 1 VALUE '.',
               END OF default_configuration,
             END OF cs_user.
ENDINTERFACE.
