INTERFACE zif_al30_ui5_data
  PUBLIC .

  TYPES: BEGIN OF ts_view_list_auth.
      INCLUDE TYPE zcl_al30_view=>ts_view_list.
  TYPES: level_auth TYPE zal30_e_level_auth,
         END OF ts_view_list_auth.
  TYPES: tt_view_list_auth TYPE STANDARD TABLE OF ts_view_list_auth WITH EMPTY KEY.

ENDINTERFACE.
