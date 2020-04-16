CLASS zcl_al30_view_ui5 DEFINITION
  PUBLIC
  INHERITING FROM zcl_al30_view
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
  PROTECTED SECTION.
    METHODS add_edit_fields REDEFINITION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_al30_view_ui5 IMPLEMENTATION.
  METHOD add_edit_fields.

    " Campos de siempre
    super->add_edit_fields( CHANGING ct_fcat = ct_fcat ).

* Se añade el campo de estilo a medida para UI5
    INSERT VALUE #( fieldname = zif_al30_ui5_data=>cs_control_fields_ui5_data-style rollname = 'ZAL30_I_UI5_FIELDS_STYLES' ) INTO TABLE ct_fcat.

  ENDMETHOD.

ENDCLASS.
