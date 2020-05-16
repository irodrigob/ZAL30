CLASS zcl_al30_view_ui5 DEFINITION
  PUBLIC
  INHERITING FROM zcl_al30_view
  CREATE PUBLIC .

  PUBLIC SECTION.
    "! <p class="shorttext synchronized">Save the original data in the global variable</p>
    "! This has to be done because the original data is lost at the end of the data collection service.
    "! @parameter io_original_data | <p class="shorttext synchronized">Original data object</p>
    METHODS set_original_data
      IMPORTING
        !io_original_data TYPE REF TO data.


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

  METHOD set_original_data.
    mo_original_data = io_original_data.
  ENDMETHOD.

ENDCLASS.
