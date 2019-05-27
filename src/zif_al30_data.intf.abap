INTERFACE zif_al30_data
  PUBLIC .


  CONSTANTS cv_ddic_fields TYPE tabname VALUE 'ZAL30_S_FIELDS_ALV' ##NO_TEXT.
  CONSTANTS cv_field_origen_data TYPE fieldname VALUE 'ZAL30_ORIGEN' ##NO_TEXT.
  CONSTANTS cv_field_style TYPE fieldname VALUE 'ZAL30_STYLE' ##NO_TEXT.
  CONSTANTS cv_field_tabix_ddic TYPE fieldname VALUE 'ZAL30_TABIX_DDIC' ##NO_TEXT.
  CONSTANTS cv_field_updkz TYPE fieldname VALUE 'ZAL30_UPDKZ' ##NO_TEXT.
  CONSTANTS cv_intf_exit TYPE seoclsname VALUE 'ZIF_AL30_EXIT_CLASS' ##NO_TEXT.
  CONSTANTS cv_mode_change TYPE char1 VALUE 'U' ##NO_TEXT.
  CONSTANTS cv_mode_delete TYPE char1 VALUE 'D' ##NO_TEXT.
  CONSTANTS cv_mode_insert TYPE char1 VALUE 'I' ##NO_TEXT.
  CONSTANTS cv_mode_view TYPE char1 VALUE 'V' ##NO_TEXT.
ENDINTERFACE.
