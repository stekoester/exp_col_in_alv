FORM data_get.
  DATA:
    ls_tadir_output TYPE zkco_samples_alv_tadir_output,
    ls_tadir        TYPE zkco_samples_alv_tadir,
    ls_cell_type    TYPE lvc_s_styl.

  FIELD-SYMBOLS:
    <lv_object> TYPE trobjtype.

  ls_cell_type-fieldname = 'EXPAND'.
  ls_cell_type-style     = cl_gui_alv_grid=>mc_style_hotspot.
  APPEND ls_cell_type TO ls_tadir_output-cell_type.
  DO.
    ASSIGN COMPONENT sy-index OF STRUCTURE gcs_tadir_objects TO <lv_object>.
    IF sy-subrc NE 0.
      EXIT.
    ENDIF.
    SELECT pgmid object obj_name korrnum srcsystem author srcdep devclass
      FROM tadir APPENDING TABLE gt_tadir               "#EC CI_GENBUFF
      UP TO 5 ROWS
      WHERE pgmid    EQ 'R3TR'
        AND object   EQ <lv_object>
        AND devclass EQ 'SCTS_CAT'.
    IF sy-subrc EQ 0.
      READ TABLE gt_tadir INTO ls_tadir INDEX lines( gt_tadir ).
      ls_tadir_output-pgmid     = ls_tadir-pgmid.
      ls_tadir_output-object    = ls_tadir-object.
      ls_tadir_output-expand    = lcl_handle_events=>get_icon( iv_type = 'E' ).
      APPEND ls_tadir_output TO gt_tadir_output.
    ENDIF.
  ENDDO.
  IF gt_tadir_output IS INITIAL.
    MESSAGE 'No data selected' TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.
ENDFORM.

FORM data_display.
  CALL SCREEN 100.
ENDFORM.

FORM alv_display.
  DATA:
    ls_layout             TYPE lvc_s_layo.

  DATA:
    lt_toolbar_excluding  TYPE ui_functions,
    lt_fieldcatalog       TYPE lvc_t_fcat.

  DATA:
    lo_custom_container   TYPE REF TO cl_gui_custom_container,
    lo_alv_grid           TYPE REF TO cl_gui_alv_grid,
    lr_event_handler      TYPE REF TO lcl_handle_events.

  FIELD-SYMBOLS:
    <ls_fieldcatalog>     TYPE lvc_s_fcat.

  CREATE OBJECT lo_custom_container
    EXPORTING
      container_name              = 'ALV'
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      lifetime_dynpro_dynpro_link = 5
      OTHERS                      = 6.
  IF sy-subrc EQ 0.
    CREATE OBJECT lo_alv_grid
      EXPORTING
        i_parent          = lo_custom_container
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.
    IF sy-subrc EQ 0.
      CREATE OBJECT lr_event_handler.
      SET HANDLER lr_event_handler->handle_added_function FOR lo_alv_grid.
      SET HANDLER lr_event_handler->handle_link_click FOR lo_alv_grid.
      SET HANDLER lr_event_handler->handle_user_command FOR lo_alv_grid.
      ls_layout-cwidth_opt = abap_true.
      ls_layout-stylefname = 'CELL_TYPE'.
      ls_layout-zebra = abap_true.
      PERFORM get_toolbar_excluding CHANGING lt_toolbar_excluding.
      lo_alv_grid->set_table_for_first_display(
        EXPORTING
          i_structure_name              = 'ZKCO_SAMPLES_ALV_TADIR_OUTPUT'
          is_layout                     = ls_layout
          it_toolbar_excluding          = lt_toolbar_excluding
        CHANGING
          it_outtab                     = gt_tadir_output
        EXCEPTIONS
          invalid_parameter_combination = 1                " Wrong Parameter
          program_error                 = 2                " Program Errors
          too_many_lines                = 3                " Too many Rows in Ready for Input Grid
          OTHERS                        = 4 ).
      IF sy-subrc EQ 0.
        lo_alv_grid->get_frontend_fieldcatalog(
          IMPORTING
            et_fieldcatalog = lt_fieldcatalog ).
        READ TABLE lt_fieldcatalog ASSIGNING <ls_fieldcatalog> WITH KEY fieldname = 'EXPAND'.
        IF sy-subrc EQ 0.
          <ls_fieldcatalog>-just = 'C'.
        ENDIF.
        lo_alv_grid->set_frontend_fieldcatalog( lt_fieldcatalog ).
        lo_alv_grid->refresh_table_display( ).
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.

FORM get_toolbar_excluding CHANGING ct_toolbar_excluding TYPE ui_functions.
  APPEND cl_gui_alv_grid=>mc_fc_auf                   TO ct_toolbar_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_average               TO ct_toolbar_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_back_classic          TO ct_toolbar_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_call_abc              TO ct_toolbar_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_call_chain            TO ct_toolbar_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_call_crbatch          TO ct_toolbar_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_call_crweb            TO ct_toolbar_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_call_lineitems        TO ct_toolbar_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_call_master_data      TO ct_toolbar_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_call_more             TO ct_toolbar_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_call_report           TO ct_toolbar_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_call_xint             TO ct_toolbar_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_call_xml_export       TO ct_toolbar_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_call_xxl              TO ct_toolbar_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_check                 TO ct_toolbar_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_col_invisible         TO ct_toolbar_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_col_optimize          TO ct_toolbar_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_count                 TO ct_toolbar_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_current_variant       TO ct_toolbar_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_data_save             TO ct_toolbar_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_delete_filter         TO ct_toolbar_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_deselect_all          TO ct_toolbar_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_detail                TO ct_toolbar_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_excl_all              TO ct_toolbar_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_expcrdata             TO ct_toolbar_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_expcrdesig            TO ct_toolbar_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_expcrtempl            TO ct_toolbar_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_expmdb                TO ct_toolbar_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_extend                TO ct_toolbar_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_f4                    TO ct_toolbar_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_filter                TO ct_toolbar_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_find                  TO ct_toolbar_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_find_more             TO ct_toolbar_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_fix_columns           TO ct_toolbar_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_graph                 TO ct_toolbar_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_help                  TO ct_toolbar_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_html                  TO ct_toolbar_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_info                  TO ct_toolbar_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_load_variant          TO ct_toolbar_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_loc_append_row        TO ct_toolbar_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_loc_copy              TO ct_toolbar_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_loc_copy_row          TO ct_toolbar_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_loc_cut               TO ct_toolbar_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_loc_delete_row        TO ct_toolbar_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_loc_insert_row        TO ct_toolbar_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_loc_move_row          TO ct_toolbar_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_loc_paste             TO ct_toolbar_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_loc_paste_new_row     TO ct_toolbar_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_loc_undo              TO ct_toolbar_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_maintain_variant      TO ct_toolbar_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_maximum               TO ct_toolbar_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_minimum               TO ct_toolbar_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_pc_file               TO ct_toolbar_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_print                 TO ct_toolbar_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_print_back            TO ct_toolbar_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_print_prev            TO ct_toolbar_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_refresh               TO ct_toolbar_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_reprep                TO ct_toolbar_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_save_variant          TO ct_toolbar_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_select_all            TO ct_toolbar_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_send                  TO ct_toolbar_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_separator             TO ct_toolbar_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_sort                  TO ct_toolbar_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_sort_asc              TO ct_toolbar_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_sort_dsc              TO ct_toolbar_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_subtot                TO ct_toolbar_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_sum                   TO ct_toolbar_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_to_office             TO ct_toolbar_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_to_rep_tree           TO ct_toolbar_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_unfix_columns         TO ct_toolbar_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_url_copy_to_clipboard TO ct_toolbar_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_variant_admin         TO ct_toolbar_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_views                 TO ct_toolbar_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_view_crystal          TO ct_toolbar_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_view_excel            TO ct_toolbar_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_view_grid             TO ct_toolbar_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_view_lotus            TO ct_toolbar_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_word_processor        TO ct_toolbar_excluding.
  APPEND cl_gui_alv_grid=>mc_mb_export                TO ct_toolbar_excluding.
  APPEND cl_gui_alv_grid=>mc_mb_filter                TO ct_toolbar_excluding.
  APPEND cl_gui_alv_grid=>mc_mb_paste                 TO ct_toolbar_excluding.
  APPEND cl_gui_alv_grid=>mc_mb_subtot                TO ct_toolbar_excluding.
  APPEND cl_gui_alv_grid=>mc_mb_sum                   TO ct_toolbar_excluding.
  APPEND cl_gui_alv_grid=>mc_mb_variant               TO ct_toolbar_excluding.
  APPEND cl_gui_alv_grid=>mc_mb_view                  TO ct_toolbar_excluding.
ENDFORM.
