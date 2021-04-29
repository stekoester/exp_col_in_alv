FORM data_get.
  DO.
    ASSIGN COMPONENT sy-index OF STRUCTURE gcs_tadir_objects TO FIELD-SYMBOL(<lv_object>).
    IF sy-subrc NE 0.
      EXIT.
    ENDIF.
    SELECT pgmid object obj_name korrnum srcsystem author srcdep devclass
      FROM tadir APPENDING TABLE gt_tadir              "#EC CI_GENBUFF
      UP TO 5 ROWS
      WHERE pgmid    EQ 'R3TR'
        AND object   EQ <lv_object>
        AND devclass EQ 'SCTS_CAT'.
    IF sy-subrc EQ 0.
      DATA(ls_tadir) = gt_tadir[ lines( gt_tadir ) ].
      APPEND VALUE #( pgmid     = ls_tadir-pgmid
                      object    = ls_tadir-object
                      expand    = lcl_handle_events=>get_icon( iv_type = 'E' )
                      cell_type = VALUE #( ( fieldname = 'EXPAND'
                                             style     = cl_gui_alv_grid=>mc_style_hotspot ) ) ) TO gt_tadir_output.
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
  DATA(lo_alv_grid) = NEW cl_gui_alv_grid( NEW cl_gui_custom_container( 'ALV' ) ).
  DATA(lr_event_handler) = NEW lcl_handle_events( ).
  SET HANDLER lr_event_handler->handle_added_function FOR lo_alv_grid.
  SET HANDLER lr_event_handler->handle_link_click FOR lo_alv_grid.
  SET HANDLER lr_event_handler->handle_user_command FOR lo_alv_grid.
  lo_alv_grid->set_table_for_first_display(
    EXPORTING
      i_structure_name              = 'ZKCO_SAMPLES_ALV_TADIR_OUTPUT'
      is_layout                     = VALUE #( cwidth_opt = abap_true stylefname = 'CELL_TYPE' zebra = abap_true )
      it_toolbar_excluding          = VALUE #( ( cl_gui_alv_grid=>mc_fc_auf ) ( cl_gui_alv_grid=>mc_fc_average ) ( cl_gui_alv_grid=>mc_fc_back_classic )
                                               ( cl_gui_alv_grid=>mc_fc_call_abc ) ( cl_gui_alv_grid=>mc_fc_call_chain ) ( cl_gui_alv_grid=>mc_fc_call_crbatch )
                                               ( cl_gui_alv_grid=>mc_fc_call_crweb ) ( cl_gui_alv_grid=>mc_fc_call_lineitems ) ( cl_gui_alv_grid=>mc_fc_call_master_data )
                                               ( cl_gui_alv_grid=>mc_fc_call_more ) ( cl_gui_alv_grid=>mc_fc_call_report ) ( cl_gui_alv_grid=>mc_fc_call_xint )
                                               ( cl_gui_alv_grid=>mc_fc_call_xml_export ) ( cl_gui_alv_grid=>mc_fc_call_xxl ) ( cl_gui_alv_grid=>mc_fc_check )
                                               ( cl_gui_alv_grid=>mc_fc_col_invisible ) ( cl_gui_alv_grid=>mc_fc_col_optimize ) ( cl_gui_alv_grid=>mc_fc_count )
                                               ( cl_gui_alv_grid=>mc_fc_current_variant ) ( cl_gui_alv_grid=>mc_fc_data_save ) ( cl_gui_alv_grid=>mc_fc_delete_filter )
                                               ( cl_gui_alv_grid=>mc_fc_deselect_all ) ( cl_gui_alv_grid=>mc_fc_detail ) ( cl_gui_alv_grid=>mc_fc_excl_all )
                                               ( cl_gui_alv_grid=>mc_fc_expcrdata ) ( cl_gui_alv_grid=>mc_fc_expcrdesig ) ( cl_gui_alv_grid=>mc_fc_expcrtempl )
                                               ( cl_gui_alv_grid=>mc_fc_expmdb ) ( cl_gui_alv_grid=>mc_fc_extend ) ( cl_gui_alv_grid=>mc_fc_f4 )
                                               ( cl_gui_alv_grid=>mc_fc_filter ) ( cl_gui_alv_grid=>mc_fc_find ) ( cl_gui_alv_grid=>mc_fc_find_more )
                                               ( cl_gui_alv_grid=>mc_fc_fix_columns ) ( cl_gui_alv_grid=>mc_fc_graph ) ( cl_gui_alv_grid=>mc_fc_help )
                                               ( cl_gui_alv_grid=>mc_fc_html ) ( cl_gui_alv_grid=>mc_fc_info ) ( cl_gui_alv_grid=>mc_fc_load_variant )
                                               ( cl_gui_alv_grid=>mc_fc_loc_append_row ) ( cl_gui_alv_grid=>mc_fc_loc_copy ) ( cl_gui_alv_grid=>mc_fc_loc_copy_row )
                                               ( cl_gui_alv_grid=>mc_fc_loc_cut ) ( cl_gui_alv_grid=>mc_fc_loc_delete_row ) ( cl_gui_alv_grid=>mc_fc_loc_insert_row )
                                               ( cl_gui_alv_grid=>mc_fc_loc_move_row ) ( cl_gui_alv_grid=>mc_fc_loc_paste ) ( cl_gui_alv_grid=>mc_fc_loc_paste_new_row )
                                               ( cl_gui_alv_grid=>mc_fc_loc_undo ) ( cl_gui_alv_grid=>mc_fc_maintain_variant ) ( cl_gui_alv_grid=>mc_fc_maximum )
                                               ( cl_gui_alv_grid=>mc_fc_minimum ) ( cl_gui_alv_grid=>mc_fc_pc_file ) ( cl_gui_alv_grid=>mc_fc_print )
                                               ( cl_gui_alv_grid=>mc_fc_print_back ) ( cl_gui_alv_grid=>mc_fc_print_prev ) ( cl_gui_alv_grid=>mc_fc_refresh )
                                               ( cl_gui_alv_grid=>mc_fc_reprep ) ( cl_gui_alv_grid=>mc_fc_save_variant ) ( cl_gui_alv_grid=>mc_fc_select_all )
                                               ( cl_gui_alv_grid=>mc_fc_send ) ( cl_gui_alv_grid=>mc_fc_separator ) ( cl_gui_alv_grid=>mc_fc_sort )
                                               ( cl_gui_alv_grid=>mc_fc_sort_asc ) ( cl_gui_alv_grid=>mc_fc_sort_dsc ) ( cl_gui_alv_grid=>mc_fc_subtot )
                                               ( cl_gui_alv_grid=>mc_fc_sum ) ( cl_gui_alv_grid=>mc_fc_to_office ) ( cl_gui_alv_grid=>mc_fc_to_rep_tree )
                                               ( cl_gui_alv_grid=>mc_fc_unfix_columns ) ( cl_gui_alv_grid=>mc_fc_url_copy_to_clipboard ) ( cl_gui_alv_grid=>mc_fc_variant_admin )
                                               ( cl_gui_alv_grid=>mc_fc_views ) ( cl_gui_alv_grid=>mc_fc_view_crystal ) ( cl_gui_alv_grid=>mc_fc_view_excel )
                                               ( cl_gui_alv_grid=>mc_fc_view_grid ) ( cl_gui_alv_grid=>mc_fc_view_lotus ) ( cl_gui_alv_grid=>mc_fc_word_processor )
                                               ( cl_gui_alv_grid=>mc_mb_export ) ( cl_gui_alv_grid=>mc_mb_filter ) ( cl_gui_alv_grid=>mc_mb_paste )
                                               ( cl_gui_alv_grid=>mc_mb_subtot ) ( cl_gui_alv_grid=>mc_mb_sum ) ( cl_gui_alv_grid=>mc_mb_variant )
                                               ( cl_gui_alv_grid=>mc_mb_view ) )
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
        et_fieldcatalog = DATA(lt_fieldcatalog) ).
    ASSIGN lt_fieldcatalog[ fieldname = 'EXPAND' ] TO FIELD-SYMBOL(<ls_fieldcatalof>).
    IF sy-subrc EQ 0.
      <ls_fieldcatalof>-just = 'C'.
    ENDIF.
    lo_alv_grid->set_frontend_fieldcatalog( lt_fieldcatalog ).
    lo_alv_grid->refresh_table_display( ).
  ENDIF.
ENDFORM.
