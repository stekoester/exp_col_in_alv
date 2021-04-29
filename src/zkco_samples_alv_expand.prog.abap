*&---------------------------------------------------------------------*
*& This report demonstrates how to add a button in ALV to
*& expand/collapse rows to increase the result with more details per
*& line.
*&---------------------------------------------------------------------*
REPORT zkco_samples_alv_expand.

INCLUDE zkco_samples_alv_expand_top.
INCLUDE zkco_samples_alv_expand_cl1.
INCLUDE zkco_samples_alv_expand_f01.
INCLUDE zkco_samples_alv_expand_o0100.
INCLUDE zkco_samples_alv_expand_i0100.

INITIALIZATION.

AT SELECTION-SCREEN OUTPUT.

AT SELECTION-SCREEN.

START-OF-SELECTION.
  PERFORM data_get.

END-OF-SELECTION.
  PERFORM data_display.
