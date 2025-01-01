CONSTANTS:
  BEGIN OF co_tadir_objects,
    doma TYPE trobjtype VALUE 'DOMA',
    dtel TYPE trobjtype VALUE 'DTEL',
    prog TYPE trobjtype VALUE 'PROG',
    ssfo TYPE trobjtype VALUE 'SHLP',
    form TYPE trobjtype VALUE 'TABL',
  END OF co_tadir_objects.

DATA gt_tadir TYPE zkco_samples_alv_tadir_t.                "#EC NEEDED
DATA gt_tadir_output TYPE zkco_samples_alv_tadir_out_t.     "#EC NEEDED
