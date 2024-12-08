FUNCTION Z_FB_AOC_2024_D02_1
  IMPORTING
    IT_INPUT TYPE STRING_TABLE
  EXPORTING
    EV_RESULT TYPE INT8.



  TYPES ltyt_itab TYPE STANDARD TABLE OF i WITH NON-UNIQUE DEFAULT KEY.

  DO lines( it_input ) TIMES.
    SPLIT it_input[ sy-index ] AT ' ' INTO TABLE DATA(lt_row).
    DATA(lv_save) = abap_true.

    DO lines( lt_row ) - 1 TIMES.
      DATA(lv_curr) = CONV i( lt_row[ sy-index ] ).
      DATA(lv_next) = CONV i( lt_row[ sy-index + 1 ] ).
      DATA(lv_diff) = abs( lv_curr - lv_next ).

      IF lv_next <= lv_curr OR lv_diff < 1 OR lv_diff > 3.
        lv_save = abap_false.
        EXIT.
      ENDIF.
    ENDDO.

    IF lv_save = abap_true.
      ev_result = ev_result + 1.
      CONTINUE.
    ENDIF.
    lv_save = abap_true.

    DO lines( lt_row ) - 1 TIMES.
      lv_curr = lt_row[ sy-index ].
      lv_next = lt_row[ sy-index + 1 ].
      lv_diff = abs( lv_curr - lv_next ).

      IF lv_next >= lv_curr OR lv_diff < 1 OR lv_diff > 3.
        lv_save = abap_false.
        EXIT.
      ENDIF.
    ENDDO.

    IF lv_save = abap_true.
      ev_result = ev_result + 1.
      CONTINUE.
    ENDIF.
    lv_save = abap_true.

  ENDDO.
ENDFUNCTION.
