FUNCTION Z_FB_AOC_2024_D02_2
  IMPORTING
    IT_INPUT TYPE STRING_TABLE
  EXPORTING
    EV_RESULT TYPE INT8.



  TYPES ltyt_itab TYPE STANDARD TABLE OF i WITH NON-UNIQUE DEFAULT KEY.

  DO lines( it_input ) TIMES.
    SPLIT it_input[ sy-index ] AT ' ' INTO TABLE DATA(lt_row).

    DO lines( lt_row ) + 1 TIMES.
      DATA(lv_save_asc) = abap_true.
      DATA(lv_save_dsc) = abap_true.

      DATA(lt_cprow) = lt_row.
      IF sy-index <> 1.
        DELETE lt_cprow INDEX sy-index - 1.
      ENDIF.

      DO lines( lt_cprow ) - 1 TIMES.
        DATA(lv_diff) = CONV i( lt_cprow[ sy-index ] ) - CONV i( lt_cprow[ sy-index + 1 ] ).

        IF lv_save_asc = abap_true AND ( lv_diff > -1 OR lv_diff < -3 ).
          lv_save_asc = abap_false.
        ENDIF.

        IF lv_save_dsc = abap_true AND ( lv_diff < 1 OR lv_diff > 3 ).
          lv_save_dsc = abap_false.
        ENDIF.

        IF lv_save_asc <> abap_true AND lv_save_dsc <> abap_true.
          EXIT.
        ENDIF.
      ENDDO.

      IF lv_save_asc = abap_true OR lv_save_dsc = abap_true.
        ev_result = ev_result + 1.
        EXIT.
      ENDIF.
    ENDDO.
  ENDDO.
ENDFUNCTION.
