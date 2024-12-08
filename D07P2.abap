FUNCTION Z_FB_AOC_2024_D07_2
  IMPORTING
    IT_INPUT TYPE STRING_TABLE
  EXPORTING
    EV_RESULT TYPE INT8.



  CLEAR ev_result.

  LOOP AT it_input ASSIGNING FIELD-SYMBOL(<fv_line>).
    SPLIT <fv_line> AT space INTO TABLE DATA(lt_values).
    DATA(lv_total) = CONV int8( substring( val = lt_values[ 1 ]
                                           off = 0
                                           len = strlen( lt_values[ 1 ] ) - 1 ) ).
    DATA(lv_start) = CONV int8( lt_values[ 2 ] ).
    DELETE lt_values INDEX 2.
    DELETE lt_values INDEX 1.

    DO 3 ** lines( lt_values ) TIMES.
      DATA(lv_binnr) = /ui2/cl_number=>base_converter( number = ( sy-index - 1 )
                                                       from   = 10
                                                       to     = 3 ).
      DATA(lv_sum) = lv_start.
      LOOP AT lt_values ASSIGNING FIELD-SYMBOL(<fv_value>).
        DATA(lv_offset) = strlen( lv_binnr ) - ( sy-tabix - 1 ) - 1.
        DATA(lv_bit) = COND #( WHEN lv_offset < 0 THEN '0' ELSE lv_binnr+lv_offset(1) ).
        IF lv_bit = '0'.
          lv_sum = lv_sum * <fv_value>.
        ELSEIF lv_bit = '1'.
          lv_sum = lv_sum + <fv_value>.
        ELSE.
          lv_sum = |{ lv_sum }{ <fv_value> }|.
        ENDIF.
        IF lv_sum > lv_total.
          EXIT.
        ENDIF.
      ENDLOOP.

      IF lv_sum = lv_total.
        ev_result = ev_result + lv_sum.
        EXIT.
      ENDIF.
    ENDDO.
  ENDLOOP.
ENDFUNCTION.
