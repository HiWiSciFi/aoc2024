FUNCTION Z_FB_AOC_2024_D01_2
  IMPORTING
    IT_INPUT TYPE STRING_TABLE
  EXPORTING
    EV_RESULT TYPE INT8.



  TYPES ltyt_itab TYPE SORTED TABLE OF i WITH NON-UNIQUE DEFAULT KEY.

  DATA lt_data1 TYPE ltyt_itab.
  DATA lt_data2 TYPE ltyt_itab.

  LOOP AT it_input ASSIGNING FIELD-SYMBOL(<fv_input>).
    SPLIT <fv_input> AT '   ' INTO TABLE DATA(lt_split).

    INSERT CONV #( lt_split[ 1 ] ) INTO TABLE lt_data1.
    INSERT CONV #( lt_split[ 2 ] ) INTO TABLE lt_data2.
  ENDLOOP.

  DO lines( lt_data1 ) TIMES.
    DATA(lv_val) = lt_data1[ sy-index ] * REDUCE i( INIT x = 0 FOR wa IN lt_data2 NEXT x = COND #( WHEN wa = lt_data1[
                                                                                                       sy-index ]
                                                                                                   THEN x + 1
                                                                                                   ELSE x ) ).
    ev_result = ev_result + lv_val.
  ENDDO.
ENDFUNCTION.
