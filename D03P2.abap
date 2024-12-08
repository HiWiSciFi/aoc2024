FUNCTION Z_FB_AOC_2024_D03_2
  IMPORTING
    IT_INPUT TYPE STRING_TABLE
  EXPORTING
    EV_RESULT TYPE INT8.



  CLEAR ev_result.

  FIND ALL OCCURRENCES OF REGEX `(mul\([0-9]{1,3},[0-9]{1,3}\))|(do\(\))|(don't\(\))` IN TABLE it_input
       RESULTS DATA(lt_regex_result).

  DATA(lv_do) = abap_true.

  LOOP AT lt_regex_result ASSIGNING FIELD-SYMBOL(<fv_match>).
    IF <fv_match>-submatches[ 2 ]-offset <> -1.
      lv_do = abap_true.
      CONTINUE.
    ELSEIF <fv_match>-submatches[ 3 ]-offset <> -1.
      lv_do = abap_false.
    ENDIF.

    IF lv_do <> abap_true.
      CONTINUE.
    ENDIF.

    DATA(lv_muldata) = substring( val = it_input[ <fv_match>-line ]
                                  off = ( <fv_match>-offset + 4 )
                                  len = ( <fv_match>-length - 5 ) ).
    SPLIT lv_muldata AT ',' INTO TABLE DATA(lt_numbers).

    ev_result = ev_result + ( CONV i( lt_numbers[ 1 ] ) * CONV i( lt_numbers[ 2 ] ) ).
  ENDLOOP.
ENDFUNCTION.
