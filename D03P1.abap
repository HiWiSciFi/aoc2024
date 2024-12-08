FUNCTION Z_FB_AOC_2024_D03_1
  IMPORTING
    IT_INPUT TYPE STRING_TABLE
  EXPORTING
    EV_RESULT TYPE INT8.



  CLEAR ev_result.

  FIND ALL OCCURRENCES OF REGEX 'mul\([0-9]{1,3},[0-9]{1,3}\)' IN TABLE it_input
       RESULTS DATA(lt_regex_result).

  LOOP AT lt_regex_result ASSIGNING FIELD-SYMBOL(<fv_match>).
    DATA(lv_muldata) = substring( val = it_input[ <fv_match>-line ]
                                  off = ( <fv_match>-offset + 4 )
                                  len = ( <fv_match>-length - 5 ) ).
    SPLIT lv_muldata AT ',' INTO TABLE DATA(lt_numbers).

    ev_result = ev_result + ( CONV i( lt_numbers[ 1 ] ) * CONV i( lt_numbers[ 2 ] ) ).
  ENDLOOP.
ENDFUNCTION.
