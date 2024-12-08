FUNCTION Z_FB_AOC_2024_D04_1
  IMPORTING
    IT_INPUT TYPE STRING_TABLE
  EXPORTING
    EV_RESULT TYPE INT8.



  CLEAR ev_result.

  TYPES: BEGIN OF ltys_vec,
           x TYPE i,
           y TYPE i,
         END OF ltys_vec.

  TYPES ltyt_vectab  TYPE STANDARD TABLE OF ltys_vec WITH NON-UNIQUE DEFAULT KEY.
  TYPES ltyt_chartab TYPE STANDARD TABLE OF char1 WITH NON-UNIQUE DEFAULT KEY.

  DATA(lt_dirs) = VALUE ltyt_vectab( ( x = -1 y = -1 )
                                     ( x =  0 y = -1 )
                                     ( x =  1 y = -1 )
                                     ( x = -1 y =  0 )
                                     ( x =  1 y =  0 )
                                     ( x = -1 y =  1 )
                                     ( x =  0 y =  1 )
                                     ( x =  1 y =  1 ) ).

  DATA(lt_chars) = VALUE ltyt_chartab( ( 'M' ) ( 'A' ) ( 'S' ) ).

  FIND ALL OCCURRENCES OF 'X' IN TABLE it_input
       RESULTS DATA(lt_match_result).

  LOOP AT lt_match_result ASSIGNING FIELD-SYMBOL(<fs_match>).
    LOOP AT lt_dirs ASSIGNING FIELD-SYMBOL(<fs_dir>).
      DATA(ls_currpos) = VALUE ltys_vec( x = <fs_match>-offset
                                         y = <fs_match>-line ).
      DATA(lv_hit) = abap_true.

      LOOP AT lt_chars ASSIGNING FIELD-SYMBOL(<fv_comp>).
        ls_currpos-x = ls_currpos-x + <fs_dir>-x.
        ls_currpos-y = ls_currpos-y + <fs_dir>-y.

        IF ls_currpos-x < 0 OR ls_currpos-x >= strlen( it_input[ 1 ] ) OR ls_currpos-y < 1 OR ls_currpos-y > lines(
            it_input ).
          lv_hit = abap_false.
          EXIT.
        ENDIF.

        DATA(lv_line) = it_input[ ls_currpos-y ].
        DATA(lv_offset) = ls_currpos-x.
        DATA(lv_char) = lv_line+lv_offset(1).

        IF lv_char <> <fv_comp>.
          lv_hit = abap_false.
          EXIT.
        ENDIF.
      ENDLOOP.

      IF lv_hit = abap_true.
*        WRITE: <fs_match>-offset, <fs_match>-line, <fs_dir>-x, <fs_dir>-y, /.
        ev_result = ev_result + 1.
      ENDIF.

    ENDLOOP.
  ENDLOOP.
ENDFUNCTION.
