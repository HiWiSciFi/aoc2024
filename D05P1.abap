FUNCTION Z_FB_AOC_2024_D05_1
  IMPORTING
    IT_INPUT TYPE STRING_TABLE
  EXPORTING
    EV_RESULT TYPE INT8.



  TYPES: BEGIN OF ltys_edge,
           from TYPE i,
           to   TYPE i,
         END OF ltys_edge.

  TYPES: BEGIN OF ltys_page,
           id TYPE i,
         END OF ltys_page.

  TYPES ltyt_edgetab TYPE HASHED TABLE OF ltys_edge WITH UNIQUE KEY from to.
  TYPES ltyt_itab    TYPE HASHED TABLE OF ltys_page WITH UNIQUE KEY id.

  CLEAR ev_result.

  DATA lt_edges TYPE ltyt_edgetab.
  DATA(lv_updates_section) = abap_false.

  LOOP AT it_input ASSIGNING FIELD-SYMBOL(<fv_line>).
    IF strlen( <fv_line> ) = 0.
      lv_updates_section = abap_true.
      CONTINUE.
    ENDIF.

    IF lv_updates_section <> abap_true.
      " Create list of dependencies
      SPLIT <fv_line> AT '|' INTO TABLE DATA(lt_splits).
      INSERT VALUE #( from = lt_splits[ 1 ]
                      to   = lt_splits[ 2 ] ) INTO TABLE lt_edges.
    ELSE.
      " Check if updates are valid
      SPLIT <fv_line> AT ',' INTO TABLE lt_splits.
      DATA(lv_valid) = abap_true.

      DO lines( lt_splits ) TIMES.
        DATA(lv_outer) = sy-index.
        DO lines( lt_splits ) - lv_outer TIMES.
*        DO lines( lt_splits ) TIMES.
*          IF sy-index <= lv_outer.
*            CONTINUE.
*          ENDIF.

          LOOP AT lt_edges ASSIGNING FIELD-SYMBOL(<fv_edge>) WHERE to = lt_splits[ lv_outer ].
            IF <fv_edge>-from = lt_splits[ sy-index + lv_outer ].
              lv_valid = abap_false.
              EXIT.
            ENDIF.
          ENDLOOP.

          IF lv_valid = abap_false.
            EXIT.
          ENDIF.
        ENDDO.
        IF lv_valid = abap_false.
          EXIT.
        ENDIF.
      ENDDO.

      IF lv_valid = abap_true.
        ev_result = ev_result + lt_splits[ lines( lt_splits ) / 2 ].
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFUNCTION.
