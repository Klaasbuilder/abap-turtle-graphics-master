*&---------------------------------------------------------------------*
*& Report ZBC401_T22_TGM_P1
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zbc401_t22_tgm_p1.

CONSTANTS x_max TYPE i VALUE 1000.
CONSTANTS y_max TYPE i VALUE 1000.


CLASS lcl_do_stuff DEFINITION.

  PUBLIC SECTION.
    METHODS init.
    METHODS do_copypasted_circle.
    METHODS do_lower_half_circle.
    METHODS set_color_white.
    METHODS: set_color_copy_pasted,
      do_random_stuff
      , draw_hat.

    DATA turtle TYPE REF TO zcl_turtle.
  PROTECTED SECTION.
  PRIVATE SECTION.


ENDCLASS.

CLASS lcl_do_stuff IMPLEMENTATION.

  METHOD init.
    turtle = zcl_turtle=>create( height = y_max width = x_max ).
    turtle->disable_random_colors( ).
    turtle->set_pen( VALUE #(
        stroke_color = `#FF00FF`
        stroke_width = 2 ) ).
  ENDMETHOD.

  METHOD do_copypasted_circle.
    turtle->enable_random_colors( ).
    DATA(i) = 0.
    DATA(num_sides) = 15.
    DATA(side_length) = 33.
    WHILE i < num_sides.
      turtle->forward( side_length ).
      turtle->right( 360 / num_sides ).

      i = i + 1.
    ENDWHILE.
    turtle->disable_random_colors( ).
  ENDMETHOD.

  METHOD do_lower_half_circle.
    DATA(i) = 0.
    DATA(num_sides) = 15.
    DATA(side_length) = 120.
    WHILE i < num_sides.
      IF i = 1.
        set_color_white( ).
      ENDIF.
      IF i = 2 OR i = 3 OR i = 4.
        set_color_white( ).
      ENDIF.
      IF i = 6.
        set_color_copy_pasted( ).
      ENDIF.

      IF i = 10.
        set_color_white( ).
      ENDIF.

      turtle->forward( side_length ).
      turtle->right( 360 / num_sides ).

      i = i + 1.
    ENDWHILE.
    turtle->enable_random_colors( ).

  ENDMETHOD.

  METHOD set_color_copy_pasted.
    turtle->set_pen( VALUE #(
        stroke_color = `#FF00FF`
        stroke_width = 2 ) ).
  ENDMETHOD.

  METHOD set_color_white.
    turtle->set_pen( VALUE #(
        stroke_color = `#FFFFFF`
        stroke_width = 2 ) ).
  ENDMETHOD.

  METHOD do_random_stuff.
    DATA(seed) = 34212397.
    turtle->enable_random_colors( ).
    DATA(ran_coord) = cl_abap_random_int=>create(
                                            seed = seed
                                            min  = 0
                                            max  = x_max - 612 ).
    turtle->goto( x = ran_coord->get_next( )
                  y = ran_coord->get_next( ) ).

    DATA(ran_degree) = cl_abap_random_int=>create(
    seed = seed
    min  = 0
                                                   max  = 360 ).
    DATA(ran_loop) = cl_abap_random_int=>create( min  = 20
    seed = seed
                                                 max  = 30 ).
    DATA(ran_direction) =  cl_abap_random_int=>create( min  = 0
    seed = seed
                                                 max  = 1 ).
    DO ran_loop->get_next( ) TIMES.
      DATA(direction) = ran_direction->get_next( ).
      IF direction = 0.
        turtle->right( degrees = ran_degree->get_next( ) ).
      ELSEIF direction = 1.
        turtle->left( degrees = ran_degree->get_next( ) ).
      ENDIF.
      turtle->forward( how_far = 612 ).
      turtle->right( degrees = ran_degree->get_next( ) ).
      turtle->forward( how_far = 231 ).
      turtle->left( degrees = ran_degree->get_next( ) ).
      turtle->forward( how_far = 333 - 123 ).
      turtle->right( degrees = ran_degree->get_next( ) ).
      turtle->forward( how_far = 666 - 111 ).
      turtle->goto( x = ran_coord->get_next( )
            y = ran_coord->get_next( ) ).
    ENDDO.
    turtle->enable_random_colors( ).

  ENDMETHOD.

  METHOD draw_hat.
    turtle->enable_random_colors( ).
    turtle->goto( x = 242 y = 175 ).
    turtle->forward( how_far = 150 ).
    turtle->goto( x = 242 + 150 + 150 y = 175 ).
    turtle->forward( how_far = 150 ).

    turtle->goto( x = 242 + 150 - 10 y = 0 ).
    turtle->right( degrees = 90 ).
    turtle->forward( 175 ).
    turtle->left( degrees = 90 ).
    turtle->forward( 140 + 10 + 10 ).
    turtle->left( degrees = 90 ).
    turtle->forward( 165 + 10 + 0 ).
    turtle->left( degrees = 90 ).
    turtle->forward( 165 + 10 + 0 - 20 ).
  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.

call screen 100.

*  DATA(program) = NEW lcl_do_stuff( ).
*  program->init( ).
*  ASSIGN program->turtle TO FIELD-SYMBOL(<turtle>).
*  <turtle>->goto( x = x_max / 3  y = y_max / 5 ).
*  program->do_copypasted_circle( ).
*  <turtle>->goto( x = x_max / 3 + 200 y = y_max / 5 ).
*  program->do_copypasted_circle( ).
*
*  <turtle>->goto( x = 300 + 100 - 100 + 50 + 20 + 5 + 5  y = 0 ).
*  program->do_lower_half_circle( ).
*  CALL METHOD program->draw_hat.
*
*  program->do_random_stuff( ).
*
*  " MESSAGE <turtle>->get_html( ) TYPE 'X'.
*
*  WRITE <turtle>->get_html( ).
*  ULINE.
*  ULINE.
*

*&---------------------------------------------------------------------*
*&      Module  HTML_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE html_9000 OUTPUT.

  TYPE-POOLS: cndp.
  DATA: go_container_html TYPE REF TO cl_gui_custom_container,
        go_picture_html   TYPE REF TO cl_gui_html_viewer,
        lv_url_html       TYPE cndp_url,
        ls_raw_html_line  TYPE string, " c LENGTH 255,
  lt_raw_html like standard table of ls_raw_html_line.

  CLEAR: go_picture_html, go_container_html.

  IF go_container_html IS INITIAL.

    CREATE OBJECT go_container_html
      EXPORTING
        container_name              = 'PHOTO' "NAME CUSTOM CONTAINER
        repid                       = 'Z_HTMLDYNPRO' "PROGRAMM NAME
        dynnr                       = '9000' "DYNPRONAME
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.
    IF sy-subrc <> 0.
      MESSAGE i001(00) WITH 'Fehler mit dem erstellen des HTML-Containers'.
      LEAVE LIST-PROCESSING.
    ENDIF.
  ENDIF.
  IF go_picture_html IS INITIAL.
    CREATE OBJECT go_picture_html
      EXPORTING
        parent = go_container_html
      EXCEPTIONS
        OTHERS = 2.
    IF sy-subrc <> 0.
      MESSAGE i001(00) WITH 'Fehler mit dem anzeigen des HTML'.
      LEAVE LIST-PROCESSING.
    ENDIF.
  ENDIF.
  IF go_picture_html IS NOT INITIAL.

    "HTML-Code
    "HTML-Beginn
    DATA(program) = NEW lcl_do_stuff( ).
    program->init( ).
    ASSIGN program->turtle TO FIELD-SYMBOL(<turtle>).
    <turtle>->goto( x = x_max / 3  y = y_max / 5 ).
    program->do_copypasted_circle( ).
    <turtle>->goto( x = x_max / 3 + 200 y = y_max / 5 ).
    program->do_copypasted_circle( ).

    <turtle>->goto( x = 300 + 100 - 100 + 50 + 20 + 5 + 5  y = 0 ).
    program->do_lower_half_circle( ).
    CALL METHOD program->draw_hat.

    program->do_random_stuff( ).

    " Bild mit HTML ausgeben:
    APPEND <turtle>->get_html( ) TO lt_raw_html.
    "lt_raw_html = <turtle>->get_html( ).




    CALL METHOD go_picture_html->load_data
      EXPORTING
        url          = lv_url_html
      IMPORTING
        assigned_url = lv_url_html
      CHANGING
        data_table   = lt_raw_html
      EXCEPTIONS
        OTHERS       = 1.

    IF sy-subrc = 0.
      "Rahmen weglassen
      CALL METHOD go_picture_html->set_ui_flag
        EXPORTING
          uiflag = cl_gui_html_viewer=>uiflag_no3dborder.

      "HTML-File anzeigen
      CALL METHOD go_picture_html->show_data
        EXPORTING
          url = lv_url_html.
    ELSE.
      MESSAGE i001(00) WITH 'Fehler beim Laden des HTML'.
      LEAVE LIST-PROCESSING.
    ENDIF.
  ENDIF.


ENDMODULE.
