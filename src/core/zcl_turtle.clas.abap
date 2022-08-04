CLASS zcl_turtle DEFINITION
  PUBLIC FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.

    CONSTANTS:
      BEGIN OF defaults,
        height TYPE i VALUE 800,
        width  TYPE i VALUE 600,
        title  TYPE string VALUE `abapTurtle`,
      END OF defaults.

    TYPES:
      BEGIN OF t_pen,
        stroke_color TYPE zcl_turtle_colors=>rgb_hex_color,
        stroke_width TYPE i,
        fill_color   TYPE zcl_turtle_colors=>rgb_hex_color,
        is_up        TYPE abap_bool,
      END OF t_pen.

    TYPES:
      BEGIN OF t_point,
        x TYPE i,
        y TYPE i,
      END OF t_point,
      t_points TYPE STANDARD TABLE OF t_point WITH KEY table_line.

    TYPES:
      BEGIN OF turtle_position,
        x     TYPE i,
        y     TYPE i,
        angle TYPE f,
      END OF turtle_position.

    TYPES: multiple_turtles TYPE STANDARD TABLE OF REF TO zcl_turtle.

    CLASS-METHODS create
      IMPORTING height           TYPE i DEFAULT defaults-height
                width            TYPE i DEFAULT defaults-width
                background_color TYPE zcl_turtle_colors=>rgb_hex_color OPTIONAL
                title            TYPE string DEFAULT defaults-title
                style            TYPE string OPTIONAL
      RETURNING VALUE(turtle)    TYPE REF TO zcl_turtle.

    "! Creates a new turtle based on an existing instance. The position, angle and pen are preserved.
    "! Does not preserve content.
    CLASS-METHODS from_existing
      IMPORTING existing_turtle TYPE REF TO zcl_turtle
      RETURNING VALUE(turtle)   TYPE REF TO zcl_turtle.

    "! Merges drawings of multiple turtles into one.
    CLASS-METHODS compose
      IMPORTING turtles       TYPE multiple_turtles
      RETURNING VALUE(turtle) TYPE REF TO zcl_turtle.

    METHODS constructor
      IMPORTING height           TYPE i
                width            TYPE i
                background_color TYPE zcl_turtle_colors=>rgb_hex_color OPTIONAL
                title            TYPE string
                style            TYPE string OPTIONAL.

    METHODS right
      IMPORTING degrees       TYPE f
      RETURNING VALUE(turtle) TYPE REF TO zcl_turtle.

    METHODS left
      IMPORTING degrees       TYPE f
      RETURNING VALUE(turtle) TYPE REF TO zcl_turtle.

    METHODS set_pen
      IMPORTING pen           TYPE t_pen
      RETURNING VALUE(turtle) TYPE REF TO zcl_turtle.

    METHODS goto
      IMPORTING x             TYPE i
                y             TYPE i
      RETURNING VALUE(turtle) TYPE REF TO zcl_turtle.

    METHODS set_angle
      IMPORTING angle TYPE f.

    METHODS forward
      IMPORTING how_far       TYPE i
      RETURNING VALUE(turtle) TYPE REF TO zcl_turtle.

    METHODS back
      IMPORTING how_far       TYPE i
      RETURNING VALUE(turtle) TYPE REF TO zcl_turtle.

    METHODS pen_up
      RETURNING VALUE(turtle) TYPE REF TO zcl_turtle.

    METHODS pen_down
      RETURNING VALUE(turtle) TYPE REF TO zcl_turtle.

    METHODS enable_random_colors.
    METHODS disable_random_colors.

    METHODS:
      get_svg RETURNING VALUE(svg) TYPE string,
      append_svg IMPORTING svg_to_append  TYPE string.

    METHODS:
      get_position RETURNING VALUE(result) TYPE turtle_position,
      set_position IMPORTING position TYPE turtle_position,
      set_color_scheme IMPORTING color_scheme TYPE zcl_turtle_colors=>rgb_hex_colors,
      set_width IMPORTING width TYPE i,
      set_height IMPORTING height TYPE i,
      set_svg IMPORTING svg TYPE string,
      set_style IMPORTING style TYPE string,
      get_html
        RETURNING VALUE(html) TYPE string.

    DATA: title        TYPE string READ-ONLY,
          svg          TYPE string READ-ONLY,
          width        TYPE i READ-ONLY,
          height       TYPE i READ-ONLY,
          position     TYPE turtle_position READ-ONLY,
          pen          TYPE t_pen READ-ONLY,
          color_scheme TYPE zcl_turtle_colors=>rgb_hex_colors READ-ONLY,
          svg_builder  TYPE REF TO zcl_turtle_svg READ-ONLY.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA use_random_colors TYPE abap_bool.
    DATA style TYPE string.


    METHODS line
      IMPORTING x_from        TYPE i
                y_from        TYPE i
                x_to          TYPE i
                y_to          TYPE i
      RETURNING VALUE(turtle) TYPE REF TO zcl_turtle.

ENDCLASS.



CLASS ZCL_TURTLE IMPLEMENTATION.


  METHOD append_svg.
    me->svg = me->svg && svg_to_append.
  ENDMETHOD.


  METHOD back.
    right( degrees = 180 ).
    forward( how_far ).
    right( degrees = 180 ).
  ENDMETHOD.


  METHOD compose.

    TYPES: tt_strings TYPE STANDARD TABLE OF string WITH EMPTY KEY.

    IF lines( turtles ) < 1.
      zcx_turtle_problem=>raise( `Not enough turtles to compose anything.` ).
    ENDIF.

    " start where the last one left off
    turtle = zcl_turtle=>from_existing( turtles[ lines( turtles ) ] ).

    " new image size is the largest of composed turtles
    DATA(new_width) = zcl_turtle_math=>find_max_int(
      VALUE #( FOR <w> IN turtles ( <w>->width ) ) ).

    DATA(new_height) = zcl_turtle_math=>find_max_int(
      VALUE #( FOR <h> IN turtles ( <h>->height ) ) ).

    turtle->set_height( new_height ).
    turtle->set_width( new_width ).

    DATA(composed_svg) = REDUCE string(
      INIT result = ``
        FOR <svg> IN VALUE tt_strings( FOR <x> IN turtles ( <x>->svg ) )
      NEXT result = result && <svg> ).

    turtle->append_svg( composed_svg ).

  ENDMETHOD.


  METHOD constructor.
    me->width = width.
    me->height = height.
    me->title = title.
    me->style = style.
    me->color_scheme = zcl_turtle_colors=>default_color_scheme.
    me->use_random_colors = abap_true.
    me->svg_builder = zcl_turtle_svg=>create( me ).

    IF background_color IS NOT INITIAL.
      me->set_pen( VALUE #( fill_color = background_color ) ).
      DATA(side_length) = 100.

      DATA(points) = VALUE t_points(
        ( x = 0 y = 0 )
        ( x = 0 + width y = 0 )
        ( x = 0 + width y = 0 + height )
        ( x = 0   y = 0 + height )
      ).

      DATA(polyline) = me->svg_builder->polyline( VALUE #( points = points ) ).
      me->append_svg( polyline ).
    ENDIF.

    me->pen = VALUE #(
     stroke_width = 1
     stroke_color = `#FF0000`
     is_up = abap_false
   ).
  ENDMETHOD.


  METHOD create.
    turtle = NEW zcl_turtle( width = width height = height background_color = background_color title = title ).
  ENDMETHOD.


  METHOD disable_random_colors.
    me->use_random_colors = abap_false.
  ENDMETHOD.


  METHOD enable_random_colors.
    me->use_random_colors = abap_true.
  ENDMETHOD.


  METHOD forward.

    DATA(old_position) = position.
    DATA(new_x) = how_far * cos( zcl_turtle_convert=>degrees_to_radians( old_position-angle ) ).
    DATA(new_y) = how_far * sin( zcl_turtle_convert=>degrees_to_radians( old_position-angle ) ).

    DATA(new_position) = VALUE turtle_position(
      x = old_position-x + new_x
      y = old_position-y + new_y
      angle = old_position-angle ).

    IF pen-is_up = abap_false.
      me->line(
        x_from = old_position-x
        y_from = old_position-y
        x_to   = new_position-x
        y_to   = new_position-y ).
    ENDIF.

    me->set_position( new_position ).

    turtle = me.
  ENDMETHOD.


  METHOD from_existing.
    turtle = NEW #(
      width = existing_turtle->width
      height = existing_turtle->height
      title = existing_turtle->title
      style = existing_turtle->style
    ).

    turtle->set_pen( existing_turtle->pen ).
    turtle->set_color_scheme( existing_turtle->color_scheme ).
    turtle->set_position( existing_turtle->position ).
    turtle->set_angle( existing_turtle->position-angle ).
  ENDMETHOD.


  METHOD get_html.
    html = zcl_turtle_html_parts=>html_document(
      title = me->title
      style = me->style
      svg   = |<svg width="{ me->width }" height="{ me->height }">{ me->svg }</svg>| ).
  ENDMETHOD.


  METHOD get_position.
    result = me->position.
  ENDMETHOD.


  METHOD get_svg.
    svg = me->svg.
  ENDMETHOD.


  METHOD goto.
    position-x = x.
    position-y = y.
    turtle = me.
  ENDMETHOD.


  METHOD left.
    position-angle = position-angle - degrees.
    position-angle = position-angle MOD 360.
    turtle = me.
  ENDMETHOD.


  METHOD line.

    IF use_random_colors = abap_true.
      pen-stroke_color = zcl_turtle_colors=>get_random_color( me->color_scheme ).
    ENDIF.

    DATA(line) = svg_builder->line( VALUE #(
      x_from = x_from
      y_from = y_from
      x_to = x_to
      y_to = y_to ) ).
    append_svg( line ).

    turtle = me.
  ENDMETHOD.


  METHOD pen_down.
    me->pen-is_up = abap_false.
    turtle = me.
  ENDMETHOD.


  METHOD pen_up.
    me->pen-is_up = abap_true.
    turtle = me.
  ENDMETHOD.


  METHOD right.
    position-angle = position-angle + degrees.
    position-angle = position-angle MOD 360.
    turtle = me.
  ENDMETHOD.


  METHOD set_angle.
    me->position-angle = angle.
  ENDMETHOD.


  METHOD set_color_scheme.
    me->color_scheme = color_scheme.
  ENDMETHOD.


  METHOD set_height.
    me->height = height.
  ENDMETHOD.


  METHOD set_pen.
    me->pen = pen.
    turtle = me.
  ENDMETHOD.


  METHOD set_position.
    me->position = position.
  ENDMETHOD.


  METHOD set_style.
    me->style = style.
  ENDMETHOD.


  METHOD set_svg.
    me->svg = svg.
  ENDMETHOD.


  METHOD set_width.
    me->width = width.
  ENDMETHOD.
ENDCLASS.
