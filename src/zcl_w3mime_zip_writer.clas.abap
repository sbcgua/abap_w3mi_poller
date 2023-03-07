class zcl_w3mime_zip_writer definition
  public
  final
  create public .

  public section.

    methods constructor
      importing
        !io_zip      type ref to cl_abap_zip optional
        !iv_encoding type abap_encoding optional .
    methods add
      importing
        !iv_filename type string
        !iv_data     type string .
    methods addx
      importing
        !iv_filename type string
        !iv_xdata    type xstring .
    methods get_blob
      returning
        value(rv_blob) type xstring .
    methods read
      importing
        !iv_filename   type string
      returning
        value(rv_data) type string
      raising
        zcx_w3mime_error .
    methods readx
      importing
        !iv_filename    type string
      returning
        value(rv_xdata) type xstring
      raising
        zcx_w3mime_error .
    methods has
      importing
        !iv_filename type string
      returning
        value(r_yes) type abap_bool .
    methods is_dirty
      returning
        value(r_yes) type abap_bool .
    methods delete
      importing
        !iv_filename type string
      raising
        zcx_w3mime_error .
    methods list
      returning
        value(rt_list) type string_table.

  protected section.
  private section.

    data mv_is_dirty type abap_bool .
    data mo_zip type ref to cl_abap_zip .
    data mo_conv_out type ref to cl_abap_conv_out_ce .
    data mo_conv_in type ref to cl_abap_conv_in_ce .
    data mo_conv_in_utf8 type ref to cl_abap_conv_in_ce .
    data mo_conv_in_utf16 type ref to cl_abap_conv_in_ce .
    data mv_encoding type abap_encoding .

ENDCLASS.



CLASS ZCL_W3MIME_ZIP_WRITER IMPLEMENTATION.


  method add.

    data lv_xdata type xstring.

    mo_conv_out->convert(
      exporting
        data = iv_data
      importing
        buffer = lv_xdata ).

    addx(
      iv_filename = iv_filename
      iv_xdata    = lv_xdata ).

  endmethod.


  method addx.

    mo_zip->delete(
      exporting
        name = iv_filename
      exceptions
        others = 1 ). " ignore exceptions

    mo_zip->add( name = iv_filename content = iv_xdata ).
    mv_is_dirty = abap_true.

  endmethod.


  method constructor.

    if io_zip is bound.
      mo_zip = io_zip.
    else.
      create object mo_zip.
    endif.

    if iv_encoding is not initial.
      mv_encoding = iv_encoding.
    else.
      mv_encoding = '4110'. " UTF8
    endif.

    mo_conv_out = cl_abap_conv_out_ce=>create( encoding = mv_encoding ).
    mo_conv_in  = cl_abap_conv_in_ce=>create( encoding = mv_encoding ).
    mo_conv_in_utf8 = cl_abap_conv_in_ce=>create( encoding = '4110' ).
    mo_conv_in_utf16 = cl_abap_conv_in_ce=>create( encoding = '4103' ).

  endmethod.


  method delete.

    mo_zip->delete(
      exporting
        name = iv_filename
      exceptions
        others = 4 ).

    if sy-subrc is not initial.
      zcx_w3mime_error=>raise( 'delete failed' ).
    endif.
    mv_is_dirty = abap_true.

  endmethod.


  method get_blob.

    rv_blob = mo_zip->save( ).
    mv_is_dirty = abap_false.

  endmethod.


  method has.

    read table mo_zip->files with key name = iv_filename transporting no fields.
    r_yes = boolc( sy-subrc is initial ).

  endmethod.


  method is_dirty.
    r_yes = mv_is_dirty.
  endmethod.


  method list.

    field-symbols <f> like line of mo_zip->files.
    loop at mo_zip->files assigning <f>.
      append <f>-name to rt_list.
    endloop.

  endmethod.


  method read.
    data lv_xdata type xstring.
    data lx type ref to cx_root.
    data lo_conv type ref to cl_abap_conv_in_ce.

    lv_xdata = readx( iv_filename ).

    " Detect encoding
    data lv_byte_order_mark_utf8 like cl_abap_char_utilities=>byte_order_mark_utf8.
    data lv_byte_order_mark_little like cl_abap_char_utilities=>byte_order_mark_little.

    lv_byte_order_mark_utf8 = lv_xdata.
    if lv_byte_order_mark_utf8 = cl_abap_char_utilities=>byte_order_mark_utf8.
      lo_conv = mo_conv_in_utf8.
    else.
      lv_byte_order_mark_little = lv_xdata.
      if lv_byte_order_mark_little = cl_abap_char_utilities=>byte_order_mark_little.
        lo_conv = mo_conv_in_utf16.
      else.
        lo_conv = mo_conv_in.
      endif.
    endif.

    " Remove unicode signatures
    case lo_conv->encoding.
      when '4110'. " UTF-8
        shift lv_xdata left deleting leading cl_abap_char_utilities=>byte_order_mark_utf8 in byte mode.
      when '4103'. " UTF-16LE
        shift lv_xdata left deleting leading cl_abap_char_utilities=>byte_order_mark_little in byte mode.
    endcase.

    try.
      lo_conv->convert( exporting input = lv_xdata importing data = rv_data ).
    catch cx_root into lx.
      zcx_w3mime_error=>raise( msg = 'Codepage conversion error' ). "#EC NOTEXT
    endtry.

  endmethod.


  method readx.

    mo_zip->get(
      exporting
        name    = iv_filename
      importing
        content = rv_xdata
      exceptions
        zip_index_error = 1 ).

    if sy-subrc is not initial.
      zcx_w3mime_error=>raise( msg = |Cannot read { iv_filename }| ). "#EC NOTEXT
    endif.

  endmethod.
ENDCLASS.
