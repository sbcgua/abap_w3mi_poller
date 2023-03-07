class ltcl_zip_writer_test definition final for testing
  duration short
  risk level harmless.

  private section.
    methods zip for testing.
    methods zip_overwrite for testing.
    methods list for testing.
    methods encodings for testing raising zcx_w3mime_error.

endclass.

class ltcl_zip_writer_test implementation.

  method zip.

    data lo_zipw type ref to zcl_w3mime_zip_writer.
    data lo_zip type ref to cl_abap_zip.
    data lv_xdata type xstring.
    data lv_data type string.
    data lo_conv type ref to cl_abap_conv_in_ce.

    create object lo_zipw.
    lo_zipw->add(
      iv_filename = 'dir/file.txt'
      iv_data = 'Hello' ).
    lv_xdata = lo_zipw->get_blob( ).

    create object lo_zip.
    lo_zip->load( zip = lv_xdata ).
    cl_abap_unit_assert=>assert_equals(
      act = lines( lo_zip->files )
      exp = 1 ).

    lo_zip->get(
      exporting
        name = 'dir/file.txt'
      importing
        content = lv_xdata ).

    lo_conv = cl_abap_conv_in_ce=>create( encoding = '4110' ).
    lo_conv->convert(
      exporting
        input = lv_xdata
      importing
        data = lv_data ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_data
      exp = 'Hello' ).

  endmethod.

  method zip_overwrite.

    data lo_zipw type ref to zcl_w3mime_zip_writer.
    data lo_zip type ref to cl_abap_zip.
    data lv_xdata type xstring.
    data lv_data type string.
    data lo_conv type ref to cl_abap_conv_in_ce.

    create object lo_zipw.
    lo_zipw->add(
      iv_filename = 'dir/file.txt'
      iv_data = 'Hello' ).
    lv_xdata = lo_zipw->get_blob( ).
    cl_abap_unit_assert=>assert_false( lo_zipw->is_dirty( ) ).

    create object lo_zip.
    lo_zip->load( zip = lv_xdata ).
    cl_abap_unit_assert=>assert_equals(
      act = lines( lo_zip->files )
      exp = 1 ).

    lo_zip->get(
      exporting
        name = 'dir/file.txt'
      importing
        content = lv_xdata ).

    lo_conv = cl_abap_conv_in_ce=>create( encoding = '4110' ).
    lo_conv->convert(
      exporting
        input = lv_xdata
      importing
        data = lv_data ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_data
      exp = 'Hello' ).

    lo_zipw->add(
      iv_filename = 'dir/file.txt'
      iv_data = 'Hello2' ).
    cl_abap_unit_assert=>assert_true( lo_zipw->is_dirty( ) ).
    lv_xdata = lo_zipw->get_blob( ).
    cl_abap_unit_assert=>assert_false( lo_zipw->is_dirty( ) ).
    lo_zip->load( zip = lv_xdata ).
    cl_abap_unit_assert=>assert_equals(
      act = lines( lo_zip->files )
      exp = 1 ).
    lo_zip->get(
      exporting
        name = 'dir/file.txt'
      importing
        content = lv_xdata ).
    lo_conv->convert(
      exporting
        input = lv_xdata
      importing
        data = lv_data ).
    cl_abap_unit_assert=>assert_equals( act = lv_data exp = 'Hello2' ).

  endmethod.

  method list.

    data lo_zipw type ref to zcl_w3mime_zip_writer.
    data lt_files_act type string_table.
    data lt_files_exp type string_table.

    create object lo_zipw.
    lo_zipw->add( iv_filename = 'dir/file.txt' iv_data = 'Hello' ).

    lt_files_act = lo_zipw->list( ).

    append 'dir/file.txt' to lt_files_exp.

    cl_abap_unit_assert=>assert_equals(
      act = lt_files_act
      exp = lt_files_exp ).

  endmethod.

  method encodings.

    data lo_zipw type ref to zcl_w3mime_zip_writer.
    " data lo_conv type ref to cl_abap_conv_out_ce.
    data lv_xdata type xstring.

    create object lo_zipw
      exporting
        iv_encoding = '1100'.

    cl_abap_conv_out_ce=>create( encoding = '4110' )->convert( " UTF8
      exporting
        data = 'Hello'
      importing
        buffer = lv_xdata ).
    concatenate cl_abap_char_utilities=>byte_order_mark_utf8 lv_xdata into lv_xdata in byte mode.
    lo_zipw->addx(
      iv_filename = 'dir/utf8.txt'
      iv_xdata = lv_xdata ).

    cl_abap_conv_out_ce=>create( encoding = '4103' )->convert( " UTF16
      exporting
        data = 'Hello'
      importing
        buffer = lv_xdata ).
    concatenate cl_abap_char_utilities=>byte_order_mark_little lv_xdata into lv_xdata in byte mode.
    lo_zipw->addx(
      iv_filename = 'dir/utf16.txt'
      iv_xdata = lv_xdata ).

    cl_abap_conv_out_ce=>create( encoding = '1100' )->convert( " 8859-1
      exporting
        data = 'Hello'
      importing
        buffer = lv_xdata ).

    lo_zipw->addx(
      iv_filename = 'dir/plain.txt'
      iv_xdata = lv_xdata ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_zipw->read( 'dir/plain.txt' )
      exp = 'Hello' ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_zipw->read( 'dir/utf8.txt' )
      exp = 'Hello' ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_zipw->read( 'dir/utf16.txt' )
      exp = 'Hello' ).

  endmethod.

endclass.
