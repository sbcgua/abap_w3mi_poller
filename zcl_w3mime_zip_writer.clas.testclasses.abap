class lcl_zip_writer_test definition final for testing
  duration short
  risk level harmless.

  private section.
    methods zip for testing.
    methods zip_overwrite for testing.

endclass.

class lcl_zip_writer_test implementation.

  method zip.
    data lo_zipw type ref to zcl_w3mime_zip_writer.
    data lo_zip type ref to cl_abap_zip.
    data lv_xdata type xstring.
    data lv_data type string.

    create object lo_zipw.
    lo_zipw->add( iv_filename = 'dir/file.txt' iv_data = 'Hello' ).
    lv_xdata = lo_zipw->get_blob( ).

    create object lo_zip.
    lo_zip->load( zip = lv_xdata ).
    cl_abap_unit_assert=>assert_equals( act = lines( lo_zip->files ) exp = 1 ).

    lo_zip->get( exporting  name = 'dir/file.txt' importing  content = lv_xdata ).
    data lo_conv type ref to cl_abap_conv_in_ce.
    lo_conv = cl_abap_conv_in_ce=>create( encoding = '4110' ).
    lo_conv->convert( exporting input = lv_xdata importing data = lv_data ).

    cl_abap_unit_assert=>assert_equals( act = lv_data exp = 'Hello' ).

  endmethod.  " zip.

  method zip_overwrite.
    data lo_zipw type ref to zcl_w3mime_zip_writer.
    data lo_zip type ref to cl_abap_zip.
    data lv_xdata type xstring.
    data lv_data type string.

    create object lo_zipw.
    lo_zipw->add( iv_filename = 'dir/file.txt' iv_data = 'Hello' ).
    lv_xdata = lo_zipw->get_blob( ).
    cl_abap_unit_assert=>assert_false( lo_zipw->is_dirty( ) ).

    create object lo_zip.
    lo_zip->load( zip = lv_xdata ).
    cl_abap_unit_assert=>assert_equals( act = lines( lo_zip->files ) exp = 1 ).

    lo_zip->get( exporting  name = 'dir/file.txt' importing  content = lv_xdata ).
    data lo_conv type ref to cl_abap_conv_in_ce.
    lo_conv = cl_abap_conv_in_ce=>create( encoding = '4110' ).
    lo_conv->convert( exporting input = lv_xdata importing data = lv_data ).

    cl_abap_unit_assert=>assert_equals( act = lv_data exp = 'Hello' ).

    lo_zipw->add( iv_filename = 'dir/file.txt' iv_data = 'Hello2' ).
    cl_abap_unit_assert=>assert_true( lo_zipw->is_dirty( ) ).
    lv_xdata = lo_zipw->get_blob( ).
    cl_abap_unit_assert=>assert_false( lo_zipw->is_dirty( ) ).
    lo_zip->load( zip = lv_xdata ).
    cl_abap_unit_assert=>assert_equals( act = lines( lo_zip->files ) exp = 1 ).
    lo_zip->get( exporting  name = 'dir/file.txt' importing  content = lv_xdata ).
    lo_conv->convert( exporting input = lv_xdata importing data = lv_data ).
    cl_abap_unit_assert=>assert_equals( act = lv_data exp = 'Hello2' ).

  endmethod.  " zip_overwrite

endclass.
