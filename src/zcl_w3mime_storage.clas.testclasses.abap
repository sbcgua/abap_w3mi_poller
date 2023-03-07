class lcl_w3s_test definition final for testing
  duration short
  risk level harmless.

  private section.
    methods read_object_x for testing.
endclass.

class lcl_w3s_test implementation.

  method read_object_x.
    data lv_xdata type xstring.
    data lv_data type string.

    try.
      lv_xdata = zcl_w3mime_storage=>read_object_x( iv_key = 'ZMIME_POLLER_TEST' ).
    catch cx_root.
      cl_abap_unit_assert=>fail( 'Unexpected error' ).
    endtry.

    data lo_conv type ref to cl_abap_conv_in_ce.
    lo_conv = cl_abap_conv_in_ce=>create( encoding = '4110' ).
    lo_conv->convert( exporting input = lv_xdata importing data = lv_data ).
    cl_abap_unit_assert=>assert_equals( act = lv_data exp = 'MIME poller test file' ).

  endmethod.

endclass.
