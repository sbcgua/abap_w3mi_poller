class lcl_fs_test definition final for testing
  duration short
  risk level harmless.

  private section.
    methods parse_path for testing.
    methods resolve_filename for testing.
    methods join_path for testing.
endclass.

class lcl_fs_test implementation.

  method parse_path.
    data:
          lv_filename type string,
          lv_directory type string,
          lv_extension type string.

    zcl_w3mime_fs=>parse_path(
      exporting
        iv_path = 'c:\tmp\test.txt'
      importing
        ev_directory = lv_directory
        ev_filename  = lv_filename
        ev_extension = lv_extension ).

    cl_abap_unit_assert=>assert_equals( act = lv_directory exp = 'c:\tmp\' ).
    cl_abap_unit_assert=>assert_equals( act = lv_filename  exp = 'test' ).
    cl_abap_unit_assert=>assert_equals( act = lv_extension exp = '.txt' ).

    zcl_w3mime_fs=>parse_path(
      exporting
        iv_path = 'c:\tmp\'
      importing
        ev_directory = lv_directory
        ev_filename  = lv_filename
        ev_extension = lv_extension ).

    cl_abap_unit_assert=>assert_equals( act = lv_directory exp = 'c:\tmp\' ).
    cl_abap_unit_assert=>assert_equals( act = lv_filename  exp = '' ).
    cl_abap_unit_assert=>assert_equals( act = lv_extension exp = '' ).

    zcl_w3mime_fs=>parse_path(
      exporting
        iv_path = 'c:\tmp\test'
      importing
        ev_directory = lv_directory
        ev_filename  = lv_filename
        ev_extension = lv_extension ).

    cl_abap_unit_assert=>assert_equals( act = lv_directory exp = 'c:\tmp\' ).
    cl_abap_unit_assert=>assert_equals( act = lv_filename  exp = 'test' ).
    cl_abap_unit_assert=>assert_equals( act = lv_extension exp = '' ).

    zcl_w3mime_fs=>parse_path(
      exporting
        iv_path = 'test.txt'
      importing
        ev_directory = lv_directory
        ev_filename  = lv_filename
        ev_extension = lv_extension ).

    cl_abap_unit_assert=>assert_equals( act = lv_directory exp = '' ).
    cl_abap_unit_assert=>assert_equals( act = lv_filename  exp = 'test' ).
    cl_abap_unit_assert=>assert_equals( act = lv_extension exp = '.txt' ).

  endmethod.  " parse_path.

  method resolve_filename.
    data:
          lv_filename type string,
          lv_directory type string.

    try.
      zcl_w3mime_fs=>resolve_filename(
        exporting
          iv_path = 'c:\tmp\test.txt'
        importing
          ev_directory = lv_directory
          ev_filename  = lv_filename ).
      cl_abap_unit_assert=>assert_equals( act = lv_directory exp = 'c:\tmp\' ).
      cl_abap_unit_assert=>assert_equals( act = lv_filename  exp = 'test.txt' ).

      zcl_w3mime_fs=>resolve_filename(
        exporting
          iv_path = 'test.txt'
        importing
          ev_directory = lv_directory
          ev_filename  = lv_filename ).
      cl_abap_unit_assert=>assert_not_initial( act = lv_directory ).
      cl_abap_unit_assert=>assert_equals( act = lv_filename  exp = 'test.txt' ).

    catch cx_root.
      cl_abap_unit_assert=>fail( 'Unexpected error' ).
    endtry.

  endmethod. " resolve_filename.

  method join_path.

    data lv_act type string.

    lv_act = zcl_w3mime_fs=>join_path( iv_p1 = 'c:\tmp' iv_p2 = 'test' ).

    cl_abap_unit_assert=>assert_equals( act = lv_act  exp = 'c:\tmp\test' ).

  endmethod.  " join_path.

endclass.
