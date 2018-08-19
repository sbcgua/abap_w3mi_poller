class lcl_error_test definition final for testing
  duration short
  risk level harmless.

  private section.
    methods raise for testing.
endclass.

class lcl_error_test implementation.

  method raise.
    data lx type ref to cx_root.

    try.
      zcx_w3mime_error=>raise( 'Crash!' ).
    catch cx_root into lx.
      " skip
    endtry.

    cl_abap_unit_assert=>assert_not_initial( lx ).
    cl_abap_unit_assert=>assert_equals( act = lx->get_text( ) exp = 'Crash!' ).

  endmethod.  " raise.

endclass.
