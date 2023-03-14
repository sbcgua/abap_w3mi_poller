class ZCL_W3MIME_VALIDATOR definition
  public
  final
  create public .

  public section.

    class-methods validate_params
      importing
        iv_filename        type string
        is_w3key           type zif_w3mime=>ty_w3obj_key
        iv_skip_file_check type abap_bool default abap_false
      raising
        zcx_w3mime_error.

  protected section.
  private section.
ENDCLASS.



CLASS ZCL_W3MIME_VALIDATOR IMPLEMENTATION.


  method validate_params.

    if iv_skip_file_check is initial
       and abap_false = cl_gui_frontend_services=>file_exist( iv_filename ).
      zcx_w3mime_error=>raise( 'File does not exist' ). "#EC NOTEXT
    endif.

    if abap_false = zcl_w3mime_storage=>check_obj_exists( iv_type = is_w3key-relid iv_key = is_w3key-objid ).
      zcx_w3mime_error=>raise( 'MIME object does not exist' ). "#EC NOTEXT
    endif.

  endmethod.
ENDCLASS.
