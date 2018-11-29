class ZCL_W3MIME_UTILS definition
  public
  final
  create public .

public section.

  class-methods upload
    importing
      iv_filename type string
      iv_key  type wwwdata-objid
      iv_type type wwwdata-relid default 'MI'
    raising zcx_w3mime_error.

  class-methods download
    importing
      iv_filename type string
      iv_key  type wwwdata-objid
      iv_type type wwwdata-relid default 'MI'
    raising zcx_w3mime_error.

protected section.
private section.
ENDCLASS.



CLASS ZCL_W3MIME_UTILS IMPLEMENTATION.


method download.

  data: lt_data type lvc_t_mime,
        lv_size type i.

  if abap_false = zcl_w3mime_storage=>check_obj_exists( iv_type = iv_type iv_key = iv_key ).
    zcx_w3mime_error=>raise( 'MIME object does not exist' ). "#EC NOTEXT
  endif.

  zcl_w3mime_storage=>read_object(
    exporting
      iv_type  = iv_type
      iv_key   = iv_key
    importing
      ev_size  = lv_size
      et_data  = lt_data ).

  zcl_w3mime_fs=>write_file(
    exporting
      iv_filename = iv_filename
      iv_size     = lv_size
    changing
      ct_data     = lt_data ).

endmethod.  " download.


method upload.

  data: lt_data type lvc_t_mime,
        lv_size type i.

  if abap_false = zcl_w3mime_storage=>check_obj_exists( iv_type = iv_type iv_key = iv_key ).
    zcx_w3mime_error=>raise( 'MIME object does not exist' ). "#EC NOTEXT
  endif.

  zcl_w3mime_fs=>read_file(
    exporting
      iv_filename = iv_filename
    importing
      et_data     = lt_data
      ev_size     = lv_size ).

  zcl_w3mime_storage=>update_object(
    iv_type  = iv_type
    iv_key   = iv_key
    it_data  = lt_data
    iv_size  = lv_size ).

endmethod.  " upload.
ENDCLASS.
