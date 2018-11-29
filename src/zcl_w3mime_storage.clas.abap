class ZCL_W3MIME_STORAGE definition
  public
  final
  create public .

public section.

  class-methods CHECK_OBJ_EXISTS
    importing
      !IV_KEY type WWWDATA-OBJID
      !IV_TYPE type WWWDATA-RELID default 'MI'
    returning
      value(RV_YES) type ABAP_BOOL .
  class-methods READ_OBJECT
    importing
      !IV_KEY type WWWDATA-OBJID
      !IV_TYPE type WWWDATA-RELID default 'MI'
    exporting
      !ET_DATA type LVC_T_MIME
      !EV_SIZE type I
    raising
      ZCX_W3MIME_ERROR .
  class-methods UPDATE_OBJECT
    importing
      !IV_KEY type WWWDATA-OBJID
      !IV_TYPE type WWWDATA-RELID default 'MI'
      !IT_DATA type LVC_T_MIME
      !IV_SIZE type I
    raising
      ZCX_W3MIME_ERROR .
  class-methods GET_OBJECT_INFO
    importing
      !IV_KEY type WWWDATA-OBJID
      !IV_TYPE type WWWDATA-RELID default 'MI'
    returning
      value(RS_OBJECT) type WWWDATATAB
    raising
      ZCX_W3MIME_ERROR .
  class-methods READ_OBJECT_X
    importing
      !IV_KEY type WWWDATA-OBJID
      !IV_TYPE type WWWDATA-RELID default 'MI'
    returning
      value(RV_DATA) type XSTRING
    raising
      ZCX_W3MIME_ERROR .
  class-methods UPDATE_OBJECT_X
    importing
      !IV_KEY type WWWDATA-OBJID
      !IV_TYPE type WWWDATA-RELID default 'MI'
      !IV_DATA type XSTRING
    raising
      ZCX_W3MIME_ERROR .
  class-methods CHOOSE_MIME_DIALOG
    returning
      value(RV_OBJ_NAME) type SHVALUE_D .
protected section.
private section.
ENDCLASS.



CLASS ZCL_W3MIME_STORAGE IMPLEMENTATION.


method check_obj_exists.

  data dummy type wwwdata-relid.

  select single relid into dummy
    from wwwdata
    where relid = iv_type
    and   objid = iv_key
    and   srtf2 = 0.

  rv_yes = boolc( sy-subrc = 0 ).

endmethod.  " check_obj_exists.


method choose_mime_dialog.

  types:
    begin of t_w3head,
      objid type wwwdata-objid,
      text  type wwwdata-text,
    end of t_w3head.

  data:
        ls_return type ddshretval,
        lt_data   type standard table of t_w3head,
        lt_return type standard table of ddshretval.

  select distinct objid text from wwwdata
    into corresponding fields of table lt_data
    where relid = 'MI'
    and   objid like 'Z%'
    order by objid.

  call function 'F4IF_INT_TABLE_VALUE_REQUEST'
    exporting
      retfield        = 'OBJID'
      value_org       = 'S'
    tables
      value_tab       = lt_data
      return_tab      = lt_return
    exceptions
      parameter_error = 1
      no_values_found = 2
      others          = 3.

  if sy-subrc is not initial.
    return. " Empty value
  endif.

  read table lt_return into ls_return index 1. " fail is ok => empty return
  rv_obj_name = ls_return-fieldval.

endmethod.


method get_object_info.

  select single * into corresponding fields of rs_object
    from wwwdata
    where relid = iv_type
    and   objid = iv_key
    and   srtf2 = 0.

  if sy-subrc > 0.
    zcx_w3mime_error=>raise( 'Cannot read W3xx info' ). "#EC NOTEXT
  endif.

endmethod.  " get_object_info.


method read_object.

  data: lv_value  type w3_qvalue,
        ls_object type wwwdatatab.

  clear: et_data, ev_size.

  call function 'WWWPARAMS_READ'
    exporting
      relid = iv_type
      objid = iv_key
      name  = 'filesize'
    importing
      value = lv_value
    exceptions
      others = 1.

  if sy-subrc > 0.
    zcx_w3mime_error=>raise( 'Cannot read W3xx filesize parameter' ). "#EC NOTEXT
  endif.

  ev_size         = lv_value.
  ls_object-relid = iv_type.
  ls_object-objid = iv_key.

  call function 'WWWDATA_IMPORT'
    exporting
      key               = ls_object
    tables
      mime              = et_data
    exceptions
      wrong_object_type = 1
      import_error      = 2.

  if sy-subrc > 0.
    zcx_w3mime_error=>raise( 'Cannot upload W3xx data' ). "#EC NOTEXT
  endif.

endmethod.  " read_object.


method read_object_x.
  data:
        lt_data type lvc_t_mime,
        lv_size type i.

  read_object(
    exporting
      iv_key  = iv_key
      iv_type = iv_type
    importing
      et_data = lt_data
      ev_size = lv_size ).

  call function 'SCMS_BINARY_TO_XSTRING'
    exporting
      input_length = lv_size
    importing
      buffer       = rv_data
    tables
      binary_tab   = lt_data.

endmethod.  " read_object_x.


method update_object.

  data: ls_param  type wwwparams,
        ls_object type wwwdatatab.

  ls_param-relid = iv_type.
  ls_param-objid = iv_key.
  ls_param-name  = 'filesize'.
  ls_param-value = iv_size.
  condense ls_param-value.

  call function 'WWWPARAMS_MODIFY_SINGLE'
    exporting
      params = ls_param
    exceptions
      others = 1.

  if sy-subrc > 0.
    zcx_w3mime_error=>raise( 'Cannot upload W3xx data' ). "#EC NOTEXT
  endif.

  ls_object = get_object_info( iv_key = iv_key iv_type = iv_type ).
  ls_object-chname = sy-uname.
  ls_object-tdate  = sy-datum.
  ls_object-ttime  = sy-uzeit.

  call function 'WWWDATA_EXPORT'
    exporting
      key               = ls_object
    tables
      mime              = it_data
    exceptions
      wrong_object_type = 1
      export_error      = 2.

  if sy-subrc > 0.
    zcx_w3mime_error=>raise( 'Cannot upload W3xx data' ). "#EC NOTEXT
  endif.

endmethod.  " update_object.


method update_object_x.
  data:
    lt_data type lvc_t_mime,
    lv_size type i.

  call function 'SCMS_XSTRING_TO_BINARY'
    exporting
      buffer        = iv_data
    importing
      output_length = lv_size
    tables
      binary_tab    = lt_data.

  update_object(
      iv_key  = iv_key
      iv_type = iv_type
      iv_size = lv_size
      it_data = lt_data ).

endmethod.  " update_object_x.
ENDCLASS.
