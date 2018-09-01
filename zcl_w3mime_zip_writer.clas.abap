class ZCL_W3MIME_ZIP_WRITER definition
  public
  final
  create public .

public section.

  type-pools ABAP .
  methods CONSTRUCTOR
    importing
      !IO_ZIP type ref to CL_ABAP_ZIP optional
      !IV_ENCODING type ABAP_ENCODING optional .
  methods ADD
    importing
      !IV_FILENAME type STRING
      !IV_DATA type STRING .
  methods ADDX
    importing
      !IV_FILENAME type STRING
      !IV_XDATA type XSTRING .
  methods GET_BLOB
    returning
      value(RV_BLOB) type XSTRING .
  methods READ
    importing
      !IV_FILENAME type STRING
    returning
      value(RV_DATA) type STRING
    raising
      ZCX_W3MIME_ERROR .
  methods READX
    importing
      !IV_FILENAME type STRING
    returning
      value(RV_XDATA) type XSTRING
    raising
      ZCX_W3MIME_ERROR .
  methods HAS
    importing
      !IV_FILENAME type STRING
    returning
      value(R_YES) type ABAP_BOOL .
protected section.
private section.

  data MO_ZIP type ref to CL_ABAP_ZIP .
  data MO_CONV_OUT type ref to CL_ABAP_CONV_OUT_CE .
  data MO_CONV_IN type ref to CL_ABAP_CONV_IN_CE .
  type-pools ABAP .
  data MV_ENCODING type ABAP_ENCODING .
ENDCLASS.



CLASS ZCL_W3MIME_ZIP_WRITER IMPLEMENTATION.


method add.
  data lv_xdata type xstring.
  mo_conv_out->convert(
    exporting data = iv_data
    importing buffer = lv_xdata ).

  addx(
    iv_filename = iv_filename
    iv_xdata    = lv_xdata ).
endmethod.  " add.


method addx.
  mo_zip->delete(
    exporting
      name = iv_filename
    exceptions others = 1 ). " ignore exceptions

  mo_zip->add( name = iv_filename content = iv_xdata ).
endmethod.  " addx.


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
endmethod.  " constructor.


method get_blob.
  rv_blob = mo_zip->save( ).
endmethod.  " get_blob


method HAS.
  read table mo_zip->files with key name = iv_filename transporting no fields.
  r_yes = boolc( sy-subrc is initial ).
endmethod.


method READ.
  data:
        lv_xdata type xstring,
        lx       type ref to cx_root.

  lv_xdata = readx( iv_filename ).

  try.
    mo_conv_in->convert( exporting input = lv_xdata importing data = rv_data ).
  catch cx_root into lx.
    zcx_w3mime_error=>raise( msg = 'Codepage conversion error' ). "#EC NOTEXT
  endtry.

endmethod.


method READX.

  mo_zip->get(
    exporting
      name    = iv_filename
    importing
      content = rv_xdata
    exceptions zip_index_error = 1 ).

  if sy-subrc is not initial.
    zcx_w3mime_error=>raise( msg = |Cannot read { iv_filename }| ). "#EC NOTEXT
  endif.

  " Remove unicode signatures
  case mv_encoding.
    when '4110'. " UTF-8
      shift rv_xdata left deleting leading  cl_abap_char_utilities=>byte_order_mark_utf8 in byte mode.
    when '4103'. " UTF-16LE
      shift rv_xdata left deleting leading  cl_abap_char_utilities=>byte_order_mark_little in byte mode.
  endcase.

endmethod.
ENDCLASS.
