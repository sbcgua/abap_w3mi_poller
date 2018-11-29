class ZCX_W3MIME_ERROR definition
  public
  inheriting from CX_STATIC_CHECK
  final
  create public .

public section.

  interfaces IF_T100_MESSAGE .

  constants:
    begin of ZCX_W3MIME_ERROR,
      msgid type symsgid value 'SY',
      msgno type symsgno value '499',
      attr1 type scx_attrname value 'MSG',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_W3MIME_ERROR .
  data MSG type STRING read-only .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !MSG type STRING optional .
  class-methods RAISE
    importing
      !MSG type STRING
    raising
      ZCX_W3MIME_ERROR .
protected section.
private section.
ENDCLASS.



CLASS ZCX_W3MIME_ERROR IMPLEMENTATION.


method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
me->MSG = MSG .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = ZCX_W3MIME_ERROR .
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
endmethod.


method raise.
  raise exception type zcx_w3mime_error
    exporting
      textid = zcx_w3mime_error
      msg    = msg.
endmethod.
ENDCLASS.
