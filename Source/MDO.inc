{************************************************************}
{                                                            }
{                  Mercury Database Objects                  }
{                                                            }
{          Copyright(c) 2002-2005, The Mercury Team          }
{                  Contact: info@mdolib.org                  }
{                                                            }
{           Based on the FreeIBComponents written            }
{          by  Gregory H. Deatz - gdeatz@hlmdd.com           }
{           and InterBase Express 4.3 created by             }
{                    Inprise Corporation.                    }
{                                                            }
{************************************************************}



{$IFDEF FPC}         // Free Pascal
  {.$MODE DELPHI}
  {$H+}
  {$DEFINE MDO_FPC}
  {$DEFINE MDO_DELPHI6}
  {$DEFINE MDO_DELPHI6_UP}
  {$IFDEF CPU64}
    {$DEFINE MDO_64BIT}
  {$ENDIF}
  {$DEFINE MDO_NEW_BOOKMARK}
{$ENDIF}

{.$DEFINE MDO_DEBUG}

