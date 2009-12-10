{************************************************************************}
{                                                                        }
{       Borland Delphi Visual Component Library                          }
{       InterBase Express core components                                }
{                                                                        }
{       Copyright (c) 1998-2000 Inprise Corporation                      }
{                                                                        }
{    InterBase Express is based in part on the product                   }
{    Free IB Components, written by Gregory H. Deatz for                 }
{    Hoagland, Longo, Moran, Dunst & Doukas Company.                     }
{    Free IB Components is used under license.                           }
{                                                                        }
{    The contents of this file are subject to the InterBase              }
{    Public License Version 1.0 (the "License"); you may not             }
{    use this file except in compliance with the License. You            }
{    may obtain a copy of the License at http://www.Inprise.com/IPL.html }
{    Software distributed under the License is distributed on            }
{    an "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either              }
{    express or implied. See the License for the specific language       }
{    governing rights and limitations under the License.                 }
{    The Original Code was created by InterBase Software Corporation     }
{       and its successors.                                              }
{    Portions created by Inprise Corporation are Copyright (C) Inprise   }
{       Corporation. All Rights Reserved.                                }
{    Contributor(s): Jeff Overcash                                       }
{                    Mercury Database Objects [info@mdolib.com]          }
{                                                                        }
{************************************************************************}

unit MDOEventsEditor;

{$I ..\MDO.inc}

interface

uses
  {$IFDEF MDO_FPC}
  LResources, LCLType,
  {$ELSE}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls,
  Grids, MDOEvents;

type
  TMDOEventsEditorForm = class (TForm)
    bCancel: TButton;
    bOK: TButton;
    cEvents: TStringGrid;
    Panel1: TPanel;
    Panel2: TPanel;
    RequestedEvents: TLabel;
    procedure FormCreate(Sender: TObject);
  end;
  
function EditAlerterEvents(Events: TStrings): Boolean;

implementation

{$IFNDEF MDO_FPC}
{$R *.DFM}
{$ENDIF}

function EditAlerterEvents(Events: TStrings): Boolean;
var
  i: integer;
begin
  result := false;
  with TMDOEventsEditorForm.Create(Application) do
  begin
    try
      for i := 0 to Events.Count - 1 do
        cEvents.Cells[1, i] := Events[i];
      if ShowModal = idOk then
      begin
        result := true;
        Events.Clear;
        for i := 0 to MaxEvents - 1 do
          if length(cEvents.Cells[1, i]) <> 0 then
            Events.Add(cEvents.Cells[1, i]);
      end;
    finally
      Free;
    end;
  end;
end;

{
***************************** TMDOEventsEditorForm *****************************
}
procedure TMDOEventsEditorForm.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  for i := 1 to MaxEvents do
    cEvents.Cells[0, i - 1] := IntToStr(i);
end;

{$IFDEF MDO_FPC}
initialization
  {$I MDOEventsEditor.lrs}
{$ENDIF}

end.
