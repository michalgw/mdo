program tablequery;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, Unit1, fMDODatabaseEdit, LResources, mdolaz_dsg
  { you can add units after this };

{$IFDEF WINDOWS}{$R tablequery.rc}{$ENDIF}

begin
  {$I tablequery.lrs}
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

