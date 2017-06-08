unit RxSortMDO;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs;

type
  TRxSortMDO = class(TComponent)
  private
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
  published
    { Published declarations }
  end;

procedure Register;

implementation
uses exsortmdo;

procedure Register;
begin
  RegisterComponents('RX DBAware',[TRxSortMDO]);
end;

end.
