unit rxsortmdoreg;

{$mode objfpc}{$H+}

interface

procedure Register;

implementation

uses
  Classes, RxSortMDO;

procedure Register;
begin
  RegisterComponents('RX DBAware', [TRxSortMDO]);
end;

end.

