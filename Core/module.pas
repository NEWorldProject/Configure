unit Module;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TModule = class(TObject)
  public
    Id: UTF8String;
    Uri: UTF8String;
    FancyName: UTF8String;
    Dependencies: array of UTF8String;
  end;

implementation

end.

