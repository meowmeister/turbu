unit test_canvas;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, sdl_frame;

type
  TfrmTesting = class(TForm)
    SdlFrame1: TSdlFrame;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmTesting: TfrmTesting;

implementation

{$R *.dfm}

end.
