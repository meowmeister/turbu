unit conversion_output;

interface

uses
  Classes, Controls, Forms, ComCtrls, StdCtrls;

type
  TfrmConversionOutput = class(TForm)
    ListBox1: TListBox;
  private
    { Private declarations }
  public
    { Public declarations }
    procedure AddItem(const name: string; group: integer);
    procedure Dump(const filename: string);
  end;

implementation
{$R *.dfm}

uses commons;

{ TfrmConversionOutput }

procedure TfrmConversionOutput.AddItem(const name: string; group: integer);
var
   closure: TThreadProcedure;
begin
   closure :=
      procedure
      begin
         ListBox1.AddItem(name, pointer(group));
         ListBox1.ClearSelection;
         ListBox1.Selected[listBox1.Count - 1] := true;
         if not self.Visible then
            self.Show;
      end;
   runThreadsafe(closure);
end;

procedure TfrmConversionOutput.Dump(const filename: string);
begin
   listbox1.Items.SaveToFile(filename);
end;

end.
