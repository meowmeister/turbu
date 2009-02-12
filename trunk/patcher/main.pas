unit main;

interface

uses
   SysUtils, Classes, Controls, Forms, StdCtrls, ExtDlgs, Dialogs, ActnList,
  Menus;

type
   TfrmPatcher = class(TForm)
      btnLoadFile: TButton;
      txtFile: TMemo;
      txtPatch: TMemo;
      btnLoadPatch: TButton;
      dlgLoadFile: TOpenTextFileDialog;
      dlgLoadPatch: TOpenTextFileDialog;
      btnRun: TButton;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Help1: TMenuItem;
    LoadFile1: TMenuItem;
    LoadPatch1: TMenuItem;
    SaveFile1: TMenuItem;
    SaveFileAs1: TMenuItem;
    ActionList1: TActionList;
    actLoadFile: TAction;
    actLoadPatch: TAction;
    AboutTURBUPatcher1: TMenuItem;
      procedure btnLoadFileClick(Sender: TObject);
      procedure btnLoadPatchClick(Sender: TObject);
      procedure btnRunClick(Sender: TObject);
    procedure AboutTURBUPatcher1Click(Sender: TObject);
   private
      { Private declarations }
      FFileLoaded, FPatchLoaded: boolean;
      FPatching: boolean;
      FInLength, FOutLength, FInPos, FOutPos: integer;
      FDifference: integer;
      procedure SetFileLoaded(const Value: boolean);
      procedure SetPatchLoaded(const Value: boolean);
      procedure Reset;
      procedure doPatch;
      function trySave: boolean;

      property fileLoaded: boolean read FFileLoaded write SetFileLoaded;
      property patchLoaded: boolean read FPatchLoaded write SetPatchLoaded;
   public
      { Public declarations }
   end;

var
   frmPatcher: TfrmPatcher;

implementation
uses
   strtok,
   about, filesave;

{$R *.dfm}

type
   TLineType = (ltPlus, ltMinus, ltAt, ltData);

{$WARN NO_RETVAL OFF}
function lineType(data: string): TLineType;
var
   header: char;
begin
   header := data[1];
   if header = '+' then
      result := ltPlus
   else if header = '-' then
      result := ltMinus
   else if header = '@' then
      result := ltAt
   else if header = ' ' then
      result := ltData
   else assert(false);
end;
{$WARN NO_RETVAL ON}

procedure TfrmPatcher.AboutTURBUPatcher1Click(Sender: TObject);
begin
   AboutBox.ShowModal;
end;

procedure TfrmPatcher.btnLoadFileClick(Sender: TObject);
begin
   if dlgLoadFile.Execute then
   begin
      txtFile.Lines.LoadFromFile(dlgLoadFile.FileName);
      fileLoaded := true;
      Reset;
   end;
end;

procedure TfrmPatcher.btnLoadPatchClick(Sender: TObject);
begin
   if dlgLoadPatch.Execute then
   begin
      txtPatch.Lines.LoadFromFile(dlgLoadPatch.FileName);
      patchLoaded := true;
      Reset;
   end;
end;

procedure TfrmPatcher.btnRunClick(Sender: TObject);
begin
   try
      doPatch;
   except
      on EAssertionFailed do
         Application.MessageBox('The patch is invalid, or does not match the file to be patched', 'Unable to complete patching.');
      else raise;
   end;
end;

procedure TfrmPatcher.doPatch;

   function verifyPatchHeader(patch: TStrings): boolean;
   begin
      result := false;
      if patch.Count <= 2 then
         Exit;
      if (lineType(patch[0]) <> ltMinus) then
         Exit;
      if (lineType(patch[1]) <> ltPlus) then
         Exit;
      if (lineType(patch[2]) <> ltAt) then
         Exit;
      patch.Delete(0);
      patch.Delete(0);
      result := true;
   end;

   procedure parseAtLine(line: string);
   var
      pos: integer;
   begin
      pos := 3;
      FInPos := abs(strToInt(strtok.GetNextToken(line, ',', pos))) - 1;
      FInLength := StrToInt(strtok.GetNextToken(line, ' ', pos));
      assert(line[pos] = '+');
      inc(pos);
      FOutPos := strToInt(strtok.GetNextToken(line, ',', pos)) - 1;
      FOutLength := strToInt(strtok.GetNextToken(line, ' ', pos));
      FPatching := true;
   end;

var
   line: string;
begin
   if not verifyPatchHeader(txtPatch.Lines) then
   begin
      assert(false);
   end;

   for line in txtPatch.Lines do
   begin
      if not FPatching then
      begin
         if lineType(line) <> ltAt then
            Exit
         else
            parseAtLine(line)
      end else begin
         assert(FOutPos - FInPos = FDifference); //sanity check
         case lineType(line) of
            ltPlus:
            begin
               txtFile.Lines.Insert(FOutPos, copy(line, 2, MAXINT));
               inc(FDifference);
               inc(FOutPos);
               dec(FOutLength);
            end;
            ltMinus:
            begin
               assert(txtFile.Lines[FOutPos] = copy(line, 2, MAXINT));
               txtFile.Lines.Delete(FOutPos);
               dec(FDifference);
               dec(FInLength);
               inc(FInPos);
            end;
            ltData:
            begin
               assert(txtFile.Lines[FOutPos] = copy(line, 2, MAXINT));
               dec(FInLength);
               dec(FOutLength);
               inc(FInPos);
               inc(FOutPos);
            end;
            ltAt: assert(false);
         end;

         if (FInLength = 0) and (FOutLength = 0) then
            FPatching := false;
      end;
   end;

   if trySave then //fill in conditions here
      else ;
end;

procedure TfrmPatcher.Reset;
begin
   FPatching := false;
   FInLength := 0;
   FOutLength := 0;
   FInPos := 0;
   FOutPos := 0;
   FDifference := 0;
end;

procedure TfrmPatcher.SetFileLoaded(const Value: boolean);
begin
   FFileLoaded := Value;
   btnRun.enabled := FFileLoaded and FPatchLoaded;
end;

procedure TfrmPatcher.SetPatchLoaded(const Value: boolean);
begin
   FPatchLoaded := Value;
   btnRun.enabled := FFileLoaded and FPatchLoaded;
end;

function TfrmPatcher.trySave: boolean;
var
   filename: string;
begin
   filename := dlgLoadFile.FileName;
   result := dlgSave.Run(filename);
   if result then
      txtFile.Lines.SaveToFile(filename);
end;

end.
