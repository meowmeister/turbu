unit test_project;

interface

uses
   SysUtils, Classes, Controls, Forms, StdCtrls, Mask, JvExMask, JvToolEdit;

type
   TfrmTestProjLocation = class(TForm)
      lblLocation: TLabel;
      dirProjectLocation: TJvDirectoryEdit;
      dirOutput: TJvDirectoryEdit;
      lblConvertLocation: TLabel;
      btnCancel: TButton;
      btnOK: TButton;
      procedure FormCreate(Sender: TObject);
      procedure dirProjectLocationChange(Sender: TObject);
      procedure btnOKClick(Sender: TObject);
   private
      { Private declarations }
      function validateDirectories: boolean;
   public
      { Public declarations }
      procedure getLocation(var data: string);
   end;

var
   frmTestProjLocation: TfrmTestProjLocation;

implementation

uses
   commons;

{$R *.dfm}

procedure TfrmTestProjLocation.btnOKClick(Sender: TObject);
begin
   SetRegistryValue('\Software\TURBU', 'TURBU Test Project', dirProjectLocation.Text);
   assert(ForceDirectories(dirOutput.Text));
   SetRegistryValue('\Software\TURBU', 'TURBU Test Project Output', dirOutput.Text);
end;

procedure TfrmTestProjLocation.dirProjectLocationChange(Sender: TObject);
begin
   btnOK.Enabled := validateDirectories;
end;

procedure TfrmTestProjLocation.FormCreate(Sender: TObject);
begin
   dirProjectLocation.InitialDir := IncludeTrailingPathDelimiter(GetRegistryValue('\Software\TURBU', 'TURBU Test Project'));
   dirProjectLocation.Text := dirProjectLocation.InitialDir;
   btnOK.Enabled := validateDirectories;
end;

procedure TfrmTestProjLocation.getLocation(var data: string);
begin
   if self.ShowModal = mrOk then
      data := dirProjectLocation.Text;
end;

function TfrmTestProjLocation.validateDirectories: boolean;
var
   dummy: string;
begin
   result := false;
   if dirProjectLocation.Text = '' then
      Exit;
   if DirectoryExists(dirProjectLocation.Text) then
      result := (FileExists(dirProjectLocation.Text + '\RPG_RT.lmt')) and (FileExists(dirProjectLocation.Text + '\RPG_RT.ldb'));
   dummy := ExtractFileName(ExcludeTrailingPathDelimiter(dirProjectLocation.Text));
   dirOutput.Text := getProjectFolder + dummy;
end;

end.
