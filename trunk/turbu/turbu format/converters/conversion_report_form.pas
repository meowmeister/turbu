unit conversion_report_form;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls,
  conversion_report, conversion_output;

type
   TfrmConversionReport = class(TForm, IConversionReport)
      Panel1: TPanel;
      prgConversion: TProgressBar;
      lblProgress: TLabel;
      prgSteps: TProgressBar;
      lblSteps: TLabel;
      pnlHints: TPanel;
      Label1: TLabel;
      lblHintCount: TLabel;
      pnlWarnings: TPanel;
      Label2: TLabel;
      lblWarningCount: TLabel;
      pnlErrors: TPanel;
      Label3: TLabel;
      lblErrorCount: TLabel;
      lblStatusLabel: TLabel;
      lblCurrentStatus: TLabel;
      btnDone: TButton;
      procedure FormShow(Sender: TObject);
      procedure FormCreate(Sender: TObject);
      procedure btnDoneClick(Sender: TObject);
   private
      { Private declarations }
      FRunning: boolean;
      FCurrentTaskRunning: boolean;
      FThread: TThread;
      FFatal: boolean;
      FOutput: TfrmConversionOutput;
      FFinished: boolean;

      procedure setTasks(const value: integer);
      procedure setCurrentTask(const name: string; const steps: integer); overload;
      procedure setCurrentTask(const name: string); overload;
      procedure newStep(name: string);
      procedure makeHint(text: string; group: integer = -1);
      procedure makeNotice(text: string; group: integer = -1);
      procedure makeError(text: string; group: integer = -1);
      procedure makeReport;
      procedure fatal(errorMessage: string); overload;
      procedure fatal(error: Exception); overload;
   public
      { Public declarations }
      property thread: TThread read FThread write FThread;
   end;

var
  frmConversionReport: TfrmConversionReport;

implementation
uses
   commons, logs;

{$R *.dfm}

{$IFDEF SAMPLING}
type
   TThreadHelper = class(TThread);
{$ENDIF}

{ TfrmConversionReport }

procedure TfrmConversionReport.fatal(errorMessage: string);
begin
   Application.MessageBox(PChar(errorMessage), 'Fatal error');
   self.ModalResult := mrAbort;
   self.Close;
end;

procedure TfrmConversionReport.btnDoneClick(Sender: TObject);
begin
   if assigned(FThread) and not FFinished then
      FThread.Terminate;
end;

procedure TfrmConversionReport.fatal(error: Exception);
begin
   Fatal(format('Unhandled conversion exception %s:'#13#10'"%s"', [error.ClassName, error.Message]));
   ReleaseExceptionObject;
end;

procedure TfrmConversionReport.FormCreate(Sender: TObject);
begin
   FOutput := TfrmConversionOutput.Create(Application);
   FOutput.Left := self.left;
   FOutput.Top := self.Top + self.Height + 20;
end;

procedure TfrmConversionReport.FormShow(Sender: TObject);
begin

   if FFatal then
   begin
      self.ModalResult := mrAbort;
      self.Close;
      Exit;
   end;
   if assigned(FThread) then
   {$IFDEF SAMPLING}
   begin
      try
         TThreadHelper(FThread).Execute;
      finally
         FThread.Free;
      end;
   end;
   {$ELSE}
      FThread.Start;
   {$ENDIF}
end;

procedure TfrmConversionReport.makeError(text: string; group: integer);
begin
   FOutput.AddItem(format('Error: %s', [text]), group);
   lblErrorCount.Caption := intToStr(StrToInt(lblErrorCount.Caption) + 1);
end;

procedure TfrmConversionReport.makeHint(text: string; group: integer);
begin
   FOutput.AddItem(format('Hint: %s', [text]), group);
   lblHintCount.Caption := intToStr(StrToInt(lblHintCount.Caption) + 1);
end;

procedure TfrmConversionReport.makeNotice(text: string; group: integer);
begin
   FOutput.AddItem(format('Note: %s', [text]), group);
   lblWarningCount.Caption := intToStr(StrToInt(lblWarningCount.Caption) + 1);
end;

procedure TfrmConversionReport.makeReport;
var
   closure: TThreadProcedure;
begin
   closure := procedure()
   begin
      prgConversion.StepIt;
      assert(prgConversion.Position = prgConversion.Max);
      lblCurrentStatus.Font.Style := [fsBold];
      lblStatusLabel.Font.Style := [fsBold];
      lblCurrentStatus.Caption := 'Complete';
      btnDone.ModalResult := mrOk;
      btnDone.Caption := '&OK';
      self.FocusControl(btnDone);
      FFinished := true;
   end;
   runThreadsafe(closure, true);
end;

procedure TfrmConversionReport.newStep(name: string);
var
   closure: TThreadProcedure;
begin
   closure := procedure()
   begin
      if FCurrentTaskRunning then
         prgSteps.StepIt;
      FCurrentTaskRunning := true;
      lblSteps.Caption := name;
   end;
   runThreadsafe(closure, true);
end;

procedure TfrmConversionReport.setCurrentTask(const name: string; const steps: integer);
var
   closure: TThreadProcedure;
begin
   closure := procedure()
   begin
      if FRunning then
         prgConversion.StepIt;
      FRunning := true;
      FCurrentTaskRunning := false;
      lblCurrentStatus.Caption := name;
      prgSteps.Max := steps;
      prgSteps.Position := 0;
   end;
   runThreadsafe(closure, true);
end;

procedure TfrmConversionReport.setCurrentTask(const name: string);
var
   closure: TThreadProcedure;
begin
   closure := procedure()
   begin
      setCurrentTask(name, 1);
      newStep('');
      newStep(name);
   end;
   runThreadsafe(closure, true);
end;

procedure TfrmConversionReport.setTasks(const value: integer);
var
   closure: TThreadProcedure;
begin
   closure := procedure()
   begin
      prgConversion.Max := value;
      prgConversion.Position := 0;
      FRunning := false;
   end;
   runThreadsafe(closure, true);
end;

end.
