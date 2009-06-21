unit conversion_report_form;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls,
  conversion_report;

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
   private
      { Private declarations }
      FRunning: boolean;
      FCurrentTaskRunning: boolean;
      FThread: TThread;
      FFatal: boolean;

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
   logs.logText('Fatal error: ' + errorMessage);
   FFatal := true;
end;

procedure TfrmConversionReport.fatal(error: Exception);
begin
   Fatal(format('unhandled conversion exception %s.  "%s"', [error.ClassName, error.Message]));
   ReleaseExceptionObject;
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
      FThread.Resume;
   {$ENDIF}
end;

procedure TfrmConversionReport.makeError(text: string; group: integer);
begin
   assert(false);
end;

procedure TfrmConversionReport.makeHint(text: string; group: integer);
begin
   assert(false);
end;

procedure TfrmConversionReport.makeNotice(text: string; group: integer);
begin
   assert(false);
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
   end;
   runThreadsafe(closure);
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
   runThreadsafe(closure);
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
   runThreadsafe(closure);
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
   runThreadsafe(closure);
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
   runThreadsafe(closure);
end;

end.
