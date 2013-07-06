unit commons;
{*****************************************************************************
* The contents of this file are used with permission, subject to
* the Mozilla Public License Version 1.1 (the "License"); you may
* not use this file except in compliance with the License. You may
* obtain a copy of the License at
* http://www.mozilla.org/MPL/MPL-1.1.html
*
* Software distributed under the License is distributed on an
* "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
* implied. See the License for the specific language governing
* rights and limitations under the License.
*
*****************************************************************************
*
* This file was created by Mason Wheeler.  He can be reached for support at
* www.turbu-rpg.com.
*****************************************************************************}

{A collection of assorted miscellaneous definitions and routines that need to be
available to the rest of the project; almost every other unit includes this
unit.}

interface

uses
   types, classes, controls, Forms, sysUtils, Registry, math, windows, contnrs; //system libraries

const
   KEY_READ = $000F0019;
   HKEY_CURRENT_USER = $80000001;
   LFCR = #13#10;
   ORIGIN: TPoint = (X: 0; Y: 0);
   NULLRECT: TRect = (Left: 0; Top: 0; Right: 0; Bottom: 0);
   MULTIPLIER_31 = 8.2258064516129032258064516129032;

//   HKEY_LOCAL_MACHINE = $80000002;

type
   TSet16 = set of 0..15;

   TRpgColor = record
      constructor Create(r, g, b, a: byte);
   case boolean of
      false: (color: cardinal);
      true: (rgba: packed array[1..4] of byte);
   end;

   TGLColor = array[1..4] of single;

   {Custom exception for handling parse errors.}
   EParseMessage = class(Exception);

   EFatalError = class(Exception);

   {After an EParseMessage exception, to continue to try to parse the file would
   only result in exception after exception after exception, due to the nature
   of RM2K's file format.  Instead, an EMessageAbort is raised after the parse
   message is displayed, causing the program to exit back to the main loop.}
   EMessageAbort = class(Exception)
   public
      constructor create;
   end;

   TRpgThread = class helper for TThread
   public
      procedure syncRun(AMethod: TThreadMethod); inline;
   end;

   function powerWrap (base, exponent: integer): integer;
   function MsgBox(text, caption: string; Flags: Word = MB_OK): Integer;
   function GetRegistryValue(KeyName, valueName: string): string;
   procedure SetRegistryValue(KeyName, valueName, value: string);
   function getPersonalFolder: string;
   function getProjectFolder: string;
   function getTempFolder: string;
   procedure createProjectFolder;
   function IsBetween(number, low, high: integer): boolean; inline;
   function round(value: real): integer; inline;
   function pointInRect(const thePoint: types.TPoint; theRect: TRect): boolean;
   procedure clamp(var value: single; low, high: single); inline; overload;
   function clamp(number, low, high: integer): integer; inline; overload;
   procedure EnableControls(const controls: array of TControl; enabled: boolean);
   procedure swap(var x, y: integer); inline;
   procedure OutputFormattedString(const value: string; const args: array of const);

   //modulus function that doesn't "mirror" for negative numbers
   function safeMod(big, small: integer): integer; inline;

   procedure runThreadsafe(closure: TThreadProcedure; synchronous: boolean = false);

var
   GCurrentFolder: string;

implementation

uses
   shlobj;

procedure runThreadsafe(closure: TThreadProcedure; synchronous: boolean = false);
var
   ct: TThread;
begin
   ct := TThread.CurrentThread;
   if ct.ThreadID <> System.MainThreadID then
   begin
      if synchronous then
         TThread.Synchronize(ct, closure)
      else TThread.Queue(ct, closure);
   end
   else closure();
end;

function safeMod(big, small: integer): integer;
begin
   result := ((big mod small) + small) mod small;
end;

{Constructor that does nothing, but is required due to the nature of Delphi's
exception handling.}
constructor EMessageAbort.create;
begin
   inherited create('');
end;

{Wrapper for the standard exponent function, allowing it to work on integers.
(It only wants to use floating-point values.)}
function powerWrap (base, exponent: integer): integer;
var
   exp, dummy: extended;
begin
   exp := exponent;
   dummy := power(base, exp);
   result := trunc(dummy);
end;

{ TMessageBoxer }

type TMessageBoxer = class(TObject)
private
   FText, FCaption: PChar;
   FFlags: integer;
   FResult: integer;
   FFinished: boolean;
public
   constructor Create(text, caption: PChar; flags: integer);
   procedure display;

   property finished: boolean read FFinished;
   property result: integer read FResult;
end;

constructor TMessageBoxer.Create(text, caption: PChar; flags: integer);
begin
   FText := text;
   FCaption := caption;
   FFlags := flags;
   FFinished := false;
end;

procedure TMessageBoxer.display;
begin
   FResult := Application.MessageBox(FText, FCaption, FFlags);
   FFinished := true;
end;

{Wrapper for the standard MessageBox function; makes displaying a message dialog
simpler.  Updated 3-15-08 for thread safety}
function MsgBox(text, caption: string; Flags: Word = MB_OK): Integer;
var
   ct: TThread;
   tempCharArray1, tempCharArray2: array[0..300] of Char;
   syncMessager: TMessageBoxer;
begin
   ct := TThread.CurrentThread;
   StrPCopy(tempCharArray1, Copy(text, 0, 299));
   StrPCopy(tempCharArray2, Copy(caption, 0, 299));
   if ct.Handle <> System.MainThreadID then
   begin
      syncMessager := TMessageBoxer.create(tempCharArray1, tempCharArray2, Flags);
      ct.syncRun(syncMessager.display);
      while not syncMessager.finished do
         sleep(100);
      result := syncMessager.result;
      syncMessager.free;
   end
   else result := Application.MessageBox(tempCharArray1, tempCharArray2, Flags);
end;

{Retrieves the specified value from the Windows Registry}
function GetRegistryValue(KeyName, valueName: string): string;
var
  Registry: TRegistry;
begin
   result := '';
   Registry := TRegistry.Create(KEY_READ);
   try
      Registry.RootKey := HKEY_CURRENT_USER;
      // False because we do not want to create it if it doesn't exist
      if not Registry.OpenKey(KeyName, false) then
         Exit;
      result := Registry.ReadString(valueName);
   finally
      Registry.Free;
   end;
end;

{Stores the specified value to the Windows Registry}
procedure SetRegistryValue(KeyName, valueName, value: string);
var
  Registry: TRegistry;
begin
   Registry := TRegistry.Create(KEY_WRITE);
   try
      Registry.RootKey := HKEY_CURRENT_USER;
      if not Registry.OpenKey(KeyName, true) then
         Exit;
      Registry.WriteString(valueName, value);
   finally
      Registry.Free;
   end;
end;

function pointInRect(const thePoint: types.TPoint; theRect: TRect): boolean;
begin
   result := IsBetween(thePoint.x, theRect.Left, theRect.Right) and
      IsBetween(thePoint.y, theRect.top, theRect.Bottom);
end;

function clamp(number, low, high: integer): integer;
begin
   result := min(max(number, low), high);
end;

function IsBetween(number, low, high: integer): boolean;
begin
   result := (number >= low) and (number <= high);
end;

//Replacement for Delphi's "banker's rounding" function
function round(value: real): integer;
begin
   if value >= 0 then
      result := trunc(value + 0.5)
   else result := trunc(value - 0.5)
end;

procedure EnableControls(const controls: array of TControl; enabled: boolean);
var
   control: TControl;
begin
   for control in controls do
      control.Enabled := enabled;
end;

procedure OutputFormattedString(const value: string; const args: array of const);
begin
   OutputDebugString(PChar(format(value, args)));
end;

{ TRpgThread }

procedure TRpgThread.syncRun(AMethod: TThreadMethod);
begin
   Synchronize(self, AMethod);
end;

function getPersonalFolder: string;
var
   dest: PChar;
begin
   dest := StrAlloc(MAX_PATH);
   SHGetSpecialFolderPath (0, dest, CSIDL_PERSONAL, false);
   result := dest;
   StrDispose(dest);
end;

function getProjectFolder: string;
begin
   result := IncludeTrailingPathDelimiter(GetRegistryValue('\Software\TURBU', 'TURBU Projects Folder'));
end;

procedure createProjectFolder;
begin
   SetRegistryValue('\Software\TURBU', 'TURBU Projects Folder', IncludeTrailingPathDelimiter(getPersonalFolder) + 'TURBU Projects');
   ForceDirectories(getProjectFolder);
end;

function getTempFolder: string;
var
   buffer: array [0..MAX_PATH] of char;
   pBuffer: PChar;
begin
   pBuffer := @buffer[0];
   GetTempPath(length(buffer), pBuffer);
   result := string(pBuffer);
end;

procedure clamp(var value: single; low, high: single);
begin
   value := min(high, max(low, value));
end;

procedure swap(var x, y: integer);
var
   dummy: integer;
begin
   dummy := x;
   x := y;
   y := dummy;
end;

{ TRpgColor }

constructor TRpgColor.Create(r, g, b, a: byte);
begin
   self.rgba[1] := r;
   self.rgba[2] := g;
   self.rgba[3] := b;
   self.rgba[4] := a;
end;

end.
