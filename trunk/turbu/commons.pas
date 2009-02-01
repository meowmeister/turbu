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
   classes, types, Forms, sysUtils, Registry, math, windows, contnrs; //system libraries

const
   KEY_READ = $000F0019;
   HKEY_CURRENT_USER = $80000001;
   OPEN_READ = fmOpenRead or fmShareDenyWrite;
   OPEN_WRITE = fmOpenWrite or fmShareExclusive;
   OPEN_CREATE = fmCreate or fmShareExclusive;
   OPEN_READWRITE = fmOpenReadWrite or fmShareExclusive;
   LFCR = #13#10;
   ORIGIN: TPoint = (X: 0; Y: 0);
   MULTIPLIER_31 = 8.2258064516129032258064516129032;
   DIRMARK = '\';


//   HKEY_LOCAL_MACHINE = $80000002;

type
   TRpgColor = record
      constructor Create(r, g, b, a: byte);
   case boolean of
      false: (color: cardinal);
      true: (rgba: packed array[1..4] of byte);
   end;

   TRpgStack = class(TObjectStack)
      function pop: TObject;
      function peek: TObject;
   end;

   TRpgPoint = record
      x, y: integer;
      class operator Equal(a, b: TRpgPoint): boolean; inline;
      class operator NotEqual(a, b: TRpgPoint): boolean; inline;
      class operator Multiply(a: TRpgPoint; b: integer): TRpgPoint; inline;
      class operator Divide(a: TRpgPoint; b: integer): TRpgPoint; inline;
      class operator Add(a, b: TRpgPoint): TRpgPoint; inline;
      class operator Subtract(a, b: TRpgPoint): TRpgPoint; inline;
      class operator Implicit(a: TPoint): TRpgPoint; inline;
      class operator Implicit(a: TRpgPoint): TPoint; inline;
   end;

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

   TRpgThread = class(TThread)
   public
      procedure syncRun(AMethod: TThreadMethod); inline;
   end;

   function powerWrap (base, exponent: integer): integer;
   function MsgBox(text, caption: string; Flags: Word = MB_OK): Integer;
   function GetRegistryValue(KeyName, valueName: string): string;
   procedure SetRegistryValue(KeyName, valueName, value: string);
   function openFile (const filename:string; const mode:word = OPEN_READ):TFileStream;
   function getPersonalFolder: string;
   function getProjectFolder: string;
   procedure createProjectFolder;
   function between(number, low, high: integer): integer; inline;
   function round(value: real): integer; inline;
   function pointInRect(const thePoint: types.TPoint; theRect: TRect): boolean;
   procedure clamp(var value: single; low, high: single); inline;
   function makeMethod(AObject: TObject; proc: pointer): TMethod;

var
   GCurrentFolder: string;
   GProjectFolder: string;

threadvar
   GCurrentThread: TRpgThread;

implementation

uses
   shlobj;

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
   tempCharArray1, tempCharArray2: array[0..300] of Char;
   syncMessager: TMessageBoxer;
begin
   StrPCopy(tempCharArray1, Copy(text, 0, 299));
   StrPCopy(tempCharArray2, Copy(caption, 0, 299));
   if assigned(GCurrentThread) then
   begin
      syncMessager := TMessageBoxer.create(tempCharArray1, tempCharArray2, Flags);
      GCurrentThread.syncRun(syncMessager.display);
      while not syncMessager.finished do
         sleep(100);
      result := syncMessager.result;
      syncMessager.free;
   end else
      result := Application.MessageBox(tempCharArray1, tempCharArray2, Flags);
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

{Quick way to open a file}
function openFile (const filename: string; const mode: word = OPEN_READ): TFileStream;
begin
   result := TFileStream.Create(filename, mode)
end;

function pointInRect(const thePoint: types.TPoint; theRect: TRect): boolean;
begin
   result := (between(thePoint.x, theRect.Left, theRect.Right) = thePoint.x) and
      (between(thePoint.y, theRect.top, theRect.Bottom) = thePoint.y);
end;

function between(number, low, high: integer): integer;
begin
   result := min(max(number, low), high);
end;

//Replacement for Delphi's "banker's rounding" function
function round(value: real): integer;
begin
   result := trunc(value + 0.5);
end;

{ TRpgStack }

function TRpgStack.peek: TObject;
begin
   if List.Count = 0 then
      result := nil
   else result := inherited peek;
end;

function TRpgStack.pop: TObject;
begin
   if List.Count = 0 then
      result := nil
   else result := inherited pop;
end;

{ TRpgThread }

procedure TRpgThread.syncRun(AMethod: TThreadMethod);
begin
   self.Synchronize(AMethod);
end;

{ TPoint }

class operator TRpgPoint.Add(a, b: TRpgPoint): TRpgPoint;
begin
   result.x := a.x + b.x;
   result.y := a.y + b.y;
end;

class operator TRpgPoint.Subtract(a, b: TRpgPoint): TRpgPoint;
begin
   result.x := a.x - b.x;
   result.y := a.y - b.y;
end;

class operator TRpgPoint.Multiply(a: TRpgPoint; b: integer): TRpgPoint;
begin
   result.x := a.x * b;
   result.y := a.y * b;
end;

class operator TRpgPoint.Divide(a: TRpgPoint; b: integer): TRpgPoint;
begin
   result.x := commons.round(a.x / b);
   result.y := commons.round(a.y / b);
end;

class operator TRpgPoint.Equal(a, b: TRpgPoint): boolean;
begin
   result := (a.x = b.x) and (a.y = b.y);
end;

class operator TRpgPoint.Implicit(a: TRpgPoint): TPoint;
begin
   result.x := a.x;
   result.y := a.y;
end;

class operator TRpgPoint.Implicit(a: TPoint): TRpgPoint;
begin
   result.x := a.x;
   result.y := a.y;
end;

class operator TRpgPoint.NotEqual(a, b: TRpgPoint): boolean;
begin
   result := not (a = b);
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

procedure clamp(var value: single; low, high: single);
begin
   value := min(high, max(low, value));
end;

function makeMethod(AObject: TObject; proc: pointer): TMethod;
begin
   result.Data := AObject;
   result.Code := proc;
end;

{ TRpgColor }

constructor TRpgColor.Create(r, g, b, a: byte);
begin
   self.rgba[1] := r;
   self.rgba[2] := g;
   self.rgba[3] := b;
   self.rgba[4] := a;
end;

initialization
begin
   GCurrentThread := nil;
end;

end.
