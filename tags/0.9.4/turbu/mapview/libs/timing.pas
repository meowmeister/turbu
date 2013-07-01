unit timing;
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

interface
type
   TRpgTimestamp = class(TObject)
   private
      class var
      FLastFrame: TDateTime;
      FCounter: integer;
      FFrameLength: array[1..10] of integer;
   private
      FHour: word;
      FMin: word;
      FSec: word;
      FMsec: word;
      FPauseTime: integer;
      FPaused: boolean;

      procedure setup(length: integer); inline;
      class function GetFrameLength: integer; static;
   public
      constructor Create(length: integer);
      function timeRemaining: integer;
      procedure pause; inline;
      procedure resume;

      class procedure newFrame;
      class property FrameLength: integer read GetFrameLength;
   end;

   procedure moveTowards(timer: integer; var current: extended; const goal: extended); overload;
   procedure moveTowards(timer: integer; var current: single; const goal: single); overload;
   procedure moveTowards(timer: integer; var current: byte; const goal: byte); overload;
   procedure moveTowards(timer: integer; var current: integer; const goal: integer); overload;

implementation
uses
   sysUtils, math, windows, DateUtils,
   commons;

{ TRpgTimestamp }

constructor TRpgTimestamp.Create(length: integer);
begin
   inherited Create;
   setup(length);
end;

class function TRpgTimestamp.GetFrameLength: integer;
begin
   result := round(math.SumInt(FFrameLength) / length(FFrameLength));
end;

class procedure TRpgTimestamp.newFrame;
var
   i: integer;
   delta: integer;
begin
   if FLastFrame = 0 then
      FLastFrame := Now
   else begin
      delta := MilliSecondsBetween(Now, FLastFrame);
      FLastFrame := Now;
      if FCounter = 0 then
      begin
         for i := low(FFrameLength) to high(FFrameLength) do
            FFrameLength[i] := delta;
         inc(FCounter);
      end
      else begin
         inc(FCounter);
         if FCounter > high(FFrameLength) then
            FCounter := low(FFrameLength);
         FFrameLength[FCounter] := delta;
      end;
   end;
end;

procedure TRpgTimestamp.pause;
begin
   if not FPaused then
   begin
      FPauseTime := self.timeRemaining;
      FPaused := true;
   end;
end;

procedure TRpgTimestamp.resume;
begin
   if FPaused then
   begin
      self.setup(FPauseTime);
      FPaused := false;
   end;
end;

procedure TRpgTimestamp.setup(length: integer);
begin
   try
      decodeTime(sysUtils.GetTime, FHour, FMin, FSec, FMsec);
      inc(FMsec, length mod 1000);
      if FMsec >= 1000 then
      begin
         inc(FSec);
         dec(FMsec, 1000);
      end;
      length := length div 1000;
      if length = 0 then
         Exit;
      inc(FSec, length mod 60);
      if FSec >= 60 then
      repeat
         inc(FMin);
         dec(FSec, 60);
      until FSec < 60;
      length := length div 60;
      if length = 0 then
         Exit;
      inc(FMin, length mod 60);
      if FMin >= 60 then
      repeat
         inc(FHour);
         dec(FMin, 60);
      until FMin < 60;
      length := length div 60;
      assert(length = 0);
   finally
      if FHour = 0 then
         FHour := 24;
   end;
end;

function TRpgTimestamp.timeRemaining: integer;
var
   theHour, theMin, theSec, theMsec: word;
   hour, min, sec, msec: smallint;
begin
   if FPaused then
   begin
      result := FPauseTime;
      Exit;
   end;

   decodeTime(sysUtils.GetTime, theHour, theMin, theSec, theMsec);
   hour := theHour;
   min := theMin;
   sec := theSec;
   msec := theMsec;
   if hour = 0 then
      hour := 24;
   hour := FHour - hour;
   min := FMin - min;
   sec := FSec - sec;
   msec := FMsec - msec;
   if msec < 0 then
   repeat
      dec(sec);
      inc(msec, 1000);
   until msec >= 0;
   if sec < 0 then
   repeat
      dec(min);
      inc(sec, 60);
   until sec >= 0;
   if min < 0 then
   repeat
      dec(hour);
      inc(min, 60);
   until min >= 0;
   if hour = 0 then
      result := (min * 60000) + (sec * 1000) + msec
   else result := 0;
end;

{ Classless }

procedure moveTowards(timer: integer; var current: extended; const goal: extended);
var
   diff: extended;
   timefactor: integer;
begin
   timefactor := max(timer div TRpgTimeStamp.FrameLength, 1);
   diff := (current - goal) / timefactor;
   current := current - diff;
end;

procedure moveTowards(timer: integer; var current: single; const goal: single);
var
   diff: single;
   timefactor: integer;
begin
   timefactor := max(timer div TRpgTimeStamp.FrameLength, 1);
   diff := (current - goal) / timefactor;
   current := current - diff;
end;

procedure moveTowards(timer: integer; var current: byte; const goal: byte);
var
   diff: smallint;
   timefactor: integer;
begin
   timefactor := max(timer div TRpgTimeStamp.FrameLength, 1);
   diff := commons.round((current - goal) / timefactor);
   assert(abs(diff) < 256);
   current := current - diff;
end;

procedure moveTowards(timer: integer; var current: integer; const goal: integer);
var
   diff: smallint;
   timefactor: integer;
begin
   timefactor := max(timer div TRpgTimeStamp.FrameLength, 1);
   diff := commons.round((current - goal) / timefactor);
   current := current - diff;
end;

end.
