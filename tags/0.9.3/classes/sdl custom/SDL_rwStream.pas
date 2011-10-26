unit SDL_rwStream;
{*****************************************************************************
* SDL_rwStream                                           Modified: 19-Apr-2008
******************************************************************************
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
******************************************************************************
*
* This file was created by Mason Wheeler.  He can be reached for support at
* www.turbu-rpg.com.
*****************************************************************************}

interface
uses
   classes,
   SDL;

type

   {***************************************************************************
   * Custom TStream descendant that encapsulates a SDL_RWops.
   ***************************************************************************}
   TRWStream = class(TStream)
   private
      FOps: PSDL_RWops;
      FOwnsOps: Boolean;
   protected

   public
      {************************************************************************
      * Creates a TRWStream.  The ops parameter must be a valid PSDL_RWops, or
      * construction will fail and raise an exception. The owns parameter sets
      * the OwnsRWops property.
      ************************************************************************}
      constructor Create(ops: PSDL_RWops; owns: boolean = true);

      {************************************************************************
      * If the OwnsRWOps property is set to true, the SDL_RWops will be
      * freed as part of the stream's destruction.  If not, its position will
      * be reset to 0.
      ************************************************************************}
      destructor Destroy; override;

      {************************************************************************
      * Standard TStream class members.
      ************************************************************************}
      function Read(var Buffer; Count: Integer): Integer; override;
      function Write(const Buffer; Count: Integer): Integer; override;
      function Seek(Offset: Integer; Origin: Word): Integer; override;

      property OwnsRWOps: boolean read FOwnsOps write FOwnsOps;
      property Ops: PSdl_RWops read FOps;
   end;

implementation

{ TRWStream }

constructor TRWStream.Create(ops: PSDL_RWops; owns: boolean);
begin
   inherited Create;
   if not assigned(ops) then
      raise EStreamError.Create('No SDL_RWops available for TRWStream creation!');
   FOps := ops;
   FOwnsOps := owns;
end;

destructor TRWStream.Destroy;
begin
   if FOwnsOps then
   begin
      FOps.close(FOps);
      SDL_FreeRW(FOps);
   end

   //this is not strictly necessary, but it leaves the RWops in a known state,
   //which is always nice
   else self.Seek(0, soFromBeginning);
   inherited Destroy;
end;

function TRWStream.Read(var Buffer; Count: Integer): Integer;
begin
   result := FOps.read(FOps, @Buffer, 1, count);
end;

function TRWStream.Seek(Offset: Integer; Origin: Word): Integer;
begin
   result := FOps.seek(FOps, offset, origin);
end;

function TRWStream.Write(const Buffer; Count: Integer): Integer;
begin
   result := FOps.write(FOps, @buffer, 1, count);
end;

end.
