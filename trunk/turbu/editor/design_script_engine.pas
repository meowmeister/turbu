unit design_script_engine;
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
uses
   types, sysUtils, classes, DB,
   dm_database,
   turbu_defs, turbu_classes, commons, turbu_script_basis, turbu_unit_dictionary,
   upsCompiler, upsRuntime, upsPreProcessor, upsUtils;

type
   TTurbuScriptEngine = class(TObject)
   private
      FCompiledScript: tbtString;
      FIndices: TScriptList;
      FDeclarations: TDeclList;
      FNameInProgress: ansiString;
      FPosInProgress: integer;
      FScript: ansiString;
      FUnits: TUnitDictionary;

      function getFunc(name: string): string;

      procedure startFunction(name: tbtString; pos, column, row: integer);
      procedure endFunction(name: tbtString; pos, column, row: integer);
      function buildProcList(Sender: TPSPascalCompiler): Boolean;
      function getBounds(name: string): TRpgPoint;
      function tryUseFile(Sender: TPSPascalCompiler; const Name: tbtstring): Boolean;
   protected
      FScriptEngine: TPSExec;
      FImporter: TPSRuntimeClassImporter;
      FCompiler: TPSPascalCompiler;
      FPreprocessor: TPSPreProcessor;

      procedure setScript(const Value: ansiString);
      procedure setup;
      procedure SetPointerToData(const VarName: ansiString; Data: Pointer; aType: TIFTypeRec);
   public
      //class functions
      constructor Create;
      destructor Destroy; override;
      procedure upload(db: TdmDatabase);

      //properties
      property compiler: TPSPascalCompiler read FCompiler;
      property script: ansiString write setScript;
      property exec: TPSExec read FScriptEngine;
      property units: TUnitDictionary read FUnits write FUnits;
      property func[name: string]: string read getFunc;
      property decl: TDeclList read FDeclarations;
      property bounds[name: string]: TRpgPoint read getBounds;
   end;

var
   GDScriptEngine: TTurbuScriptEngine;

implementation
uses
   math, TypInfo,
   logs, turbu_vartypes, turbu_constants,
   uPSI_script_interface,
   strtok,
   uPSC_std, uPSR_std;

function scriptOnUses(Sender: TPSPascalCompiler; const Name: tbtString): Boolean; forward;
procedure setupImportedFunctions(exec: TPSExec); forward;
function buildProcList(Sender: TPSPascalCompiler): Boolean; forward;

{ TTurbuScriptEngine }

constructor TTurbuScriptEngine.Create;
begin
   inherited Create;
   GDScriptEngine := self;

   FIndices := TScriptList.Create;
   FDeclarations := TDeclList.Create;

   FCompiler := TPSPascalCompiler.Create;
   FCompiler.BooleanShortCircuit := true;
   FImporter := TPSRuntimeClassImporter.Create;
   RIRegisterTObject(FImporter);
   FPreprocessor := TPSPreProcessor.Create;
   FPreprocessor.ID := self;
   FScriptEngine := TPSExec.Create;
   self.setup;
   RegisterClassLibraryRuntime(FScriptEngine, FImporter);
end;

destructor TTurbuScriptEngine.Destroy;
begin
   FCompiler.free;
   FImporter.Free;
   FScriptEngine.Free;
   FIndices.Free;
   FDeclarations.Free;
   FPreprocessor.Free;
   inherited Destroy;
end;

function TTurbuScriptEngine.buildProcList(Sender: TPSPascalCompiler): Boolean;
var
   i, j: integer;
   proc: TPSProcedure;
   decl: TPSParametersDecl;
   dummy: TRpgDecl;
   nameType: TNameType;
begin
   for I := 0 to FIndices.Count - 1 do
   begin
      dummy := TRpgDecl.Create(FIndices[i].name);
      proc := FCompiler.GetProc(FCompiler.FindProc(ansiString(dummy.name)));
      if proc is TPSInternalProcedure then
         decl := TPSInternalProcedure(proc).Decl
      else
         decl := (proc as TPSExternalProcedure).RegProc.Decl;
      for j := 0 to decl.ParamCount - 1 do
      begin
         nameType.typeVar := lookupType(string(decl.Params[j].aType.OriginalName));
         nameType.name := string(decl.params[j].OrgName);
         case decl.Params[j].Mode of
            pmIn: nameType.flags := [];
            pmOut: nameType.flags := [pfOut];
            pmInOut: nameType.flags := [pfVar];
         end;
         if nameType.typeVar in VT_ADDRESSES then
            Include(nameType.flags, pfAddress);
         dummy.params.add(nameType);
      end;
      dummy.retval := lookupType(string(decl.Result.OriginalName));
      FDeclarations.Add(dummy);
   end;
   result := true;
end;

procedure TTurbuScriptEngine.startFunction(name: tbtString; pos, column, row: integer);
begin
   FNameInProgress := name;
   FPosInProgress := pos;
end;

procedure TTurbuScriptEngine.endFunction(name: tbtString; pos, column, row: integer);
begin
   assert(name = FNameInProgress);
   FIndices.Add(TScriptRange.Create(string(FNameInProgress), point(FPosInProgress, pos)));
end;

function TTurbuScriptEngine.tryUseFile(Sender: TPSPascalCompiler; const Name: tbtstring): Boolean;
var
   uName: string;

   function useFile(out output: tbtString): boolean;
   begin
      result := FUnits.ContainsKey(uName);
      if result then
         output := tbtString(FUnits[uName].Text);
   end;

var
   lPrevAllowUnit: Boolean;
   lData: tbtstring;
begin
//   uName := UpperCase(string(Name) + '.trs');
   uName := string(Name);
   if useFile(lData) then begin
      lPrevAllowUnit := Sender.AllowUnit;
      Sender.AllowUnit := true;
//      FPreprocessor.Defines.Assign(FDefines);
      FPreprocessor.MainFile := lData;
      FPreprocessor.MainFileName := Name;
      FPreprocessor.PreProcess(Name, lData);
      Result := Sender.Compile(lData);
      FPreprocessor.AdjustMessages(Sender);
      Sender.AllowUnit := lPrevAllowUnit;
   end else begin
      Sender.MakeError(Sender.UnitName, ecUnknownIdentifier, Name);
      Result := false;
   end;
end;

procedure TTurbuScriptEngine.upload(db: TdmDatabase);
var
   iterator: TRpgDatafile;
begin
   for iterator in FIndices do
      iterator.upload(db.scriptRange);
end;

function TTurbuScriptEngine.getBounds(name: string): TRpgPoint;
var
   dummy: TScriptRange;
begin
   dummy := FIndices.names[name];
   if dummy = nil then
      result := point(0,0)
   else result := dummy.range;
end;

function TTurbuScriptEngine.getFunc(name: string): string;
var
   range: TPoint;
begin
   range := getBounds(name);
   if range.Y > 0 then
      result := string(Copy(FScript, range.X, (range.y - 1) - range.x))
   else result := '';
end;

procedure TTurbuScriptEngine.SetPointerToData(const VarName: ansiString;
  Data: Pointer; aType: TIFTypeRec);
var
   v: PIFVariant;
   t: TPSVariantIFC;
begin
   v := FScriptEngine.GetVar2(VarName);
   if (Atype = nil) or (v = nil) then
      raise EScriptError.Create('Unable to find variable');
   t.Dta := @PPSVariantData(v).Data;
   t.aType := v.FType;
   t.VarParam := false;
   VNSetPointerTo(t, Data, aType);
end;

procedure TTurbuScriptEngine.setScript(const Value: ansiString);
begin
   FScript := value;
   FIndices.Clear;
   FDeclarations.Clear;
   FCompiler.Clear;
   FCompiler.OnFunctionStart := Self.startFunction;
   FCompiler.OnFunctionEnd := self.endFunction;
   FCompiler.OnBeforeCleanup := design_script_engine.buildProcList;
   if not FCompiler.Compile(value) then
   begin
      logText(unicodeString(value));
      logText(LFCR + 'Could not compile script file!');
      logText(unicodeString(Fcompiler.Msg[0].MessageToString));
      raise EScriptError.create('Could not compile script file!');
   end;
   assert(FCompiler.GetOutput(FCompiledScript));
   if not FScriptEngine.LoadData(FCompiledScript) then
   begin
      logText(unicodeString(value));
      logText(LFCR + 'Could not load compiled script file!');
      logText(unicodeString(TIFErrorToString(FScriptEngine.ExceptionCode, FScriptEngine.ExceptionString)));
      raise EScriptError.create('Could not load compiled script file!');
   end;
end;

procedure TTurbuScriptEngine.setup;
begin
   //add to this as needed
   //example:

   FCompiler.onUses := scriptOnUses;
   setupImportedFunctions(FScriptEngine);
   RIRegister_script_interface(FImporter);

end;

function scriptOnUses(Sender: TPSPascalCompiler; const Name: tbtString): Boolean;

   function AddPointerVariable(const VarName, VarType: ansiString): Boolean;
   var
      FVar: TPSVar;
   begin
      FVar := sender.AddUsedVariableN(varname, vartype);
      if fvar = nil then
         result := False
      else begin
         fvar.exportname := fvar.Name;
         fvar.SaveAsPointer := true;
         Result := True;
      end;
   end;

var dummy: TPSType;
begin
   result := true;
   if Name = 'SYSTEM' then begin
      SIRegisterTObject(sender);
      sender.AddDelphiFunction('function min(const a, b: integer): integer;');
      sender.AddDelphiFunction('function max(const a, b: integer): integer;');
      SIRegister_script_interface(sender);
      dummy := sender.addtypeS('varArray', 'array of integer');
      dummy.ExportName := true;
      dummy := sender.AddTypeS('switchArray', 'array of boolean');
      dummy.ExportName := true;
      assert(AddPointerVariable('variable', 'varArray'));
      assert(AddPointerVariable('switch', 'switchArray'));
   end
   else result := GDScriptEngine.tryUseFile(sender, name);
end;

procedure setupImportedFunctions(exec: TPSExec);
begin
   exec.RegisterDelphiFunction(@math.min, 'MIN', cdRegister);
   exec.RegisterDelphiFunction(@math.max, 'MAX', cdRegister);
end;

function buildProcList(Sender: TPSPascalCompiler): Boolean;
begin
   result := GDScriptEngine.buildProcList(sender);
end;

initialization
begin
   GDScriptEngine := TTurbuScriptEngine.create;
end;

finalization
begin
   GDScriptEngine.Free;
end;

end.
