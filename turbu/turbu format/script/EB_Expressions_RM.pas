unit EB_Expressions_RM;

interface
uses
   EB_Expressions, EventBuilder;

type
   TEBSwitchesValue = class(TEBVariableValue)
   public
      constructor Create(subscript: integer); overload;
      constructor Create(subscript: TEBExpression); overload;
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBIntsValue = class(TEBVariableValue)
   public
      constructor Create(subscript: integer); overload;
      constructor Create(subscript: TEBExpression); overload;
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

function CreateSubscript(mode, data: integer): TEBExpression;

implementation
uses
   SysUtils,
   EB_ObjectHelper;

function CreateSubscript(mode, data: integer): TEBExpression;
begin
   case mode of
      0: result := TEBIntegerValue.Create(data);
      1: result := TEBVariableValue.Create('Num');
      2: result := TEBIntsValue.Create(data);
      else raise ERPGScriptError.CreateFmt('Unknown subscript mode value: %d!', [mode]);
   end;
end;

{ TEBSwitchesValue }

constructor TEBSwitchesValue.Create(subscript: integer);
begin
   inherited Create('Switch', subscript);
   FIsGlobal := true;
end;

constructor TEBSwitchesValue.Create(subscript: TEBExpression);
begin
   inherited Create('Switch', subscript);
   FIsGlobal := true;
end;

function TEBSwitchesValue.GetNodeText: string;
begin
   if values.count > 0 then
      result := format('Switch[%s]', [SwitchName(values[0])])
   else result := inherited GetNodeText;
end;

function TEBSwitchesValue.GetScriptText: string;
begin
   if values.count > 0 then
      result := format('Switch[%d]', [values[0]])
   else result := inherited GetScriptText;
end;

{ TEBIntsValue }

constructor TEBIntsValue.Create(subscript: integer);
begin
   inherited Create('Ints', subscript);
   FIsGlobal := true;
end;

constructor TEBIntsValue.Create(subscript: TEBExpression);
begin
   inherited Create('Ints', subscript);
   FIsGlobal := true;
end;

function TEBIntsValue.GetNodeText: string;
begin
   if values.count > 0 then
      result := format('Ints[%s]', [IntName(values[0])])
   else result := inherited GetNodeText;
end;

function TEBIntsValue.GetScriptText: string;
begin
   if values.count > 0 then
      result := format('Ints[%d]', [values[0]])
   else result := inherited GetScriptText;
end;

initialization
   TEBObject.RegisterClasses([TEBSwitchesValue, TEBIntsValue]);
end.
