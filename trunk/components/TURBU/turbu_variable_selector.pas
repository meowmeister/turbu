unit turbu_variable_selector;

interface
uses
   DBClient,
   variable_selector;

type
   TSwitchSelector = class(TCustomVarSelector)
   protected
      function GetName: string; override;
      function GetDset: TCustomClientDataset; override;
   end;

   TIntSelector = class(TCustomVarSelector)
   protected
      function GetName: string; override;
      function GetDset: TCustomClientDataset; override;
   end;

procedure Register;

implementation
uses
   Classes{$IFNDEF COMPONENT},
   dm_database{$ENDIF};

procedure Register;
begin
   RegisterComponents('TURBU', [TIntSelector, TSwitchSelector]);
end;

{ TSwitchSelector }

function TSwitchSelector.GetDset: TCustomClientDataset;
begin
{$IFNDEF COMPONENT}
   result := dmDatabase.Switches;
{$ENDIF}
end;

function TSwitchSelector.GetName: string;
begin
   result := 'Switch';
end;

{ TIntSelector }

function TIntSelector.GetDset: TCustomClientDataset;
begin
{$IFNDEF COMPONENT}
   result := dmDatabase.Variables;
{$ENDIF}
end;

function TIntSelector.GetName: string;
begin
   result := 'Variable';
end;

end.
