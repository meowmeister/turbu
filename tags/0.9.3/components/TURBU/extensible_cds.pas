unit extensible_cds;

interface
uses
   DBClient;

type
   TExtensibleClientDataset = class(TClientDataset)
   protected
      procedure InternalOpen; override;
   end;

implementation
uses
   db, RTTI;

{ TExtensibleClientDataset }

procedure TExtensibleClientDataset.InternalOpen;
var
   context: TRttiContext;
   field: TField;
   value: boolean;
begin
   value := true;
   for field in self.Fields do
      value := value and (field.FieldKind <> fkData);
   context.GetType(self.ClassInfo).GetField('FDefaultFields').SetValue(self, TValue.From(value));
   inherited InternalOpen;
end;

end.
