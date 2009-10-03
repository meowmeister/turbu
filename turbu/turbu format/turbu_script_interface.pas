unit turbu_script_interface;

interface
uses
   uPSCompiler, uPSRuntime, turbu_classes, turbu_unit_dictionary;

type
   IScriptEngine = interface(IInterface)
   ['{60799F2F-0C1F-491C-869D-1996AF8D858F}']
      function getExec: TPSExec;
      function getDeclarations: TDeclList;
      procedure setDBScript(const value: ansiString);
      function getUnits: TUnitDictionary;
      procedure setUnits(const value: TUnitDictionary);

      property exec: TPSExec read getExec;
      property decl: TDeclList read getDeclarations;
      property dbScript: ansiString write setDBScript;
      property units: TUnitDictionary read getUnits write setUnits;
   end;
implementation

end.
