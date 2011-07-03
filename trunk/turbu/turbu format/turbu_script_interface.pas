unit turbu_script_interface;

interface
uses
   turbu_classes, dm_database;

type
   IScriptEngine = interface(IInterface)
   ['{60799F2F-0C1F-491C-869D-1996AF8D858F}']
      function getExecMethod(const name: string): TMethod;
      function getDeclarations: TDeclList;
      procedure setDBScript(const value: ansiString);

      property decl: TDeclList read getDeclarations;
      property dbScript: ansiString write setDBScript;
   end;

   IDesignScriptEngine = interface(IScriptEngine)
   ['{A3B2746C-F3D1-4B61-B9D8-9D8E82229979}']
      procedure upload(db: TdmDatabase);
      function GetFunctions(ResultType: integer): TDeclList;
      function FindFunction(name: string): TRpgDecl;
   end;
implementation

end.
