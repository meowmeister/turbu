unit turbu_script_interface;

interface
uses
   uPSCompiler, uPSRuntime;
type
   IScriptEngine = interface(IInterface)
   ['{60799F2F-0C1F-491C-869D-1996AF8D858F}']
      function getExec: TPSExec;
      property exec: TPSExec read getExec;
   end;
implementation

end.
