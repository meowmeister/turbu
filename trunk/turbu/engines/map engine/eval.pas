unit eval;

interface

uses
  SysUtils, Forms, Menus, ExtCtrls, StdCtrls, ComCtrls, Classes, Controls,
  Graphics, // system libs
  UnframedRadioGroup; // custom control


type
   TfrmMapProperties = class(Tform)
      btnOK: TButton;
      pnlBack: TPanel;
      grpMonsters: TGroupBox;
      grpChipset: TGroupBox;
      grpMapsize: TGroupBox;
      lblX: TLabel;
      grpScrollType: TGroupBox;
      dboxScrollType: TComboBox;
      grpName: TGroupBox;
      lvwEncounterList: TListView;
      bvlLine: TBevel;
      lblSteps: TLabel;
      grpPano: TGroupBox;
      chkUsePano: TCheckBox;
      grpSelectGraphic: TGroupBox;
      imgPano: TImage;
      btnSet: TButton;
      grpOption: TGroupBox;
      bvlLine2: TBevel;
      chkHorizontal: TCheckBox;
      chkAutoHoriz: TCheckBox;
      chkVertical: TCheckBox;
      chkAutoVert: TCheckBox;
      spinWidth: TUpDown;
      spinHeight: TUpDown;
      spnAutoVert: TUpDown;
      spnAutoHoriz: TUpDown;
      spinSteps: TUpDown;
      lblSpeed: TLabel;
      lblSpeed2: TLabel;
      grpBGM: TGroupBox;
      btnSetBGMTo: TButton;
      grpBattleBG: TGroupBox;
      txtWidth: TEdit;
      txtHeight: TEdit;
      txtName: TEdit;
      txtSteps: TEdit;
      txtAutoHoriz: TEdit;
      txtAutoVert: TEdit;
      txtSetBGMTo: TEdit;
      txtSetBBGTo: TEdit;
      txtPanoName: TEdit;
      btnSetBBGTo: TButton;
      menuBar: TMainMenu;
      mnuFile: TMenuItem;
      optOpen: TMenuItem;
      optExit: TMenuItem;
      grpTeleport: TRadioGroup;
      grpEscape: TRadioGroup;
      grpSave: TRadioGroup;
      btnBack: TButton;
      btnForward: TButton;
      cbxNav: TComboBox;
      cbxChipSets: TComboBox;
      urgBBG: TUnframedRadioGroup;
      mnuView: TMenuItem;
      optTiles: TMenuItem;
      optEvents: TMenuItem;
      optGlobalEvents: TMenuItem;
      urgBGM: TUnframedRadioGroup;
   end;

var
   frmMapProperties: TfrmMapProperties;

implementation

{$R *.dfm}

end.
