(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of FPC understand                                        *)
(*                                                                            *)
(*  See the file license.md, located under:                                   *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(******************************************************************************)
Unit Unit2;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  CheckLst, ExtCtrls, ufpc_understand;

Type

  { TForm2 }

  TForm2 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    CheckListBox1: TCheckListBox;
    ColorDialog1: TColorDialog;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label10: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    OpenDialog1: TOpenDialog;
    OpenDialog2: TOpenDialog;
    PageControl1: TPageControl;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    Shape1: TShape;
    Shape2: TShape;
    Shape3: TShape;
    Shape4: TShape;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TreeView1: TTreeView;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure Button4Click(Sender: TObject);
    Procedure Button5Click(Sender: TObject);
    Procedure Button6Click(Sender: TObject);
    Procedure Button7Click(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure Shape1ChangeBounds(Sender: TObject);
    Procedure TreeView1Click(Sender: TObject);
    Procedure ShapeClick(Sender: TObject);
  private
    fProject: TProject;
    fList: TFileList;
    Procedure UpdateCheckListbox();
  public
    Procedure LoadProjectToLCL(Const aProject: TProject);
    Procedure GetProjectFromLCL(Const aProject: TProject);
  End;

Var
  Form2: TForm2;

Implementation

{$R *.lfm}

Uses unit7, LCLVersion;

{ TForm2 }

Procedure TForm2.FormCreate(Sender: TObject);
Begin
  Caption := 'Project Settings';
  PageControl1.ShowTabs := false;
  Shape1.OnClick := @ShapeClick;
  Shape2.OnClick := @ShapeClick;
  Shape3.OnClick := @ShapeClick;
  Shape4.OnClick := @ShapeClick;
{$IF lcl_fullversion >= 2030000}
  TreeView1.ShowSeparators := false;
{$ELSE}
  TreeView1.Options := TreeView1.Options - [tvoShowSeparators];
{$ENDIF}
End;

Procedure TForm2.Button5Click(Sender: TObject);
Begin
  // Select Root Folder
  If SelectDirectoryDialog1.Execute Then Begin
    label6.Caption := ExcludeTrailingPathDelimiter(SelectDirectoryDialog1.FileName);
    UpdateCheckListbox();
  End;
End;

Procedure TForm2.Button6Click(Sender: TObject);
Begin
  label2.Caption := '';
  UpdateCheckListbox();
End;

Procedure TForm2.Button7Click(Sender: TObject);
Var
  lpiFile: String;
Begin
  // Edit .lpi File
  lpiFile := ConcatRelativePath(Label6.Caption, Label2.Caption);
  //  -> Hinzufügen / Entfernen von Dateien, Kompatibel zum .lpi Dateiformat
  If FileExists(lpiFile) Then Begin
    form7.LoadLPIFile(lpiFile);
    form7.ShowModal;
    UpdateCheckListbox();
  End
  Else Begin
    ShowMessage('Error, could not locate: ' + lpiFile);
  End;
End;

Procedure TForm2.Button2Click(Sender: TObject);
Begin
  // Select .lpi File
  If OpenDialog1.Execute Then Begin
    If label6.caption = '' Then Begin
      label6.caption := ExcludeTrailingPathDelimiter(ExtractFilePath(OpenDialog1.FileName));
      label2.caption := ExtractFileName(OpenDialog1.FileName);
    End
    Else Begin
      label2.caption := ExtractRelativePath(IncludeTrailingPathDelimiter(Label6.Caption), OpenDialog1.FileName);
      If label6.caption <> ExcludeTrailingPathDelimiter(ExtractFilePath(OpenDialog1.FileName)) Then Begin
        showmessage('Warning, setting the project root folder other than the .lpi folder can cause unwanted behavior.');
      End;
    End;
    UpdateCheckListbox();
  End;
End;

Procedure TForm2.Button3Click(Sender: TObject);
Var
  i: Integer;
Begin
  OpenDialog2.InitialDir := Label6.Caption;
  If OpenDialog2.Execute Then Begin
    For i := 0 To OpenDialog2.Files.Count - 1 Do Begin
      setlength(fList, high(fList) + 2);
      fList[high(fList)].FileName := OpenDialog2.Files[i];
      fList[high(fList)].FromLPI := false;
      fList[high(fList)].Enabled := true;
    End;
    UpdateCheckListbox();
  End;
End;

Procedure TForm2.Button4Click(Sender: TObject);
Begin
  // Remove file
  showmessage('Todo.');
  (*
   * Ist die Datei aus dem .lpi File -> Warnung kann nur deaktiviert werden
   * sonst raus damit
   *)
End;

Procedure TForm2.Button1Click(Sender: TObject);
Begin
  ModalResult := mrOK;
End;

Procedure TForm2.TreeView1Click(Sender: TObject);
Begin
  If Not assigned(TreeView1.Selected) Then exit;
  Case TreeView1.Selected.Text Of
    'General': PageControl1.ActivePageIndex := 0;
    'Files': PageControl1.ActivePageIndex := 1;
  End;
End;

Procedure TForm2.ShapeClick(Sender: TObject);
Begin
  ColorDialog1.Color := TShape(sender).Brush.Color;
  If ColorDialog1.Execute Then Begin
    TShape(sender).Brush.Color := ColorDialog1.Color;
  End;
End;

Procedure TForm2.Shape1ChangeBounds(Sender: TObject);
Begin
  // Nichts, das ist nur dass man schneller zum "Klick" kommt  und der steht direct drüber ;)
End;

Procedure TForm2.UpdateCheckListbox;
Var
  i: Integer;
Begin
  fList := GetFileList(Label6.Caption, label2.Caption, fList);
  CheckListBox1.items.BeginUpdate;
  CheckListBox1.Clear;
  For i := 0 To high(fList) Do Begin
    CheckListBox1.Items.Add(ExtractRelativePath(IncludeTrailingPathDelimiter(Label6.Caption), fList[i].FileName));
    CheckListBox1.Checked[i] := fList[i].Enabled;
  End;
  CheckListBox1.items.EndUpdate;
End;

Procedure TForm2.LoadProjectToLCL(Const aProject: TProject);
Begin
  PageControl1.ActivePageIndex := 0; // Immer Reset auf die Generellen Einstellungen
  TreeView1.Selected := TreeView1.Items[0];
  fProject := aProject;
  // General
  edit1.text := aProject.Name;
  Shape1.Brush.Color := aProject.CCColors.ColorGood;
  Shape2.Brush.Color := aProject.CCColors.ColorModerate;
  Shape3.Brush.Color := aProject.CCColors.ColorComplex;
  Shape4.Brush.Color := aProject.CCColors.ColorUnstable;
  edit2.text := inttostr(aProject.CCColors.LevelGood);
  edit3.text := inttostr(aProject.CCColors.LevelModerate);
  edit4.text := inttostr(aProject.CCColors.LevelComplex);
  // Files
  label6.Caption := ExcludeTrailingPathDelimiter(aProject.RootFolder);
  label2.Caption := aProject.LPISource;
  fList := aProject.Files;
  // Relativ zo intern "Absolut"
  fList := RelativeFileListToAbsoluteFileList(fProject.RootFolder, fProject.Files);
  UpdateCheckListbox();
End;

Procedure TForm2.GetProjectFromLCL(Const aProject: TProject);
Var
  i: Integer;
  c: TCCColors;
Begin
  // General
  aProject.Name := edit1.text;
  c.ColorGood := Shape1.Brush.Color;
  c.ColorModerate := Shape2.Brush.Color;
  c.ColorComplex := Shape3.Brush.Color;
  c.ColorUnstable := Shape4.Brush.Color;
  c.LevelGood := strtointdef(Edit2.Text, 10);
  c.LevelModerate := strtointdef(Edit3.Text, 20);
  c.LevelComplex := strtointdef(Edit4.Text, 50);
  aProject.CCColors := c;
  // Files
  aProject.RootFolder := IncludeTrailingPathDelimiter(label6.Caption);
  aProject.LPISource := label2.Caption;
  For i := 0 To high(fList) Do Begin
    fList[i].Enabled := CheckListBox1.Checked[i];
    // Wieder zu Relativ zurück wandeln
    fList[i].FileName := ExtractRelativePath(aProject.RootFolder, fList[i].FileName);
  End;
  aProject.Files := fList;
End;

End.

