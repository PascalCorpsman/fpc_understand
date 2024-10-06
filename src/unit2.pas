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
    Button8: TButton;
    Button9: TButton;
    CheckListBox1: TCheckListBox;
    ColorDialog1: TColorDialog;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit5: TEdit;
    Edit6: TEdit;
    Edit7: TEdit;
    Edit8: TEdit;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    ListBox1: TListBox;
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
    TabSheet3: TTabSheet;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure Button4Click(Sender: TObject);
    Procedure Button5Click(Sender: TObject);
    Procedure Button6Click(Sender: TObject);
    Procedure Button7Click(Sender: TObject);
    Procedure Button8Click(Sender: TObject);
    Procedure Button9Click(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure Shape1ChangeBounds(Sender: TObject);
    Procedure ShapeClick(Sender: TObject);
  private
    fProject: TProject;
    fList: TFileList; // Dateiliste des Projekts "Absolute" Dateinamen
    fpathList: TPathList;
    Procedure UpdateFileList();
    Procedure UpadeteSearchPathList();
    Procedure AddSearchPathToProject(Const Searchpath: String);
  public
    Procedure LoadProjectToLCL(Const aProject: TProject);
    Procedure GetProjectFromLCL(Const aProject: TProject);
    Function SelectLPIAndShow: Boolean;
  End;

Var
  Form2: TForm2;

Implementation

{$R *.lfm}

Uses LCLVersion
  , LCLType
  , uDOMXML
  , Unit7 // Project File Edit dialog
  , Unit12 // Question Dialog "Add search paths"
  ;

{ TForm2 }

Procedure TForm2.FormCreate(Sender: TObject);
Begin
{$IFDEF Windows}
  PageControl1.TabPosition := tpTop;
{$ENDIF}
  Caption := 'Project Settings';
  Shape1.OnClick := @ShapeClick;
  Shape2.OnClick := @ShapeClick;
  Shape3.OnClick := @ShapeClick;
  Shape4.OnClick := @ShapeClick;
End;

Procedure TForm2.Button5Click(Sender: TObject);
Begin
  // Select Root Folder
  If SelectDirectoryDialog1.Execute Then Begin
    label6.Caption := ExcludeTrailingPathDelimiter(SelectDirectoryDialog1.FileName);
    UpdateFileList();
  End;
End;

Procedure TForm2.Button6Click(Sender: TObject);
Begin
  label2.Caption := '';
  UpdateFileList();
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
    UpdateFileList();
  End
  Else Begin
    ShowMessage('Error, could not locate: ' + lpiFile);
  End;
End;

Procedure TForm2.AddSearchPathToProject(Const Searchpath: String);
Var
  i: Integer;
  p, lpiFile, val, PDelim: String;
  DOMLPIFile: TDOMXML;
  SearchPaths, OtherUnitFiles: TDomNode;
Begin
  For i := 0 To high(fpathList) Do Begin
    If IncludeTrailingPathDelimiter(fpathList[i].Path) = IncludeTrailingPathDelimiter(Searchpath) Then Begin
      showmessage('Error, path: "' + IncludeTrailingPathDelimiter(Searchpath) + '" already in list.');
      exit;
    End;
  End;
  lpiFile := ConcatRelativePath(Label6.Caption, Label2.Caption);
  form12.Label2.caption := ExtractRelativePath(IncludeTrailingPathDelimiter(label6.Caption), IncludeTrailingPathDelimiter(Searchpath));
  If FileExists(LPIFile) Then Begin
    form12.ModalResult := mrNone;
    form12.RadioButton1.Checked := true; // Default ab ins LPI
    Form12.ShowModal;
  End
  Else Begin
    Form12.ModalResult := mrOK;
    form12.RadioButton1.Checked := false; // es gibt kein LPI -> Also ins Projekt
  End;
  If Form12.ModalResult = mrOK Then Begin
    If form12.RadioButton1.Checked Then Begin
      // Add to LPI Suchpfaden
      DOMLPIFile := TDOMXML.Create;
      DOMLPIFile.LoadFromFile(lpiFile);
      PDelim := GetPathDelimFromLPIFile(DOMLPIFile);
      p := ExcludeTrailingPathDelimiter(form12.Label2.caption);
      If PathDelim <> PDelim Then Begin
        p := StringReplace(p, PathDelim, PDelim, [rfReplaceAll]);
      End;
      SearchPaths := DOMLPIFile.DocumentElement.FindPath('CONFIG.CompilerOptions.SearchPaths');
      OtherUnitFiles := SearchPaths.FindPath('OtherUnitFiles');
      If assigned(OtherUnitFiles) Then Begin
        val := OtherUnitFiles.AttributeValue['Value'];
        val := val + ';' + p;
        OtherUnitFiles.AttributeValue['Value'] := val;
      End
      Else Begin
        OtherUnitFiles := SearchPaths.AddChild('OtherUnitFiles', '');
        OtherUnitFiles.AddAttribute('Value', p);
      End;
      DOMLPIFile.SaveToFile(lpiFile);
      DOMLPIFile.Free;
    End
    Else Begin
      // Add to FPU-Projekt Suchpfaden
      setlength(fpathList, length(fpathList) + 1);
      fpathList[high(fpathList)].FromLPI := false;
      fpathList[high(fpathList)].Path := IncludeTrailingPathDelimiter(Searchpath);
    End;
  End;
  UpadeteSearchPathList;
End;

Procedure TForm2.Button8Click(Sender: TObject);
Begin
  // Add search path
  If SelectDirectoryDialog1.Execute Then Begin
    AddSearchPathToProject(SelectDirectoryDialog1.FileName);
  End;
End;

Procedure TForm2.Button9Click(Sender: TObject);
Var
  j, i: Integer;
  domLPIFile: TDOMXML;
  p, lpiFile, fp, s: String;
  found: Boolean;
  units, unitfile, fn, OtherUnitFiles: TDomNode;
  spa: TStringArray;
Begin
  // Remove search path
  If ListBox1.ItemIndex = -1 Then Begin
    showmessage('Nothing selected.');
    exit;
  End;
  p := ExcludeTrailingPathDelimiter(ListBox1.Items[ListBox1.ItemIndex]);
  If fpathList[ListBox1.ItemIndex].FromLPI Then Begin
    lpiFile := ConcatRelativePath(Label6.Caption, Label2.Caption);
    DOMLPIFile := TDOMXML.Create;
    DOMLPIFile.LoadFromFile(lpiFile);
    found := false;
    units := DOMLPIFile.DocumentElement.FindPath('CONFIG.ProjectOptions.units');
    unitfile := Nil;
    If assigned(units) Then unitfile := units.FirstChild;
    While assigned(unitfile) Do Begin
      fn := unitfile.FindNode('Filename');
      fp := ExcludeTrailingPathDelimiter(ExtractFilePath(fn.AttributeValue['Value']));
      unitfile := units.NextSibling;
      If p = fp Then Begin
        found := true;
        unitfile := Nil;
      End;
    End;
    If found Then Begin
      If id_no = Application.MessageBox(pchar('Searchpath "' + p + '" is in use, remove anyway?'), 'Question', MB_ICONQUESTION Or MB_YESNO) Then Begin
        domLPIFile.Free;
        exit;
      End;
    End;
    OtherUnitFiles := DOMLPIFile.DocumentElement.FindPath('CONFIG.CompilerOptions.SearchPaths.OtherUnitFiles');
    If Not assigned(OtherUnitFiles) Then Begin
      domLPIFile.Free;
      exit;
    End;
    spa := OtherUnitFiles.AttributeValue['Value'].Split(';');
    For i := 0 To high(spa) Do Begin
      If spa[i] = p Then Begin
        For j := i To high(spa) - 1 Do Begin
          spa[j] := spa[j + 1];
        End;
        setlength(spa, high(spa));
        break;
      End;
    End;
    s := '';
    For i := 0 To high(spa) Do Begin
      If s <> '' Then s := s + ';';
      s := s + spa[i];
    End;
    If s <> '' Then Begin
      OtherUnitFiles.AttributeValue['Value'] := s;
    End
    Else Begin
      OtherUnitFiles.Free;
    End;
    DOMLPIFile.SaveToFile(lpiFile);
    DOMLPIFile.Free;
  End
  Else Begin
    found := false;
    For i := 0 To CheckListBox1.Items.Count - 1 Do Begin
      If CheckListBox1.Checked[i] Then Begin
        If pos(p, CheckListBox1.Items[i]) <> 0 Then Begin
          found := true;
          break;
        End;
      End;
    End;
    If found Then Begin
      If id_no = Application.MessageBox(pchar('Searchpath "' + p + '" is in use, remove anyway?'), 'Question', MB_ICONQUESTION Or MB_YESNO) Then Begin
        exit;
      End;
    End;
    For j := ListBox1.ItemIndex To high(fpathList) - 1 Do Begin
      fpathList[j] := fpathList[j + 1];
    End;
    SetLength(fpathList, high(fpathList));
  End;
  UpadeteSearchPathList;
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
    UpdateFileList();
    UpadeteSearchPathList();
  End;
End;

Procedure TForm2.Button3Click(Sender: TObject);
  Function IsFileInSearchPath(Const Filename: String): Boolean;
  Var
    fp: String;
    i: Integer;
  Begin
    fp := IncludeTrailingPathDelimiter(ExtractFilePath(Filename));
    result := false;
    For i := 0 To high(fpathList) Do Begin
      If fp = IncludeTrailingPathDelimiter(fpathList[i].Path) Then Begin
        result := true;
        exit;
      End;
    End;
  End;

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
      If Not IsFileInSearchPath(OpenDialog2.Files[i]) Then Begin
        AddSearchPathToProject(ExtractFilePath(OpenDialog2.Files[i]));
      End;
    End;
    UpdateFileList();
  End;
End;

Procedure TForm2.Button4Click(Sender: TObject);
Var
  i: Integer;
Begin
  // Remove file
  If CheckListBox1.ItemIndex = -1 Then Begin
    showmessage('Error, nothing selected.');
    exit;
  End;
  (*
   * Ist die Datei aus dem .lpi File -> Warnung kann nur deaktiviert werden
   * sonst raus damit
   *)
  If fList[CheckListBox1.ItemIndex].FromLPI Then Begin
    showmessage('The selected file is loaded through the .lpi file, it can only be disabled.' + LineEnding +
      'To remove the file use the "edit.lpi" button.');
    CheckListBox1.Checked[CheckListBox1.ItemIndex] := false;
    fList[CheckListBox1.ItemIndex].Enabled := false;
  End
  Else Begin
    // Eine Händisch hinzu gefügte Datei kann einfach gelöscht werden
    For i := CheckListBox1.ItemIndex To CheckListBox1.Count - 2 Do Begin
      fList[i] := fList[i + 1];
    End;
    setlength(fList, high(fList));
    CheckListBox1.Items.Delete(CheckListBox1.ItemIndex);
  End;
End;

Procedure TForm2.Button1Click(Sender: TObject);
Begin
  ModalResult := mrOK;
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
  // Nichts, das ist nur dass man schneller zum "Klick" kommt und der steht direct drüber ;)
End;

Procedure TForm2.UpdateFileList;
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

Procedure TForm2.UpadeteSearchPathList;
Var
  i: Integer;
Begin
  fpathList := GetSearchPathList(Label6.Caption, label2.Caption, fpathList);
  ListBox1.Items.BeginUpdate;
  ListBox1.Clear;
  For i := 0 To high(fpathList) Do Begin
    ListBox1.Items.Add(ExcludeTrailingPathDelimiter(ExtractRelativePath(IncludeTrailingPathDelimiter(Label6.Caption), fpathList[i].Path)));
  End;
  ListBox1.Items.EndUpdate;
End;

Procedure TForm2.LoadProjectToLCL(Const aProject: TProject);
Begin
  PageControl1.ActivePageIndex := 0; // Immer Reset auf die Generellen Einstellungen
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
  // Chart statistics
  edit5.Text := inttostr(aProject.ChartStatisticSettings.BoarderForLargestFunction);
  edit6.Text := inttostr(aProject.ChartStatisticSettings.BoarderForLargestFiles);
  edit7.Text := inttostr(aProject.ChartStatisticSettings.BoarderForMostComplexFunction);
  edit8.Text := inttostr(aProject.ChartStatisticSettings.BoarderForAverageMostComplexFiles);

  // Relativ zu intern "Absolut"
  fList := RelativeFileListToAbsoluteFileList(fProject.RootFolder, fProject.Files);
  UpdateFileList();

  fpathList := RelativePathListToAbsolutePathList(fProject.RootFolder, fProject.SearchPaths);
  UpadeteSearchPathList();
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
  c.LevelGood := strtointdef(Edit2.Text, Default_CCLevelGood);
  c.LevelModerate := strtointdef(Edit3.Text, Default_CCLevelModerate);
  c.LevelComplex := strtointdef(Edit4.Text, Default_CCLevelComplex);
  aProject.CCColors := c;

  // Chart statistics
  aProject.ChartStatisticSettings.BoarderForMostComplexFunction := strtointdef(edit7.Text, Default_CCLevelGood);
  aProject.ChartStatisticSettings.BoarderForLargestFunction := strtointdef(edit5.Text, Default_BoarderForLargestFunction);
  aProject.ChartStatisticSettings.BoarderForLargestFiles := strtointdef(edit6.Text, Default_BoarderForLargestFiles);
  aProject.ChartStatisticSettings.BoarderForAverageMostComplexFiles := strtointdef(edit8.Text, Default_BoarderForAverageMostComplexFiles);

  // Files
  aProject.RootFolder := IncludeTrailingPathDelimiter(label6.Caption);
  aProject.LPISource := label2.Caption;
  For i := 0 To high(fList) Do Begin
    fList[i].Enabled := CheckListBox1.Checked[i];
    // Wieder zu Relativ zurück wandeln
    fList[i].FileName := ExtractRelativePath(aProject.RootFolder, fList[i].FileName);
  End;
  aProject.Files := fList;

  // Suchpfade
  For i := 0 To high(fpathList) Do Begin
    fpathList[i].Path := ExcludeTrailingPathDelimiter(ExtractRelativePath(aProject.RootFolder, fpathList[i].Path));
  End;
  aProject.SearchPaths := fpathList;
End;

Function TForm2.SelectLPIAndShow: Boolean;
Begin
  Button2.Click;
  If label2.caption <> '' Then Begin
    PageControl1.ActivePage := TabSheet2;
    ShowModal;
    result := true;
  End
  Else Begin
    result := false;
  End;
End;

End.

