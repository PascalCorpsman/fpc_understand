Unit Unit11;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Buttons, TAGraph, TASeries, TATools, usunburstchart, ufpc_understand,
  ucirclepackchart, Types;

Const
  // !! Beide Arrays benötigen die selbe Länge !!
  //                                                Grün,     Blau,      Orange,    Rot
  SelectedDirectoryColors: Array[0..3] Of TColor = ($0070D8AF, $00B98C4E, $0056ADFC, $002157E4);
  NormalDirectoryColors: Array[0..3] Of TColor = ($00C4E2CD, $00EADDCA, $00CDE7FE, $00BDCDF7);

  //                                                  Hellblau,  Dunkelblau,HellGrün,  Dunkelgrün,Orange,    Rot
  MostComplexFunctionColors: Array[0..5] Of TColor = ($00E4C490, $00B98C4E, $0070D8AF, $00389F56, $0056ADFC, $002157E4);

Type

  TDirInfo = Record
    isDir: Boolean;
    Info: String;
    aColorIndex: Integer;
  End;

  PDirInfo = ^TDirInfo;

  TMCFunctionsInfo = Record
    Info: String;
  End;

  PMCFunctionsInfo = ^TMCFunctionsInfo;

  TMCFilessInfo = Record
    Info: String;
  End;

  PMCFilessInfo = ^TMCFilessInfo;

  { TForm11 }

  TForm11 = Class(TForm)
    Chart1: TChart;
    Chart1BarSeries1: TBarSeries;
    Chart2: TChart;
    Chart2BarSeries1: TBarSeries;
    ChartToolset1: TChartToolset;
    ChartToolset1DataPointHintTool1: TDataPointHintTool;
    ChartToolset1DataPointHintTool2: TDataPointHintTool;
    ChartToolset2: TChartToolset;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    GroupBox5: TGroupBox;
    GroupBox6: TGroupBox;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label2: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    LinebreakChart1: TSunburstChart;
    DirectoryChart1: TSunburstChart;
    MostComplexFunctions1: TPackedCircleChart;
    MostComplexFiles1: TPackedCircleChart;
    Shape1: TShape;
    Shape2: TShape;
    Shape3: TShape;
    ToggleBox1: TToggleBox;
    ToggleBox2: TToggleBox;
    ToggleBox3: TToggleBox;
    ToggleBox4: TToggleBox;
    ToggleBox5: TToggleBox;
    ToggleBox6: TToggleBox;
    Procedure ChartToolset1DataPointHintTool1Hint(ATool: TDataPointHintTool;
      Const APoint: TPoint; Var AHint: String);
    Procedure ChartToolset1DataPointHintTool2Hint(ATool: TDataPointHintTool;
      Const APoint: TPoint; Var AHint: String);
    Procedure FormCreate(Sender: TObject);
    Procedure OnLineBreakChart1Resize(Sender: TObject);
    Procedure OnLineBreakChart1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    Procedure OnLineBreakChart1ShowHint(Sender: TObject; HintInfo: PHintInfo);
    Procedure OnLineBreakChart1DeleteElementsUserData(Sender: TObject; aUserData: Pointer);
    Procedure OnDirectoryChart1Resize(Sender: TObject);
    Procedure OnDirectoryChart1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    Procedure OnDirectoryChart1DeleteElementsUserData(Sender: TObject; aUserData: Pointer);
    Procedure OnDirectoryChart1AfterPaint(Sender: TObject);
    Procedure OnMostComplexFunctions1DeleteElementsUserData(Sender: TObject; aUserData: Pointer);
    Procedure OnMostComplexFunctions1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    Procedure OnMostComplexFunctions1AfterPaint(Sender: TObject);
    Procedure OnMostComplexFiles1DeleteElementsUserData(Sender: TObject; aUserData: Pointer);
    Procedure OnMostComplexFiles1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    Procedure OnMostComplexFiles1AfterPaint(Sender: TObject);

    Procedure ToggleBox1Click(Sender: TObject);
    Procedure ToggleBox2Click(Sender: TObject);
    Procedure ToggleBox3Click(Sender: TObject);
    Procedure ToggleBox4Change(Sender: TObject);
    Procedure ToggleBox5Change(Sender: TObject);
    Procedure ToggleBox6Change(Sender: TObject);
  private
    fLBMousePos: TPoint;
    fDSMousePos: TPoint;
    fdsCursorElement: PSunBurstChartElement;
    fmcfuMousePos: TPoint;
    fmcfuCursorElement: PCircle;
    fmcfiMousePos: TPoint;
    fmcfiCursorElement: PCircle;

    Procedure InsertFileInfo(RootNode: PSunBurstChartElement; PathToFile: String; Const aInfo: TProjectFileInfo);

    Procedure InitLineBreakdown(aList: TProjectFilesInfo; Const aProject: TProject);
    Procedure InitDirectoryStructure(aList: TProjectFilesInfo; Const aProject: TProject);
    Procedure InitMostComplexFunctions(aList: TProjectFilesInfo; Const aProject: TProject);
    Procedure InitMostComplexFiles(aList: TProjectFilesInfo; Const aProject: TProject);
    Procedure InitLargestFunctions(aList: TProjectFilesInfo; Const aProject: TProject);
    Procedure InitLargestFiles(aList: TProjectFilesInfo; Const aProject: TProject);
  public
    Procedure InitCharts(aList: TProjectFilesInfo; Const aProject: TProject);

  End;

Var
  Form11: TForm11;

Implementation

{$R *.lfm}

Uses math;

{ TForm11 }

Procedure TForm11.FormCreate(Sender: TObject);
Begin
  caption := 'Chart statistics';
  LinebreakChart1 := TSunburstChart.Create(GroupBox1);
  LinebreakChart1.Name := 'LinebreakChart1';
  LinebreakChart1.Parent := GroupBox1;
  LinebreakChart1.Left := 8;
  LinebreakChart1.Width := GroupBox1.Width - 16;
  LinebreakChart1.Top := Shape3.Top + Shape3.Height + 8;
  LinebreakChart1.Height := GroupBox1.ClientHeight - LinebreakChart1.Top - 8;
  LinebreakChart1.Anchors := [akLeft, akRight, akTop, akBottom];
  LinebreakChart1.OnResize := @OnLineBreakChart1Resize;
  LinebreakChart1.OnMouseMove := @OnLineBreakChart1MouseMove;
  LinebreakChart1.OnShowHint := @OnLineBreakChart1ShowHint;
  LinebreakChart1.ShowHint := true;
  LinebreakChart1.OnResize(Nil);
  LinebreakChart1.OnDeleteElementsUserData := @OnLineBreakChart1DeleteElementsUserData;

  DirectoryChart1 := TSunburstChart.Create(GroupBox2);
  DirectoryChart1.Name := 'DirectoryChart1';
  DirectoryChart1.Parent := GroupBox2;
  DirectoryChart1.Left := 8;
  DirectoryChart1.Width := GroupBox2.Width - 16;
  DirectoryChart1.Top := label9.Top + Label9.Height + 8;
  DirectoryChart1.Height := GroupBox2.ClientHeight - DirectoryChart1.Top - 8;
  DirectoryChart1.Anchors := [akLeft, akRight, akTop, akBottom];
  DirectoryChart1.OnResize := @OnDirectoryChart1Resize;
  DirectoryChart1.OnMouseMove := @OnDirectoryChart1MouseMove;
  DirectoryChart1.OnResize(Nil);
  DirectoryChart1.OnDeleteElementsUserData := @OnDirectoryChart1DeleteElementsUserData;
  DirectoryChart1.OnAfterPaint := @OnDirectoryChart1AfterPaint;
  fdsCursorElement := Nil;

  MostComplexFunctions1 := TPackedCircleChart.Create(GroupBox3);
  MostComplexFunctions1.Name := 'MostComplexFunctions1';
  MostComplexFunctions1.Parent := GroupBox3;
  MostComplexFunctions1.Left := 8;
  MostComplexFunctions1.Width := GroupBox3.Width - 16;
  MostComplexFunctions1.Top := label11.Top + Label11.Height + 8;
  MostComplexFunctions1.Height := GroupBox3.ClientHeight - MostComplexFunctions1.Top - 8;
  MostComplexFunctions1.Anchors := [akLeft, akRight, akTop, akBottom];
  MostComplexFunctions1.OnMouseMove := @OnMostComplexFunctions1MouseMove;
  MostComplexFunctions1.OnDeleteCirclesUserData := @OnMostComplexFunctions1DeleteElementsUserData;
  MostComplexFunctions1.OnAfterPaint := @OnMostComplexFunctions1AfterPaint;
  fmcfuCursorElement := Nil;

  MostComplexFiles1 := TPackedCircleChart.Create(GroupBox5);
  MostComplexFiles1.Name := 'MostComplexFiles1';
  MostComplexFiles1.Parent := GroupBox5;
  MostComplexFiles1.Left := 8;
  MostComplexFiles1.Width := GroupBox5.Width - 16;
  MostComplexFiles1.Top := label18.Top + Label18.Height + 8;
  MostComplexFiles1.Height := GroupBox5.ClientHeight - MostComplexFiles1.Top - 8;
  MostComplexFiles1.Anchors := [akLeft, akRight, akTop, akBottom];
  MostComplexFiles1.OnMouseMove := @OnMostComplexFiles1MouseMove;
  MostComplexFiles1.OnDeleteCirclesUserData := @OnMostComplexFiles1DeleteElementsUserData;
  MostComplexFiles1.OnAfterPaint := @OnMostComplexFiles1AfterPaint;
  fmcfiCursorElement := Nil;
End;

Procedure TForm11.ChartToolset1DataPointHintTool1Hint(
  ATool: TDataPointHintTool; Const APoint: TPoint; Var AHint: String);
Begin
  AHint := Chart1BarSeries1.Source.Item[ATool.PointIndex]^.Text;
End;

Procedure TForm11.ChartToolset1DataPointHintTool2Hint(
  ATool: TDataPointHintTool; Const APoint: TPoint; Var AHint: String);
Begin
  AHint := Chart2BarSeries1.Source.Item[ATool.PointIndex]^.Text;
End;

Procedure TForm11.InitCharts(aList: TProjectFilesInfo; Const aProject: TProject
  );
Begin
  InitLineBreakdown(alist, aProject);
  InitDirectoryStructure(alist, aProject);
  InitMostComplexFunctions(alist, aProject);
  InitLargestFunctions(alist, aProject);
  InitMostComplexFiles(alist, aProject);
  InitLargestFiles(alist, aProject);
  ToggleBox1.Checked := false;
  ToggleBox2.Checked := false;
  ToggleBox3.Checked := false;
  ToggleBox4.Checked := false;
  ToggleBox5.Checked := false;
  ToggleBox6.Checked := false;
  GroupBox1.Visible := true;
  GroupBox2.Visible := true;
  GroupBox3.Visible := true;
  GroupBox4.Visible := true;
  GroupBox5.Visible := true;
  GroupBox6.Visible := true;
End;

Procedure TForm11.OnLineBreakChart1Resize(Sender: TObject);
Var
  v: integer;
Begin
  v := min(LinebreakChart1.Width Div 2, LinebreakChart1.Height - 16);
  LinebreakChart1.PieCenter := point(v, max(v, LinebreakChart1.Height Div 2));
  LinebreakChart1.PieRadius := v - 10;
End;

Procedure TForm11.OnLineBreakChart1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
Begin
  fLBMousePos := point(x, y);
End;

Procedure TForm11.OnLineBreakChart1ShowHint(Sender: TObject; HintInfo: PHintInfo
  );
Var
  p: PSunBurstChartElement;
Begin
  If LinebreakChart1.GetSegmentAtPos(fLBMousePos.x, fLBMousePos.y, p) Then Begin
    HintInfo^.HintStr := pchar(p^.UserData);
  End
  Else Begin
    HintInfo^.HintStr := '';
  End;
End;

Procedure TForm11.OnLineBreakChart1DeleteElementsUserData(Sender: TObject;
  aUserData: Pointer);
Begin
  If assigned(aUserData) Then Begin
    pchar(aUserData) := '';
  End;
End;

Procedure TForm11.OnDirectoryChart1Resize(Sender: TObject);
Var
  v: integer;
Begin
  v := min(DirectoryChart1.Width Div 2, DirectoryChart1.Height Div 2);
  DirectoryChart1.PieCenter := point(DirectoryChart1.Width Div 2, DirectoryChart1.Height Div 2);
  DirectoryChart1.PieRadius := v - 10;
End;

Procedure TForm11.OnDirectoryChart1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
Var
  old, p: PSunBurstChartElement;
Begin
  fDSMousePos := point(x, y);
  old := fdsCursorElement;
  If Not DirectoryChart1.GetSegmentAtPos(x, y, fdsCursorElement) Then Begin
    fdsCursorElement := Nil;
  End;
  If old <> fdsCursorElement Then Begin
    DirectoryChart1.DeselectAll;
    p := fdsCursorElement;
    While assigned(p) Do Begin
      p^.Selected := true;
      p := p^.Parent;
    End;
    DirectoryChart1.Invalidate;
  End;
End;

Procedure TForm11.OnDirectoryChart1DeleteElementsUserData(Sender: TObject;
  aUserData: Pointer);
Var
  p: PDirInfo;
Begin
  p := aUserData;
  Dispose(p);
End;

Procedure TForm11.OnDirectoryChart1AfterPaint(Sender: TObject);
Var
  tw: Integer;
  s: String;
Begin
  If assigned(fdsCursorElement) Then Begin
    If assigned(fdsCursorElement^.UserData) Then Begin
      DirectoryChart1.Canvas.Pen.Color := clBlack;
      DirectoryChart1.Canvas.Brush.Color := clBlack;
      DirectoryChart1.Canvas.Font.Color := clWhite;
      s := PDirInfo(fdsCursorElement^.UserData)^.Info;
      tw := DirectoryChart1.Canvas.TextWidth(s);
      DirectoryChart1.Canvas.TextOut(min(fDSMousePos.x, DirectoryChart1.Width - tw), fDSMousePos.y, s);
    End;
  End;
End;

Procedure TForm11.OnMostComplexFunctions1DeleteElementsUserData(
  Sender: TObject; aUserData: Pointer);
Var
  p: PMCFunctionsInfo;
Begin
  If assigned(aUserData) Then Begin
    p := PMCFunctionsInfo(aUserData);
    Dispose(p);
  End;
End;

Procedure TForm11.OnMostComplexFunctions1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
Var
  old: PCircle;
Begin
  fmcfuMousePos := point(x, y);
  old := fmcfuCursorElement;
  If Not MostComplexFunctions1.GetCircleAtPos(x, y, fmcfuCursorElement) Then Begin
    fmcfuCursorElement := Nil;
  End;
  If old <> fmcfuCursorElement Then Begin
    MostComplexFunctions1.Invalidate;
  End;
End;

Procedure TForm11.OnMostComplexFunctions1AfterPaint(Sender: TObject);
Var
  tw: Integer;
  s: String;
  p: PMCFunctionsInfo;
Begin
  If assigned(fmcfuCursorElement) Then Begin
    If assigned(fmcfuCursorElement^.UserData) Then Begin
      p := PMCFunctionsInfo(fmcfuCursorElement^.UserData);
      MostComplexFunctions1.Canvas.Pen.Color := clBlack;
      MostComplexFunctions1.Canvas.Brush.Color := clBlack;
      MostComplexFunctions1.Canvas.Font.Color := clWhite;
      s := p^.Info;
      tw := MostComplexFunctions1.Canvas.TextWidth(s);
      MostComplexFunctions1.Canvas.TextOut(min(fmcfuMousePos.x, MostComplexFunctions1.Width - tw), fmcfuMousePos.y, s);
    End;
  End;
End;

Procedure TForm11.OnMostComplexFiles1DeleteElementsUserData(Sender: TObject;
  aUserData: Pointer);
Var
  p: PMCFilessInfo;
Begin
  If assigned(aUserData) Then Begin
    p := PMCFilessInfo(aUserData);
    Dispose(p);
  End;
End;

Procedure TForm11.OnMostComplexFiles1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
Var
  old: PCircle;
Begin
  fmcfiMousePos := point(x, y);
  old := fmcfiCursorElement;
  If Not MostComplexFiles1.GetCircleAtPos(x, y, fmcfiCursorElement) Then Begin
    fmcfiCursorElement := Nil;
  End;
  If old <> fmcfiCursorElement Then Begin
    MostComplexFiles1.Invalidate;
  End;
End;

Procedure TForm11.OnMostComplexFiles1AfterPaint(Sender: TObject);
Var
  tw: Integer;
  s: String;
  p: PMCFilessInfo;
Begin
  If assigned(fmcfiCursorElement) Then Begin
    If assigned(fmcfiCursorElement^.UserData) Then Begin
      p := PMCFilessInfo(fmcfiCursorElement^.UserData);
      MostComplexFiles1.Canvas.Pen.Color := clBlack;
      MostComplexFiles1.Canvas.Brush.Color := clBlack;
      MostComplexFiles1.Canvas.Font.Color := clWhite;
      s := p^.Info;
      tw := MostComplexFiles1.Canvas.TextWidth(s);
      MostComplexFiles1.Canvas.TextOut(min(fmcfiMousePos.x, MostComplexFiles1.Width - tw), fmcfiMousePos.y, s);
    End;
  End;
End;

Procedure TForm11.ToggleBox1Click(Sender: TObject);
Begin
  GroupBox2.Visible := Not ToggleBox1.Checked;
  GroupBox3.Visible := Not ToggleBox1.Checked;
  GroupBox4.Visible := Not ToggleBox1.Checked;
  GroupBox5.Visible := Not ToggleBox1.Checked;
  GroupBox6.Visible := Not ToggleBox1.Checked;
End;

Procedure TForm11.ToggleBox2Click(Sender: TObject);
Begin
  GroupBox1.Visible := Not ToggleBox2.Checked;
  GroupBox3.Visible := Not ToggleBox2.Checked;
  GroupBox4.Visible := Not ToggleBox2.Checked;
  GroupBox5.Visible := Not ToggleBox2.Checked;
  GroupBox6.Visible := Not ToggleBox2.Checked;
End;

Procedure TForm11.ToggleBox3Click(Sender: TObject);
Begin
  GroupBox1.Visible := Not ToggleBox3.Checked;
  GroupBox2.Visible := Not ToggleBox3.Checked;
  GroupBox4.Visible := Not ToggleBox3.Checked;
  GroupBox5.Visible := Not ToggleBox3.Checked;
  GroupBox6.Visible := Not ToggleBox3.Checked;
End;

Procedure TForm11.ToggleBox4Change(Sender: TObject);
Begin
  GroupBox1.Visible := Not ToggleBox4.Checked;
  GroupBox2.Visible := Not ToggleBox4.Checked;
  GroupBox3.Visible := Not ToggleBox4.Checked;
  GroupBox5.Visible := Not ToggleBox4.Checked;
  GroupBox6.Visible := Not ToggleBox4.Checked;
End;

Procedure TForm11.ToggleBox5Change(Sender: TObject);
Begin
  GroupBox1.Visible := Not ToggleBox5.Checked;
  GroupBox2.Visible := Not ToggleBox5.Checked;
  GroupBox3.Visible := Not ToggleBox5.Checked;
  GroupBox4.Visible := Not ToggleBox5.Checked;
  GroupBox6.Visible := Not ToggleBox5.Checked;
End;

Procedure TForm11.ToggleBox6Change(Sender: TObject);
Begin
  GroupBox1.Visible := Not ToggleBox6.Checked;
  GroupBox2.Visible := Not ToggleBox6.Checked;
  GroupBox3.Visible := Not ToggleBox6.Checked;
  GroupBox4.Visible := Not ToggleBox6.Checked;
  GroupBox5.Visible := Not ToggleBox6.Checked
End;

Procedure TForm11.InsertFileInfo(RootNode: PSunBurstChartElement;
  PathToFile: String; Const aInfo: TProjectFileInfo);

  Function GetSiblingCount(Const aElement: PSunBurstChartElement): integer;
  Var
    p: PSunBurstChartElement;
  Begin
    result := 0;
    p := aElement;
    While assigned(p) Do Begin
      inc(result);
      p := p^.PrevSibling;
    End;
  End;

Var
  p_i: PDirInfo;
  aDir: String;
  P, fp: PSunBurstChartElement;
  i: integer;
Begin
  If pos(PathDelim, PathToFile) <> 0 Then Begin
    aDir := copy(PathToFile, 1, pos(PathDelim, PathToFile) - 1);
    // Suchen ob es das Verzeichnis in RootNode gibt
    p := RootNode^.Child;
    fp := Nil;
    While assigned(p) And (Not assigned(fp)) Do Begin
      If (PDirInfo(p^.UserData)^.isDir) And (PDirInfo(p^.UserData)^.Info = aDir) Then Begin
        fp := p;
      End;
      p := p^.NextSibling;
    End;
    If assigned(fp) Then Begin
      // Den Pfad gibt es schon also rein und dort weiter suchen
      adir := copy(PathToFile, pos(PathDelim, PathToFile) + 1, length(PathToFile));
      fp^.Value := fp^.Value + aInfo.FileInfo.NumberOfCodeLines;
      InsertFileInfo(fp, aDir, aInfo);
    End
    Else Begin
      // Den Pfad gibt es noch nicht, anlegen und weiter Rein
      p := DirectoryChart1.AddChildElement(RootNode, DefaultSunBurstChartElement());
      new(p_i);
      p_i^.isDir := true;
      p_i^.Info := aDir;
      p^.UserData := p_i;
      i := GetSiblingCount(p) Mod length(NormalDirectoryColors);
      p^.Color.BrushColor := NormalDirectoryColors[i];
      p^.Color.PenColor := NormalDirectoryColors[i];
      p^.SelectedColor.BrushColor := SelectedDirectoryColors[i];
      p^.SelectedColor.PenColor := SelectedDirectoryColors[i];
      p^.SelectedColor.PenWitdh := 1;
      adir := copy(PathToFile, pos(PathDelim, PathToFile) + 1, length(PathToFile));
      p^.Value := p^.Value + aInfo.FileInfo.NumberOfCodeLines;
      InsertFileInfo(p, aDir, aInfo);
    End;
  End
  Else Begin
    // "Terminierung"
    p := DirectoryChart1.AddChildElement(RootNode, DefaultSunBurstChartElement());
    new(p_i);
    p_i^.isDir := false;
    p_i^.Info := aInfo.Filename;
    p^.Value := aInfo.FileInfo.NumberOfCodeLines;
    p^.UserData := p_i;
    i := GetSiblingCount(p) Mod length(NormalDirectoryColors);
    p^.Color.BrushColor := NormalDirectoryColors[i];
    p^.Color.PenColor := NormalDirectoryColors[i];
    p^.SelectedColor.BrushColor := SelectedDirectoryColors[i];
    p^.SelectedColor.PenColor := SelectedDirectoryColors[i];
    p^.SelectedColor.PenWitdh := 1;
  End;
End;

Procedure TForm11.InitLineBreakdown(aList: TProjectFilesInfo;
  Const aProject: TProject);
Var
  t, Root: PSunBurstChartElement;
  sum, i, NumberOfCodeLines, NumberofCommentLines, EmptyLines, TotalLines: Integer;
Begin
  // Line Breakdown
  LinebreakChart1.Clear;
  LinebreakChart1.InitialArc := pi;
  root := LinebreakChart1.AddChildElement(Nil, DefaultSunBurstChartElement());
  root^.Caption := '';
  root^.Color.BrushColor := clWhite;
  root^.Color.PenColor := clWhite;

  NumberOfCodeLines := 0;
  NumberofCommentLines := 0;
  TotalLines := 0;
  EmptyLines := 0;

  For i := 0 To high(aList) Do Begin
    NumberOfCodeLines := NumberOfCodeLines + alist[i].FileInfo.NumberOfCodeLines;
    NumberofCommentLines := NumberofCommentLines + alist[i].FileInfo.NumberofCommentLines;
    TotalLines := TotalLines + alist[i].FileInfo.NumberOfTotalLines;
    EmptyLines := EmptyLines + alist[i].FileInfo.NumberOfEmptyLines;
  End;

  Root^.Caption := 'Total' + LineEnding + inttostr(TotalLines);

  sum := max(1, EmptyLines + NumberofCommentLines + NumberOfCodeLines);
  t := LinebreakChart1.AddChildElement(root, DefaultSunBurstChartElement());
  t^.Color.BrushColor := Shape3.Brush.Color;
  t^.Color.PenColor := Shape3.Brush.Color;
  t^.Value := EmptyLines;
  label8.caption := format('%d, (%d%%)', [EmptyLines, round(100 * EmptyLines / (sum))]);
  t^.UserData := pchar(label8.caption);

  t := LinebreakChart1.AddChildElement(root, DefaultSunBurstChartElement());
  t^.Color.BrushColor := Shape2.Brush.Color;
  t^.Color.PenColor := Shape2.Brush.Color;
  t^.Value := NumberofCommentLines;
  label7.caption := format('%d, (%d%%)', [NumberofCommentLines, round(100 * NumberofCommentLines / (EmptyLines + NumberofCommentLines + NumberOfCodeLines))]);
  t^.UserData := pchar(label7.caption);

  t := LinebreakChart1.AddChildElement(root, DefaultSunBurstChartElement());
  t^.Color.BrushColor := Shape1.Brush.Color;
  t^.Color.PenColor := Shape1.Brush.Color;
  t^.Value := NumberOfCodeLines;
  label6.caption := format('%d, (%d%%)', [NumberOfCodeLines, round(100 * NumberOfCodeLines / (EmptyLines + NumberofCommentLines + NumberOfCodeLines))]);
  t^.UserData := pchar(label6.caption);
End;

Procedure TForm11.InitDirectoryStructure(aList: TProjectFilesInfo;
  Const aProject: TProject);
Var
  t, Root: PSunBurstChartElement;
  i: Integer;
  p: PDirInfo;
Begin
  DirectoryChart1.Clear;
  fdsCursorElement := Nil;
  Root := DirectoryChart1.AddChildElement(Nil, DefaultSunBurstChartElement());
  root^.Caption := '';
  root^.Color.BrushColor := clWhite;
  root^.Color.PenColor := clWhite;
  root^.SelectedColor.BrushColor := clWhite;
  root^.SelectedColor.PenColor := clWhite;
  root^.SelectedColor.PenWitdh := 1;
  root := DirectoryChart1.AddChildElement(Root, DefaultSunBurstChartElement());
  root^.Caption := '';
  root^.Color.BrushColor := NormalDirectoryColors[0];
  root^.Color.PenColor := NormalDirectoryColors[0];
  root^.SelectedColor.BrushColor := SelectedDirectoryColors[0];
  root^.SelectedColor.PenColor := SelectedDirectoryColors[0];
  root^.SelectedColor.PenWitdh := 1;
  new(p);
  p^.isDir := true;
  p^.Info := aProject.Name;
  root^.UserData := p;
  root^.Value := 0;
  For i := 0 To high(aList) Do Begin
    InsertFileInfo(Root, aList[i].Filename, aList[i]);
    root^.Value := root^.Value + aList[i].FileInfo.NumberOfCodeLines;
  End;
  DirectoryChart1.DeselectAll;
  // Geschwind noch mal durch alle Durch gehen und die Größen Infos anfügen ;)
  DirectoryChart1.IterFirst;
  While assigned(DirectoryChart1.Iterator) Do Begin
    t := DirectoryChart1.Iterator;
    If assigned(t^.UserData) Then Begin
      p := PDirInfo(t^.UserData);
      p^.Info := p^.Info + Format(' %d (%d%%)', [t^.Value, round(100 * t^.Value / root^.Value)]);
    End;
    DirectoryChart1.IterNext;
  End;
  DirectoryChart1.Invalidate;
  label19.Caption := format('%d files', [length(aList)]);
End;

Procedure TForm11.InitMostComplexFunctions(aList: TProjectFilesInfo;
  Const aProject: TProject);
Var
  cnt_taken, cnt, i, j: Integer;
  c: TCircle;
  s: String;
  p: PMCFunctionsInfo;
Begin
  MostComplexFunctions1.Clear;
  fmcfuCursorElement := Nil;
  cnt := 0;
  cnt_taken := 0;
  For i := 0 To high(aList) Do Begin
    cnt := cnt + length(aList[i].Methods);
    For j := 0 To high(aList[i].Methods) Do Begin
      If aList[i].Methods[j].CC > aProject.ChartStatisticSettings.BoarderForMostComplexFunction Then Begin
        inc(cnt_taken);
        c := DefaultCircle();
        c.Value := sqrt(aList[i].Methods[j].CC / pi);
        c.Caption := inttostr(aList[i].Methods[j].CC);
        c.Color.BrushColor := MostComplexFunctionColors[MostComplexFunctions1.CircleCount Mod length(MostComplexFunctionColors)];
        c.Color.PenColor := c.Color.BrushColor;
        c.Color.FontColor := clWhite;
        If aList[i].Methods[j].ClassName = '' Then Begin
          s := aList[i].Filename + '.' + aList[i].Methods[j].Name;
        End
        Else Begin
          s := aList[i].Filename + '.' + aList[i].Methods[j].ClassName + '.' + aList[i].Methods[j].Name;
        End;
        s := s + ': ' + c.Caption;
        new(p);
        p^.Info := s;
        c.UserData := P;
        MostComplexFunctions1.AddCircle(c);
      End;
    End;
  End;
  // Es gibt keine "Komplexesten" funktionen
  If MostComplexFunctions1.CircleCount = 0 Then Begin
    c := DefaultCircle();
    c.Caption := 'none';
    c.Color.BrushColor := clWhite;
    c.Color.PenColor := clWhite;
    c.Color.FontColor := clBlack;
    c.SelectedColor := c.Color;
    MostComplexFunctions1.AddCircle(c);
  End;
  MostComplexFunctions1.Invalidate;
  label20.caption := format('%d out of %d', [cnt_taken, cnt]);
End;

Procedure TForm11.InitMostComplexFiles(aList: TProjectFilesInfo;
  Const aProject: TProject);
Var
  cnt_taken, i, j: Integer;
  c: TCircle;
  p: PMCFilessInfo;
  sum, cnt: integer;
Begin
  MostComplexFiles1.Clear;
  fmcfiCursorElement := Nil;
  cnt_taken := 0;
  For i := 0 To high(aList) Do Begin
    sum := 0;
    cnt := max(1, length(aList[i].Methods));
    For j := 0 To high(aList[i].Methods) Do Begin
      sum := sum + aList[i].Methods[j].CC;
    End;
    If sum / cnt > aProject.ChartStatisticSettings.BoarderForAverageMostComplexFiles Then Begin
      cnt_taken := cnt_taken + 1;
      c := DefaultCircle();
      c.Value := sqrt(sum / (cnt * pi));
      c.Caption := format('%0.1f', [sum / cnt]); //inttostr(sum Div cnt);
      c.Color.BrushColor := MostComplexFunctionColors[MostComplexFiles1.CircleCount Mod length(MostComplexFunctionColors)];
      c.Color.PenColor := c.Color.BrushColor;
      c.Color.FontColor := clWhite;
      new(p);
      p^.Info := aList[i].Filename + ': ' + c.Caption;
      c.UserData := P;
      MostComplexFiles1.AddCircle(c);
    End;
  End;
  If MostComplexFiles1.CircleCount = 0 Then Begin
    c := DefaultCircle();
    c.Caption := 'none';
    c.Color.BrushColor := clWhite;
    c.Color.PenColor := clWhite;
    c.Color.FontColor := clBlack;
    c.SelectedColor := c.Color;
    MostComplexFiles1.AddCircle(c);
  End;
  MostComplexFiles1.Invalidate;
  label22.caption := format('%d out of %d', [cnt_taken, length(aList)]);
End;

Procedure TForm11.InitLargestFunctions(aList: TProjectFilesInfo;
  Const aProject: TProject);
Var
  cnt, cnt_taken, len, c, i, j: Integer;
  s: String;
Begin
  Chart1BarSeries1.Clear;
  c := 0;
  cnt := 0;
  cnt_taken := 0;
  For i := 0 To high(aList) Do Begin
    cnt := cnt + length(aList[i].Methods);
    For j := 0 To high(aList[i].Methods) Do Begin
      len := aList[i].Methods[j].EndLineInFile - aList[i].Methods[j].BeginLineInFile;
      If len > aProject.ChartStatisticSettings.BoarderForLargestFunction Then Begin
        cnt_taken := cnt_taken + 1;
        If aList[i].Methods[j].ClassName = '' Then Begin
          s := aList[i].Filename + '.' + aList[i].Methods[j].Name;
        End
        Else Begin
          s := aList[i].Filename + '.' + aList[i].Methods[j].ClassName + '.' + aList[i].Methods[j].Name;
        End;
        s := s + ' [ ' + inttostr(len) + ']';
        Chart1BarSeries1.AddXY(c, len, s, MostComplexFunctionColors[c Mod length(MostComplexFunctionColors)]);
        inc(c);
      End;
    End;
  End;
  Chart1.Invalidate;
  label21.caption := format('%d out of %d', [cnt_taken, cnt]);
End;

Procedure TForm11.InitLargestFiles(aList: TProjectFilesInfo;
  Const aProject: TProject);
Var
  cnt_taken, len, c, i: Integer;
  s: String;
Begin
  Chart2BarSeries1.Clear;
  c := 0;
  cnt_taken := 0;
  For i := 0 To high(aList) Do Begin
    len := aList[i].FileInfo.NumberOfCodeLines;
    If len > aProject.ChartStatisticSettings.BoarderForLargestFiles Then Begin
      cnt_taken := cnt_taken + 1;
      s := aList[i].Filename + ' [ ' + inttostr(len) + ']';
      Chart2BarSeries1.AddXY(c, len, s, MostComplexFunctionColors[c Mod length(MostComplexFunctionColors)]);
      inc(c);
    End;
  End;
  Chart2.Invalidate;
  label23.caption := format('%d out of %d', [cnt_taken, length(aList)]);
End;

End.

