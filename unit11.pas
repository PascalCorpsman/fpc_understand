Unit Unit11;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  usunburstchart, ufpc_understand;

Const
  // !! Beide Arrays benötigen die selbe Länge !!
  //                                        Grün,     Blau,      Orange,    Rot
  SelectedColors: Array[0..3] Of TColor = ($0070D8AF, $00B98C4E, $0056ADFC, $002157E4);
  NormalColors: Array[0..3] Of TColor = ($00C4E2CD, $00EADDCA, $00CDE7FE, $00BDCDF7);

Type

  TDirInfo = Record
    isDir: Boolean;
    Info: String;
    aColorIndex: Integer;
  End;

  PDirInfo = ^TDirInfo;

  { TForm11 }

  TForm11 = Class(TForm)
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    LinebreakChart1: TSunburstChart;
    DirectoryChart1: TSunburstChart;
    Shape1: TShape;
    Shape2: TShape;
    Shape3: TShape;
    Procedure FormCreate(Sender: TObject);
    Procedure OnLineBreakChart1Resize(Sender: TObject);
    Procedure OnLineBreakChart1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    Procedure OnLineBreakChart1ShowHint(Sender: TObject; HintInfo: PHintInfo);
    Procedure OnLineBreakChart1DeleteElementsUserData(Sender: TObject; aUserData: Pointer);
    Procedure OnDirectoryChart1Resize(Sender: TObject);
    Procedure OnDirectoryChart1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    Procedure OnDirectoryChart1DeleteElementsUserData(Sender: TObject; aUserData: Pointer);
    Procedure OnDirectoryChart1AfterPaint(Sender: TObject);
  private
    fLBMousePos: TPoint;
    fDSMousePos: TPoint;
    fdsCursorElement: PSunBurstChartElement;
    Procedure InsertFileInfo(RootNode: PSunBurstChartElement; PathToFile: String; Const aInfo: TProjectFileInfo);
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
End;

Procedure TForm11.InitCharts(aList: TProjectFilesInfo; Const aProject: TProject
  );
Var
  t, Root: PSunBurstChartElement;
  sum, i, NumberOfCodeLines, NumberofCommentLines, EmptyLines, TotalLines: Integer;
  p: PDirInfo;
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

  // Directory Structure
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
  root^.Color.BrushColor := NormalColors[0];
  root^.Color.PenColor := NormalColors[0];
  root^.SelectedColor.BrushColor := SelectedColors[0];
  root^.SelectedColor.PenColor := SelectedColors[0];
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
  DirectoryChart1.PieCenter := point(v, v);
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
      i := GetSiblingCount(p) Mod length(NormalColors);
      p^.Color.BrushColor := NormalColors[i];
      p^.Color.PenColor := NormalColors[i];
      p^.SelectedColor.BrushColor := SelectedColors[i];
      p^.SelectedColor.PenColor := SelectedColors[i];
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
    i := GetSiblingCount(p) Mod length(NormalColors);
    p^.Color.BrushColor := NormalColors[i];
    p^.Color.PenColor := NormalColors[i];
    p^.SelectedColor.BrushColor := SelectedColors[i];
    p^.SelectedColor.PenColor := SelectedColors[i];
    p^.SelectedColor.PenWitdh := 1;
  End;
End;

End.

