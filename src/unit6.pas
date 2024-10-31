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
Unit Unit6;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Grids, Menus,
  ufpcparser, ufpc_understand;

Type

  { TForm6 }

  TForm6 = Class(TForm)
    Button1: TButton;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    PopupMenu1: TPopupMenu;
    SaveDialog1: TSaveDialog;
    StringGrid1: TStringGrid;
    Procedure Button1Click(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure MenuItem1Click(Sender: TObject);
    Procedure StringGrid1CompareCells(Sender: TObject; ACol, ARow, BCol,
      BRow: Integer; Var Result: integer);
    Procedure StringGrid1DblClick(Sender: TObject);
    Procedure StringGrid1HeaderClick(Sender: TObject; IsColumn: Boolean;
      Index: Integer);
    Procedure StringGrid1PrepareCanvas(Sender: TObject; aCol, aRow: Integer;
      aState: TGridDrawState);
  private
    fStringGridSortDirection: Boolean;
    fCCColors: TCCColors;
  public
    fProject: TProject;
    Procedure ClearGrid;
    Procedure AddDataset(Filename: String; Const AData: TFileInfo;
      MethodCount: integer; AvgCC: Single; MaxCC: Integer);
    Procedure SetColors(aColors: TCCColors);
  End;

Var
  Form6: TForm6;

Implementation

Uses math
  , Unit13 // Code Window
  ;

{$R *.lfm}

Const
  IndexFilename = 0;
  IndexCodeLines = 1;
  IndesTotalLines = 2;
  IndexClassesCount = 3;
  IndexMethodsCount = 4;
  IndexAveraceCC = 5;
  IndexMaxCC = 6;

  { TForm6 }

Procedure TForm6.Button1Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm6.FormCreate(Sender: TObject);
Begin
  caption := 'Statistik details';
  button1.Align := alBottom;
  StringGrid1.Align := alClient;
End;

Procedure TForm6.MenuItem1Click(Sender: TObject);
Begin
  // Export as CSV
  If SaveDialog1.Execute Then Begin
    StringGrid1.SaveToCSVFile(SaveDialog1.FileName, ';', true);
  End;
End;

Procedure TForm6.StringGrid1CompareCells(Sender: TObject; ACol, ARow, BCol,
  BRow: Integer; Var Result: integer);
Begin
  If acol <> bCol Then Begin
    Raise Exception.Create('TForm2.StringGrid1CompareCells, falsche Sortierung!');
  End;
  If acol = IndexFilename Then Begin
    result := CompareStr(StringGrid1.Cells[ACol, ARow], StringGrid1.Cells[BCol, BRow]);
  End
  Else Begin
    If acol = IndexAveraceCC Then Begin
      result := trunc(StrToFloatDef(StringGrid1.Cells[ACol, ARow], 0) - StrToFloatDef(StringGrid1.Cells[BCol, BRow], 0));
    End
    Else Begin
      result := strtointdef(StringGrid1.Cells[ACol, ARow], 0) - strtointdef(StringGrid1.Cells[BCol, BRow], 0);
    End;
  End;
  If Not fStringGridSortDirection Then Begin
    result := -Result;
  End;
End;

Procedure TForm6.StringGrid1DblClick(Sender: TObject);
Begin
  // Open Code
  If StringGrid1.Selection.Top <> -1 Then Begin
    form13.OpenFile(fProject, StringGrid1.Cells[0, StringGrid1.Selection.Top]);
  End;
End;

Procedure TForm6.StringGrid1HeaderClick(Sender: TObject; IsColumn: Boolean;
  Index: Integer);
Var
  i: Integer;
Begin
  If pos('\/', StringGrid1.Columns[Index].Title.Caption) <> 0 Then Begin
    // Die Sortierrichtung der Spalte Drehen
    StringGrid1.Columns[Index].Title.Caption := copy(StringGrid1.Columns[Index].Title.Caption, 1, pos('\/', StringGrid1.Columns[Index].Title.Caption) - 1);
    StringGrid1.Columns[Index].Title.Caption := StringGrid1.Columns[Index].Title.Caption + '/\';
    fStringGridSortDirection := false;
  End
  Else Begin
    // 1. Alle "Pfeile" entfernen
    For i := 0 To StringGrid1.Columns.Count - 1 Do Begin
      If pos('\/', StringGrid1.Columns[i].Title.Caption) <> 0 Then Begin
        StringGrid1.Columns[i].Title.Caption := copy(StringGrid1.Columns[i].Title.Caption, 1, pos('\/', StringGrid1.Columns[i].Title.Caption) - 1);
      End;
      If pos('/\', StringGrid1.Columns[i].Title.Caption) <> 0 Then Begin
        StringGrid1.Columns[i].Title.Caption := copy(StringGrid1.Columns[i].Title.Caption, 1, pos('/\', StringGrid1.Columns[i].Title.Caption) - 1);
      End;
    End;
    // 2. Anzeigen des "Pfeils
    StringGrid1.Columns[Index].Title.Caption := StringGrid1.Columns[Index].Title.Caption + '\/';
    fStringGridSortDirection := true;
  End;
  StringGrid1.SortColRow(true, index);
  StringGrid1.AutoSizeColumns;
End;

Procedure TForm6.StringGrid1PrepareCanvas(Sender: TObject; aCol, aRow: Integer;
  aState: TGridDrawState);
Var
  val: integer;
Begin
  If gdSelected In aState Then exit;
  If (acol In [IndexAveraceCC, IndexMaxCC]) And (aRow > 0) Then Begin
    If acol In [IndexAveraceCC] Then Begin
      val := ceil(StrToFloatDef(StringGrid1.Cells[aCol, aRow], 0));
    End
    Else Begin
      val := StrToIntDef(StringGrid1.Cells[aCol, aRow], 0);
    End;
    If val <= fCCColors.LevelGood Then Begin
      StringGrid1.Canvas.Brush.Color := fCCColors.ColorGood;
    End
    Else Begin
      If val <= fCCColors.LevelModerate Then Begin
        StringGrid1.Canvas.Brush.Color := fCCColors.ColorModerate;
      End
      Else Begin
        If val <= fCCColors.LevelComplex Then Begin
          StringGrid1.Canvas.Brush.Color := fCCColors.ColorComplex;
        End
        Else Begin
          StringGrid1.Canvas.Brush.Color := fCCColors.ColorUnstable;
        End;
      End;
    End;
  End;
End;

Procedure TForm6.ClearGrid;
Begin
  StringGrid1.RowCount := 1;
  fStringGridSortDirection := false;
  StringGrid1.Columns[IndexFilename].Title.Caption := 'Filename';
  StringGrid1.Columns[IndexCodeLines].Title.Caption := 'Code lines';
  StringGrid1.Columns[IndesTotalLines].Title.Caption := 'Total lines';
  StringGrid1.Columns[IndexClassesCount].Title.Caption := 'Classes count';
  StringGrid1.Columns[IndexMethodsCount].Title.Caption := 'Method count';
  StringGrid1.Columns[IndexAveraceCC].Title.Caption := 'Average CC';
  StringGrid1.Columns[IndexMaxCC].Title.Caption := 'Max CC';
End;

Procedure TForm6.AddDataset(Filename: String; Const AData: TFileInfo;
  MethodCount: integer; AvgCC: Single; MaxCC: Integer);
Var
  i: Integer;
Begin
  StringGrid1.RowCount := StringGrid1.RowCount + 1;
  i := StringGrid1.RowCount - 1;
  StringGrid1.Cells[IndexFilename, i] := Filename;
  StringGrid1.Cells[IndexCodeLines, i] := inttostr(AData.NumberOfCodeLines);
  StringGrid1.Cells[IndesTotalLines, i] := inttostr(AData.NumberOfTotalLines);
  StringGrid1.Cells[IndexClassesCount, i] := inttostr(length(AData.aClasses));
  StringGrid1.Cells[IndexMethodsCount, i] := inttostr(MethodCount);
  StringGrid1.Cells[IndexAveraceCC, i] := format('%0.1f', [AvgCC]);
  StringGrid1.Cells[IndexMaxCC, i] := inttostr(MaxCC);
End;

Procedure TForm6.SetColors(aColors: TCCColors);
Begin
  fCCColors := aColors;
End;

End.

