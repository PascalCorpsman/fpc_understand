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
Unit Unit5;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  Grids, ExtCtrls, Menus, ufpc_understand, ufpcparser;

Type

  { TForm5 }

  TForm5 = Class(TForm)
    Button1: TButton;
    ListBox1: TListBox;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    PopupMenu1: TPopupMenu;
    SaveDialog1: TSaveDialog;
    Splitter1: TSplitter;
    StringGrid1: TStringGrid;
    Procedure Button1Click(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure ListBox1Click(Sender: TObject);
    Procedure MenuItem1Click(Sender: TObject);
    Procedure MenuItem2Click(Sender: TObject);
    Procedure StringGrid1CompareCells(Sender: TObject; ACol, ARow, BCol,
      BRow: Integer; Var Result: integer);
    Procedure StringGrid1HeaderClick(Sender: TObject; IsColumn: Boolean;
      Index: Integer);
    Procedure StringGrid1PrepareCanvas(Sender: TObject; aCol, aRow: Integer;
      aState: TGridDrawState);
  private
    fCCColors: TCCColors;
    fFileInfos: Array Of TProcInfos;
    fStringGridSortDirection: Boolean;
  public
    Function LoadFunctions(aList: TProjectFilesInfo; Colors: TCCColors): Boolean;
  End;

Var
  Form5: TForm5;

Implementation

{$R *.lfm}

Const
  IndexFilename = 0;
  IndexKind = 1;
  IndexClass = 2;
  IndexName = 3;
  IndexCC = 4;
  IndexCaseCC = 5;
  IndexLine = 6;
  IndexLength = 7;

  { TForm5 }

Procedure TForm5.Button1Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm5.FormCreate(Sender: TObject);
Begin
  caption := 'Cyclomatic complexity viewer';
End;

Procedure TForm5.ListBox1Click(Sender: TObject);
Var
  i: Integer;
Begin
  If ListBox1.ItemIndex <> -1 Then Begin
    StringGrid1.Columns[IndexKind].Title.Caption := 'Kind';
    StringGrid1.Columns[IndexClass].Title.Caption := 'Class';
    StringGrid1.Columns[IndexName].Title.Caption := 'Name';
    StringGrid1.Columns[IndexCC].Title.Caption := 'Complexity';
    StringGrid1.Columns[IndexCaseCC].Title.Caption := 'Case Complexity';
    StringGrid1.Columns[IndexLine].Title.Caption := 'Line';
    StringGrid1.Columns[IndexLength].Title.Caption := 'Length';
    StringGrid1.RowCount := length(fFileInfos[ListBox1.ItemIndex]) + 1;
    fStringGridSortDirection := false;
    For i := 0 To high(fFileInfos[ListBox1.ItemIndex]) Do Begin
      StringGrid1.Cells[IndexFilename, i + 1] := ListBox1.Items[ListBox1.ItemIndex];
      StringGrid1.Cells[IndexKind, i + 1] := fFileInfos[ListBox1.ItemIndex][i].Method;
      StringGrid1.Cells[IndexClass, i + 1] := fFileInfos[ListBox1.ItemIndex][i].ClassName;
      StringGrid1.Cells[IndexName, i + 1] := fFileInfos[ListBox1.ItemIndex][i].Name;
      StringGrid1.Cells[IndexCC, i + 1] := inttostr(fFileInfos[ListBox1.ItemIndex][i].CC);
      StringGrid1.Cells[IndexCaseCC, i + 1] := inttostr(fFileInfos[ListBox1.ItemIndex][i].CaseCC);
      StringGrid1.Cells[IndexLine, i + 1] := inttostr(fFileInfos[ListBox1.ItemIndex][i].BeginLineInFile);
      StringGrid1.Cells[IndexLength, i + 1] := inttostr(fFileInfos[ListBox1.ItemIndex][i].EndLineInFile - fFileInfos[ListBox1.ItemIndex][i].BeginLineInFile + 1);
    End;
    StringGrid1.SortColRow(true, IndexCC);
    StringGrid1.AutoSizeColumns;
  End;
End;

Procedure TForm5.MenuItem1Click(Sender: TObject);
Begin
  // Export as CSV
  If SaveDialog1.Execute Then Begin
    StringGrid1.SaveToCSVFile(SaveDialog1.FileName, ';', true, true);
  End;
End;

Procedure TForm5.MenuItem2Click(Sender: TObject);
Var
  m: TMemoryStream;
  j, i: Integer;
Begin
  // Export All as CSV
  If SaveDialog1.Execute Then Begin
    m := TMemoryStream.Create;
    j := ListBox1.ItemIndex;
    For i := 0 To ListBox1.Items.Count - 1 Do Begin
      ListBox1.ItemIndex := i;
      ListBox1Click(Nil);
      StringGrid1.SaveToCSVStream(m, ';', i = 0, false);
    End;
    m.SaveToFile(SaveDialog1.FileName);
    m.free;
    ListBox1.ItemIndex := j;
    ListBox1Click(Nil);
  End;
End;

Procedure TForm5.StringGrid1CompareCells(Sender: TObject; ACol, ARow, BCol,
  BRow: Integer; Var Result: integer);
Begin
  If acol <> bCol Then Begin
    Raise Exception.Create('TForm2.StringGrid1CompareCells, falsche Sortierung!');
  End;
  If acol In [IndexCC, IndexLine, IndexCaseCC, IndexLength] Then Begin
    result := strtointdef(StringGrid1.Cells[ACol, ARow], 0) - strtointdef(StringGrid1.Cells[BCol, BRow], 0);
  End
  Else Begin
    result := CompareStr(StringGrid1.Cells[ACol, ARow], StringGrid1.Cells[BCol, BRow]);
  End;
  If Not fStringGridSortDirection Then Begin
    result := -Result;
  End;
End;

Procedure TForm5.StringGrid1HeaderClick(Sender: TObject; IsColumn: Boolean;
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

Procedure TForm5.StringGrid1PrepareCanvas(Sender: TObject; aCol, aRow: Integer;
  aState: TGridDrawState);
Var
  val: integer;
Begin
  If gdSelected In aState Then exit;
  If (acol In [IndexCC, IndexCaseCC]) And (aRow > 0) Then Begin
    val := StrToIntDef(StringGrid1.Cells[aCol, aRow], 0);
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

Function TForm5.LoadFunctions(aList: TProjectFilesInfo; Colors: TCCColors): Boolean;
Var
  i: Integer;
Begin
  fCCColors := Colors;
  result := false;
  ListBox1.Clear;
  setlength(fFileInfos, length(aList));
  For i := 0 To high(aList) Do Begin
    listbox1.items.add(alist[i].FileName);
    fFileInfos[i] := alist[i].Methods;
  End;
  If ListBox1.Items.Count > 0 Then result := true;
  If ListBox1.items.Count <> 0 Then Begin
    ListBox1.ItemIndex := 0;
    ListBox1.Click;
  End;
End;

End.

