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
Unit Unit4;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ufpc_understand;

Type

  { TForm4 }

  TForm4 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Memo1: TMemo;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
  private
    fColors: TCCColors;
    fProject: TProject;
  public
    Function CountFiles(aList: TProjectFilesInfo; Project: TProject): Boolean;
  End;

Var
  Form4: TForm4;

Implementation

{$R *.lfm}

Uses unit6, ufpcparser, Math;

{ TForm4 }

Procedure TForm4.Button1Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm4.Button2Click(Sender: TObject);
Begin
  // Details
  form6.SetColors(fColors);
  form6.StringGrid1.AutoSizeColumns;
  form6.ShowModal;
End;

Procedure TForm4.FormCreate(Sender: TObject);
Begin
  caption := 'Code Statistics';
  button1.Align := alBottom;
  button2.Align := alBottom;
  Memo1.Align := alClient;
End;

Function TForm4.CountFiles(aList: TProjectFilesInfo; Project: TProject
  ): Boolean;
Var
  j, minCC, i, MethodeCount, CCSum, maxCC: Integer;
  minCCName, maxCCName, minCCFileName, maxCCFileName: String;
  ClassCount: Integer;
  FileCount: integer;
  NumberOfCodeLines, NumberofCommentLines, TotalLines, EmptyLines: integer;
  lmaxCC, lCCSum: Integer;
Begin
  fProject := Project;
  form6.fProject := Project;
  result := false;
  If high(aList) = -1 Then exit;
  fColors := Project.CCColors;
  MethodeCount := 0;
  CCSum := 0;
  minCC := high(Integer);
  maxCC := 0;
  minCCName := '';
  maxCCName := '';
  minCCFileName := '';
  maxCCFileName := '';
  ClassCount := 0;
  FileCount := 0;
  NumberOfCodeLines := 0;
  NumberofCommentLines := 0;
  TotalLines := 0;
  EmptyLines := 0;
  form6.ClearGrid;
  For i := 0 To high(aList) Do Begin
    ClassCount := ClassCount + length(alist[i].FileInfo.aClasses);
    NumberOfCodeLines := NumberOfCodeLines + alist[i].FileInfo.NumberOfCodeLines;
    NumberofCommentLines := NumberofCommentLines + alist[i].FileInfo.NumberofCommentLines;
    TotalLines := TotalLines + alist[i].FileInfo.NumberOfTotalLines;
    EmptyLines := EmptyLines + alist[i].FileInfo.NumberOfEmptyLines;
    inc(FileCount);
    MethodeCount := MethodeCount + length(alist[i].Methods);
    lmaxCC := 0;
    lCCSum := 0;
    For j := 0 To high(alist[i].Methods) Do Begin
      CCSum := CCSum + alist[i].Methods[j].CC;
      lCCSum := lCCSum + alist[i].Methods[j].CC;
      lmaxCC := max(lmaxCC, alist[i].Methods[j].CC);
      If alist[i].Methods[j].CC < minCC Then Begin
        minCC := alist[i].Methods[j].CC;
        minCCFileName := alist[i].Methods[j].Filename;
        If alist[i].Methods[j].ClassName <> '' Then Begin
          minCCName := alist[i].Methods[j].ClassName + '.' + alist[i].Methods[j].Name;
        End
        Else Begin
          minCCName := alist[i].Methods[j].Name;
        End;
      End;
      If alist[i].Methods[j].CC > maxCC Then Begin
        maxCC := alist[i].Methods[j].CC;
        maxCCFileName := alist[i].Methods[j].Filename;
        If alist[i].Methods[j].ClassName <> '' Then Begin
          maxCCName := alist[i].Methods[j].ClassName + '.' + alist[i].Methods[j].Name;
        End
        Else Begin
          maxCCName := alist[i].Methods[j].Name;
        End;
      End;
    End;
    // Fürs Detail
    Form6.AddDataset(alist[i].FileName, alist[i].FileInfo, length(alist[i].Methods), lCCSum / max(1, length(alist[i].Methods)), lmaxCC);
  End;
  memo1.clear;
  memo1.lines.add(format('Comment lines    : %8d', [NumberofCommentLines]));
  memo1.lines.add(format('Sourceode lines  : %8d', [NumberOfCodeLines]));
  memo1.lines.add(format('Empty lines      : %8d', [EmptyLines]));
  memo1.lines.add(format('Total lines      : %8d', [TotalLines]));
  memo1.lines.add('');
  memo1.lines.add(format('Number of classes: %8d', [ClassCount]));
  memo1.lines.add(format('Number of methods: %8d', [MethodeCount]));

  // CC Informationen
  If minCC <> high(Integer) Then Begin
    memo1.lines.add('');
    memo1.lines.add(format('Average CC       : %10.1f', [CCSum / max(1, MethodeCount)]));
    memo1.lines.add(format('Lowest CC        : %8d = %s (%s)', [minCC, minCCName, minCCFileName]));
    memo1.lines.add(format('Highest CC       : %8d = %s (%s)', [maxCC, maxCCName, maxCCFileName]));
  End
  Else Begin
    memo1.lines.add('');
    memo1.lines.add('No methods found.');
  End;
  memo1.lines.add('');
  memo1.lines.add(format('Total files      : %8d', [FileCount]));
  memo1.lines.add(format('Total CC         : %8d', [CCSum]));
  result := true;
End;

End.

