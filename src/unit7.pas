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
Unit Unit7;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, CheckLst,
  uDOMXML;

Type

  { TForm7 }

  TForm7 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Label1: TLabel;
    CheckListBox1: TCheckListBox;
    OpenDialog1: TOpenDialog;
    Procedure Button1Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure Button4Click(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure FormDestroy(Sender: TObject);
  private
    lpiFile: TDOMXML;
    fFilename: String;
    ProjectPathDelim: String; // Der Pathdelim muss ggf für die .lpi Datei angepasst werden.
  public
    Procedure LoadLPIFile(aFilename: String);
  End;

Var
  Form7: TForm7;

Implementation

{$R *.lfm}

Uses LCLType, LazFileUtils, ufpc_understand;

{ TForm7 }

Procedure TForm7.FormCreate(Sender: TObject);
Begin
  caption := '.lpi editor';
  label1.caption := 'Filelist';
  Constraints.MinHeight := Height;
  Constraints.MinWidth := Width;
  lpiFile := TDOMXML.Create;
End;

Procedure TForm7.FormDestroy(Sender: TObject);
Begin
  lpiFile.free;
End;

Procedure TForm7.Button1Click(Sender: TObject);
Var
  units, unitfile, ip: TDomNode;
  index: integer;
Begin
  // OK
  // Übernehmen der IsPartOf Attribute
  units := lpiFile.DocumentElement.FindPath('CONFIG.ProjectOptions.units');
  unitfile := units.FirstChild;
  index := 0;
  While assigned(unitfile) Do Begin
    If CheckListBox1.Items.Count <= index Then Begin
      Raise exception.Create('Error invalid index in Array.');
    End;
    If CheckListBox1.Checked[index] Then Begin
      // Is Part = True und rein !
      ip := unitfile.FindNode('IsPartOfProject');
      If Not assigned(ip) Then Begin
        ip := unitfile.AddChild('IsPartOfProject', '');
      End;
      ip.AttributeValue['Value'] := 'True';
    End
    Else Begin
      // Is part = False oder raus werfen
      ip := unitfile.FindNode('IsPartOfProject');
      If assigned(ip) Then ip.free;
    End;
    unitfile := units.NextSibling;
    inc(index);
  End;
  lpiFile.Indent := '  ';
  lpiFile.SaveToFile(fFilename);
  ModalResult := mrOK;
End;

Procedure TForm7.Button3Click(Sender: TObject);
Var
  fn: String;
  i: Integer;
Begin
  // Remove Selected from file Project
  If CheckListBox1.ItemIndex = -1 Then Begin
    showmessage('Error, nothing selected.');
    exit;
  End;
  // 1. Einen "echten" Dateinamen daraus machen.
  fn := ConcatRelativePath(IncludeTrailingPathDelimiter(ExtractFilePath(fFilename)), CheckListBox1.items[CheckListBox1.ItemIndex]);
  For i := 1 To length(fn) Do Begin
    If fn[i] In AllowDirectorySeparators Then Begin
      fn[i] := PathDelim;
    End;
  End;
  Case RemoveFileFromLPIFile(IncludeTrailingPathDelimiter(ExtractFilePath(fFilename)), lpiFile, fn) Of
    rrRemoved: Begin
        CheckListBox1.Items.Delete(CheckListBox1.ItemIndex);
        CheckListBox1.ItemIndex := -1;
      End;
    rrdeactivated: Begin
        CheckListBox1.Checked[CheckListBox1.ItemIndex] := false;
      End;
    rrError: Begin
        showmessage('Error, could not remove file from .lpi');
      End;
  End;
End;

Procedure TForm7.Button4Click(Sender: TObject);
Var
  fn: String;
  j, index: Integer;
Begin
  // Add File to Project
  If OpenDialog1.Execute Then Begin
    For j := 0 To OpenDialog1.Files.Count - 1 Do Begin
      index := AddFileToLPIFile(ExtractFileDir(fFilename), lpiFile, OpenDialog1.Files[j]);
      fn := ExtractRelativePath(IncludeTrailingPathDelimiter(ExtractFileDir(fFilename)), OpenDialog1.Files[j]);
      If index < CheckListBox1.Items.Count Then Begin
        showmessage('Info: "' + fn + '" already in .lpi file, will enable it.');
        CheckListBox1.Checked[index] := true;
        Continue;
      End
      Else Begin
        CheckListBox1.Items.Add(fn);
        CheckListBox1.Checked[CheckListBox1.Items.Count - 1] := true;
      End;
    End;
  End;
End;

Procedure TForm7.LoadLPIFile(aFilename: String);
Var
  units, unitfile, fn, ip: TDomNode;
  f: String;
  isPart: Boolean;
Begin
  ProjectPathDelim := '/';
  fFilename := aFilename;
  lpiFile.LoadFromFile(aFilename);
  ProjectPathDelim := GetPathDelimFromLPIFile(lpiFile);
  CheckListBox1.Clear;
  units := lpiFile.DocumentElement.FindPath('CONFIG.ProjectOptions.units');
  unitfile := Nil;
  If assigned(units) Then unitfile := units.FirstChild;
  While assigned(unitfile) Do Begin
    fn := unitfile.FindNode('Filename');
    If assigned(fn) Then Begin
      f := fn.AttributeValue['Value'];
      ip := unitfile.FindNode('IsPartOfProject');
      isPart := true;
      If assigned(ip) Then Begin
        If lowercase(ip.AttributeValue['Value']) <> 'true' Then isPart := false;
      End
      Else Begin
        isPart := false;
      End;
      CheckListBox1.items.add(f);
      CheckListBox1.Checked[CheckListBox1.Items.Count - 1] := isPart;
    End;
    unitfile := units.NextSibling;
  End;
End;

End.

