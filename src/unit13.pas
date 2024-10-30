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
Unit Unit13;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, SynEdit,
  SynHighlighterPas, SynPopupMenu;

Type

  { TForm13 }

  TForm13 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    SynEdit1: TSynEdit;
    SynPasSyn1: TSynPasSyn;
    Procedure Button2Click(Sender: TObject);
  private
    frelativeFilename: String;
    fAbsoluteFilename: String;
  public
    Procedure OpenFile(Const Rootfolder, Filename: String; aLine: integer = -1);

  End;

Var
  Form13: TForm13;

Implementation

{$R *.lfm}

Uses ufpc_understand;

{ TForm13 }

Procedure TForm13.Button2Click(Sender: TObject);
Begin
  showmessage('Todo.');
End;

Procedure TForm13.OpenFile(Const Rootfolder, Filename: String; aLine: integer);
Begin
  frelativeFilename := Filename;
  fAbsoluteFilename := ConcatRelativePath(Rootfolder, Filename);

  If FileExists(fAbsoluteFilename) Then Begin
    SynEdit1.Lines.LoadFromFile(fAbsoluteFilename);
    SynEdit1.TopLine := 0;
    If aLine <> -1 Then Begin
      SynEdit1.TopLine := SynEdit1.Lines.Count - 1;
      SynEdit1.Invalidate;
      SynEdit1.TopLine := aLine;
    End;
    caption := 'File preview: ' + ExtractFileName(frelativeFilename);
    ShowModal;
  End
  Else Begin
    showmessage('Error, unable to find: ' + fAbsoluteFilename);
  End;
End;

End.

