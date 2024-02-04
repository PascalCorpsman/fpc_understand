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
Unit Unit10;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  CheckLst;

Type

  { TForm10 }

  TForm10 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    CheckListBox1: TCheckListBox;
    Label1: TLabel;
    Procedure Button2Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
  private

  public

  End;

Var
  Form10: TForm10;

Implementation

{$R *.lfm}

{ TForm10 }

Procedure TForm10.FormCreate(Sender: TObject);
Begin
  Constraints.MinHeight := Height;
  Constraints.MinWidth := Width;
  caption := '';
End;

Procedure TForm10.Button2Click(Sender: TObject);
Begin
  CheckListBox1.CheckAll(cbChecked);
End;

Procedure TForm10.Button3Click(Sender: TObject);
Begin
  CheckListBox1.CheckAll(cbUnchecked);
End;

End.

