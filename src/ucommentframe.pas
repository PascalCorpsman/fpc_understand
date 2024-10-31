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
Unit uCommentFrame;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, StdCtrls;

Type

  { TCommentFrame }

  TCommentFrame = Class(TFrame)
    Button1: TButton;
    Memo1: TMemo;
  private

  public
    Line: integer;
    Order: integer; // Die Sortierte Reihenfolge
    Constructor Create(TheOwner: TComponent); override;
  End;

Implementation

{$R *.lfm}

{ TCommentFrame }

Constructor TCommentFrame.Create(TheOwner: TComponent);
Begin
  Inherited Create(TheOwner);
  memo1.clear;
End;

End.

