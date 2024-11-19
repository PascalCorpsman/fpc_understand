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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Menus,
  ExtCtrls, SynEdit, SynHighlighterPas, SynEditMarks, uCommentFrame,
  ufpc_understand;

Type

  { TForm13 }

  TForm13 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    ImageList1: TImageList;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    Panel1: TPanel;
    PopupMenu1: TPopupMenu;
    PopupMenu2: TPopupMenu;
    SaveDialog3: TSaveDialog;
    ScrollBox1: TScrollBox;
    Splitter1: TSplitter;
    SynEdit1: TSynEdit;
    SynPasSyn1: TSynPasSyn;
    Procedure Button2Click(Sender: TObject);
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
    Procedure FormCreate(Sender: TObject);
    Procedure FormShow(Sender: TObject);
    Procedure MenuItem1Click(Sender: TObject);
    Procedure MenuItem2Click(Sender: TObject);
    Procedure ScrollBox1Resize(Sender: TObject);
    Procedure SynEdit1Change(Sender: TObject);
    Procedure SynEdit1GutterClick(Sender: TObject; X, Y, Line: integer;
      mark: TSynEditMark);
  private
    fProject: TProject;
    frelativeFilename: String;
    fAbsoluteFilename: String;
    fFrameNameCounter: integer;
    fformShowOnce: Boolean;
    Procedure RepositionAllComments;
    Function AddLineMark(Line: Integer): TCommentFrame;
    Procedure RemoveLineMark(Line: integer);
    Procedure OnRemoveCommentClick(Sender: TObject);
    Procedure OnSelectLineComment(Sender: TObject);
  public
    Procedure OpenFile(Project: TProject; Const Filename: String; aLine: integer = -1);
  End;

Var
  Form13: TForm13;

Implementation

{$R *.lfm}

{ TForm13 }

Procedure TForm13.Button2Click(Sender: TObject);
Begin
  showmessage('Todo.');
End;

Procedure TForm13.FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
Var
  i: Integer;
  f: TCommentFrame;
Begin
  // Speichern aller Unit Kommentare
  For i := 0 To ScrollBox1.ComponentCount - 1 Do Begin
    f := ScrollBox1.Components[i] As TCommentFrame;
    fProject.SetLineComment(frelativeFilename, f.Line, f.Memo1.Text);
  End;
End;

Procedure TForm13.FormCreate(Sender: TObject);
Begin
  ScrollBox1.Align := alRight;
  Splitter1.Align := alRight;
  SynEdit1.Align := alClient;
  fFrameNameCounter := 0;
  fformShowOnce := true;
End;

Procedure TForm13.FormShow(Sender: TObject);
Begin
  If fformShowOnce Then Begin
    fformShowOnce := false;
    SynEdit1.SetFocus;
  End;
End;

Procedure TForm13.MenuItem1Click(Sender: TObject);
Begin
  SynEdit1.Lines.SaveToFile(fAbsoluteFilename);
  caption := 'File preview: ' + ExtractFileName(frelativeFilename);
End;

Procedure TForm13.MenuItem2Click(Sender: TObject);
Var
  c: TLineComments;
  i: Integer;
Begin
  // Export Comments
  If ScrollBox1.ComponentCount = 0 Then Begin
    showmessage('Error, nothing to export.');
    exit;
  End;
  c := Nil;
  setlength(c, ScrollBox1.ComponentCount);
  For i := 0 To high(c) Do Begin
    c[i].Line := (ScrollBox1.Components[i] As TCommentFrame).Line;
    c[i].Filename := frelativeFilename;
    c[i].Comment := (ScrollBox1.Components[i] As TCommentFrame).Memo1.Text;
  End;
  If SaveDialog3.Execute Then Begin
    ExportCommentsAsCSV(c, SaveDialog3.FileName);
  End;
End;

Procedure TForm13.ScrollBox1Resize(Sender: TObject);
Var
  i: Integer;
Begin
  // Ändert sich die Breite der Scrollbox muss das noch mal "Händisch" Korrigiert werden
  For i := 0 To ScrollBox1.ComponentCount - 1 Do Begin
    (ScrollBox1.Components[i] As TCommentFrame).Width := ScrollBox1.ClientWidth;
  End;
End;

Procedure TForm13.SynEdit1Change(Sender: TObject);
Var
  s: String;
Begin
  s := caption;
  If (s <> '') And (s[length(s)] <> '*') Then Begin
    caption := caption + '*';
  End;
End;

Procedure TForm13.SynEdit1GutterClick(Sender: TObject; X, Y, Line: integer;
  mark: TSynEditMark);
Begin
  AddLineMark(Line);
End;

Procedure TForm13.RepositionAllComments;
Var
  a: Array Of TCommentFrame; // Scrollbox1.Components läst kein reorder zu :( also muss über diesen Zwischenschritt sortiert werden

  Procedure Quick(li, re: integer);
  Var
    l, r, p: Integer;
    h: TCommentFrame;
  Begin
    If Li < Re Then Begin
      p := a[Trunc((li + re) / 2)].Line; // Auslesen des Pivo Elementes
      l := Li;
      r := re;
      While l < r Do Begin
        While a[l].Line < p Do
          inc(l);
        While a[r].Line > p Do
          dec(r);
        If L <= R Then Begin
          h := a[l];
          a[l] := a[r];
          a[r] := h;
          inc(l);
          dec(r);
        End;
      End;
      quick(li, r);
      quick(l, re);
    End;
  End;

Var
  i: Integer;
Begin
  // 1. Sortieren der Components nach Line Aufsteigend
  setlength(a, ScrollBox1.ComponentCount);
  For i := 0 To high(a) Do Begin
    a[i] := ScrollBox1.Components[i] As TCommentFrame;
  End;
  Quick(0, ScrollBox1.ComponentCount - 1);
  For i := 0 To high(a) Do Begin
    a[i].Order := i;
  End;
  // 2. Top eigenschaften anpassen
  For i := 0 To ScrollBox1.ComponentCount - 1 Do Begin
    (ScrollBox1.Components[i] As TCommentFrame).Top := (ScrollBox1.Components[0] As TCommentFrame).Height * (ScrollBox1.Components[i] As TCommentFrame).Order;
  End;
End;

Function TForm13.AddLineMark(Line: Integer): TCommentFrame;
  Procedure SelectLine;
  Var
    i: integer;
  Begin
    // Scrollt die Scrollbox so hin, das das "Line" Element ausgewählt wird
    // Und setzt den Fokus ins Memo
    For i := 0 To ScrollBox1.ControlCount - 1 Do Begin
      If (ScrollBox1.Components[i] As TCommentFrame).Line = Line Then Begin
        ScrollBox1.ScrollInView(ScrollBox1.Components[i] As TControl);
        If visible Then (ScrollBox1.Components[i] As TCommentFrame).Memo1.SetFocus;
        result := ScrollBox1.Components[i] As TCommentFrame;
        break;
      End;
    End;
    // Scrollt das Synedit falls dieses die Zeile nicht anzeigt
    If (line < SynEdit1.TopLine) Or (line >= SynEdit1.TopLine + SynEdit1.Height Div SynEdit1.LineHeight) Then Begin
      SynEdit1.TopLine := SynEdit1.Lines.Count - 1;
      SynEdit1.TopLine := Line;
    End;
  End;
Var
  m: TSynEditMark;
  f: TCommentFrame;
  i: Integer;
Begin
  result := Nil;
  // 1. Suchen ob es die Markierung bereits gibt
  For i := 0 To SynEdit1.Marks.Count - 1 Do Begin
    If SynEdit1.Marks[i].Line = Line Then Begin
      // Wenn Ja wählen wir sie Seitlich an ...
      SelectLine;
      exit;
    End;
  End;
  // Wenn Nein wird sie erstellt ;)
  m := TSynEditMark.Create(SynEdit1);
  m.BookmarkNumber := 0;
  m.Line := Line;
  m.ImageList := ImageList1;
  m.ImageIndex := 0;
  m.Visible := true;
  SynEdit1.Marks.Add(m);
  f := TCommentFrame.Create(ScrollBox1);
  fFrameNameCounter := fFrameNameCounter + 1;
  f.name := 'Comment' + inttostr(fFrameNameCounter);
  f.Parent := ScrollBox1;
  f.Line := Line;
  f.Width := ScrollBox1.ClientWidth;
  f.Anchors := [akLeft, akTop]; // akRight funktioniert hier nicht, deswegen wird das im OnResize korrigiert
  f.Button1.OnClick := @OnRemoveCommentClick;
  f.Memo1.OnClick := @OnSelectLineComment;
  f.PopupMenu := PopupMenu2;
  f.Memo1.PopupMenu := PopupMenu2;
  RepositionAllComments;
  SelectLine;
End;

Procedure TForm13.RemoveLineMark(Line: integer);
Var
  i: Integer;
Begin
  For i := 0 To SynEdit1.Marks.Count - 1 Do Begin
    If SynEdit1.Marks[i].Line = Line Then Begin
      SynEdit1.Marks[i].Free;
      break;
    End;
  End;
  For i := 0 To ScrollBox1.ComponentCount - 1 Do Begin
    If (ScrollBox1.Components[i] As TCommentFrame).Line = Line Then Begin
      ScrollBox1.Components[i].Free;
      break;
    End;
  End;
  fProject.RemoveLineComment(frelativeFilename, line);
  RepositionAllComments;
End;

Procedure TForm13.OnRemoveCommentClick(Sender: TObject);
Begin
  RemoveLineMark(((sender As TButton).Parent As TCommentFrame).Line);
End;

Procedure TForm13.OnSelectLineComment(Sender: TObject);
Begin
  // Add ist hier eigentlich falsch, aber der Add code selectiert auch, wenn Line bereits existiert, also stimmts doch wieder ;)
  AddLineMark(((sender As TMemo).Parent As TCommentFrame).Line);
End;

Procedure TForm13.OpenFile(Project: TProject; Const Filename: String;
  aLine: integer);
Var
  i: Integer;
  c: TLineComments;
  f: TCommentFrame;
Begin
  fProject := Project;
  frelativeFilename := Filename;
  fAbsoluteFilename := ConcatRelativePath(fProject.Rootfolder, Filename);

  If FileExists(fAbsoluteFilename) Then Begin
    SynEdit1.Lines.LoadFromFile(fAbsoluteFilename);
    For i := SynEdit1.Marks.Count - 1 Downto 0 Do Begin
      SynEdit1.Marks[i].free;
    End;
    For i := ScrollBox1.ComponentCount - 1 Downto 0 Do Begin
      ScrollBox1.Components[i].free;
    End;
    c := fProject.GetLineComments();
    For i := 0 To high(c) Do Begin
      If c[i].Filename = frelativeFilename Then Begin
        f := AddLineMark(c[i].Line);
        f.Memo1.Text := c[i].Comment;
      End;
    End;
    SynEdit1.TopLine := 0;
    If aLine <> -1 Then Begin
      SynEdit1.TopLine := SynEdit1.Lines.Count - 1;
      SynEdit1.Invalidate;
      SynEdit1.TopLine := aLine;
      SynEdit1.CaretY := aLine;
      fformShowOnce := true; // Synedit1.setfocus triggern
    End;
    caption := 'File preview: ' + ExtractFileName(frelativeFilename);
    ShowModal;
  End
  Else Begin
    showmessage('Error, unable to find: ' + fAbsoluteFilename);
  End;
End;

End.

