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
Unit Unit8;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ufpc_understand,
  ugraphs;

Type

  TMethodDeclaration = Record
    Name: String;
    lName: String; // =Lowercase(Name);
    FileName: String;
    ClassName: String;
    FilesInfo: TPoint; // FFiles[x].Methods[y]
  End;

  TCall = Record
    Caller: Integer; // Der Aufrufende
    Calle: Integer; // Der Aufgerufene
  End;

  TMode = (mNone, mMethods, mClasses, mUnits);


  { TForm8 }

  TForm8 = Class(TForm)
    GraphBox1: TGraphBox;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem20: TMenuItem;
    MenuItem21: TMenuItem;
    MenuItem22: TMenuItem;
    MenuItem23: TMenuItem;
    MenuItem24: TMenuItem;
    MenuItem25: TMenuItem;
    MenuItem26: TMenuItem;
    MenuItem27: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    PopupMenu1: TPopupMenu;
    SaveDialog1: TSaveDialog;
    Separator1: TMenuItem;
    Separator2: TMenuItem;
    Separator3: TMenuItem;
    Separator4: TMenuItem;
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
    Procedure FormCreate(Sender: TObject);
    Procedure MenuItem10Click(Sender: TObject);
    Procedure MenuItem11Click(Sender: TObject);
    Procedure MenuItem12Click(Sender: TObject);
    Procedure MenuItem13Click(Sender: TObject);
    Procedure MenuItem19Click(Sender: TObject);
    Procedure MenuItem20Click(Sender: TObject);
    Procedure MenuItem21Click(Sender: TObject);
    Procedure MenuItem22Click(Sender: TObject);
    Procedure MenuItem23Click(Sender: TObject);
    Procedure MenuItem24Click(Sender: TObject);
    Procedure MenuItem25Click(Sender: TObject);
    Procedure MenuItem26Click(Sender: TObject);
    Procedure MenuItem27Click(Sender: TObject);
    Procedure MenuItem3Click(Sender: TObject);
    Procedure MenuItem4Click(Sender: TObject);
    Procedure MenuItem5Click(Sender: TObject);
    Procedure MenuItem6Click(Sender: TObject);
    Procedure MenuItem8Click(Sender: TObject);
    Procedure MenuItem9Click(Sender: TObject);
  private
    fMode: TMode;
    fMethods: Array Of TMethodDeclaration; // Liste Aller Verfügbaren Methoden
    Calls: Array Of TCall;
    fNodeInfoFile: String;

    mMovePos, mDownPos: TPoint;
    fShowRectangle: Boolean;

    Function getMethodIndex(Value: String): integer;


    Procedure GraphboxMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    Procedure GraphboxMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    Procedure GraphboxMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    Procedure GraphboxPaint(Sender: TObject);
    Procedure GraphboxDblClick(Sender: TObject);

    Procedure LoadNodesPos();
    Procedure SaveNodesPos();

    Procedure LoadMethods();
    Procedure LoadClasses();
    Procedure LoadUnits();
    Procedure OnDeleteUserDataEvent(Sender: TObject; UserData: Pointer);
  public
    Procedure CreateCallerList(Const FFiles: TProjectFilesInfo; NodeInfoFile: String);
  End;

Var
  Form8: TForm8;

Implementation

Uses IniFiles, LazFileUtils, Unit9, Unit10;

{$R *.lfm}

Const
  Grid_step = 8;

Operator = (a, b: TMethodDeclaration): Boolean;
Begin
  result := (a.lName = b.lName)
    And (a.FileName = b.FileName) // Dadurch dass die Dateinamen direkt vom System kommen brauchen wir die nicht Case Sensitiv machen ;)
  And (lowercase(a.ClassName) = lowercase(b.ClassName));
End;

Function MethodDeclaration(aClassName, aName, aFilename: String): TMethodDeclaration;
Begin
  result.ClassName := aClassName;
  result.Name := aName;
  result.lName := LowerCase(aName);
  result.FileName := aFilename;
End;

{ TForm8 }

Procedure TForm8.FormCreate(Sender: TObject);
Begin
  fMode := mNone;
  caption := 'Callgraph info (beta)';
  fShowRectangle := false;
  GraphBox1 := TGraphBox.Create(self);
  GraphBox1.Name := 'GraphBox1';
  GraphBox1.Parent := Self;
  GraphBox1.Align := alClient;
  GraphBox1.Autoselect := false;
  GraphBox1.OnMouseUp := @GraphboxMouseUp;
  GraphBox1.OnMouseDown := @GraphboxMouseDown;
  GraphBox1.OnMouseMove := @GraphboxMouseMove;
  GraphBox1.OnPaint := @GraphboxPaint;
  GraphBox1.OnDblClick := @GraphboxDblClick;
  GraphBox1.PopupMenu := PopupMenu1;
  GraphBox1.Graph.OnDeleteNodeUserData := @OnDeleteUserDataEvent;
End;

Procedure TForm8.MenuItem10Click(Sender: TObject);
Var
  sl: TStringList;
  i, j: Integer;
  aunit: String;
  p: ^TMethodDeclaration;
  n: TNode;
Begin
  // Mark by Unit
  sl := TStringList.Create;
  sl.Sorted := True;
  sl.Duplicates := dupIgnore;
  For i := 0 To high(fMethods) Do Begin
    aunit := lowercase(ExtractFileNameOnly(fMethods[i].FileName));
    sl.add(aunit);
  End;
  form10.CheckListBox1.items.Clear;
  form10.CheckListBox1.items.AddCommaText(sl.CommaText);
  If form10.ShowModal = mrOK Then Begin
    For i := 0 To form10.CheckListBox1.Items.Count - 1 Do Begin
      If form10.CheckListBox1.Checked[i] Then Begin
        For j := 0 To GraphBox1.Graph.NodeCount - 1 Do Begin
          p := GraphBox1.Graph.Node[j].UserData;
          If lowercase(ExtractFileNameOnly(p^.FileName)) = form10.CheckListBox1.Items[i] Then Begin
            n := GraphBox1.Graph.Node[j];
            n.Marked := true;
            GraphBox1.Graph.Node[j] := n;
          End;
        End;
      End;
    End;
    GraphBox1.Invalidate;
  End;
  sl.free;
End;

Procedure TForm8.MenuItem11Click(Sender: TObject);
Var
  sl: TStringList;
  i, j: Integer;
  aunit: String;
  p: ^TMethodDeclaration;
  n: TNode;
Begin
  // Mark by Class
  sl := TStringList.Create;
  sl.Sorted := True;
  sl.Duplicates := dupIgnore;
  For i := 0 To high(fMethods) Do Begin
    aunit := lowercase(fMethods[i].ClassName);
    sl.add(aunit);
  End;
  form10.CheckListBox1.items.Clear;
  form10.CheckListBox1.items.AddCommaText(sl.CommaText);
  If form10.ShowModal = mrOK Then Begin
    For i := 0 To form10.CheckListBox1.Items.Count - 1 Do Begin
      If form10.CheckListBox1.Checked[i] Then Begin
        For j := 0 To GraphBox1.Graph.NodeCount - 1 Do Begin
          p := GraphBox1.Graph.Node[j].UserData;
          If lowercase(p^.ClassName) = form10.CheckListBox1.Items[i] Then Begin
            n := GraphBox1.Graph.Node[j];
            n.Marked := true;
            GraphBox1.Graph.Node[j] := n;
          End;
        End;
      End;
    End;
    GraphBox1.Invalidate;
  End;
  sl.free;
End;

Procedure TForm8.MenuItem12Click(Sender: TObject);
Var
  i: Integer;
  n: TNode;
Begin
  // Mark by selected
  For i := 0 To GraphBox1.Graph.NodeCount - 1 Do Begin
    If GraphBox1.Graph.Node[i].Selected Then Begin
      n := GraphBox1.Graph.Node[i];
      n.Marked := true;
      GraphBox1.Graph.Node[i] := n;
    End;
  End;
  GraphBox1.Invalidate;
End;

Procedure TForm8.MenuItem13Click(Sender: TObject);
Var
  i: Integer;
  n: TNode;
Begin
  // UnMark by selected
  For i := 0 To GraphBox1.Graph.NodeCount - 1 Do Begin
    If GraphBox1.Graph.Node[i].Selected Then Begin
      n := GraphBox1.Graph.Node[i];
      n.Marked := false;
      GraphBox1.Graph.Node[i] := n;
    End;
  End;
  GraphBox1.Invalidate;
End;

Procedure TForm8.MenuItem19Click(Sender: TObject);
Begin
  LoadMethods();
End;

Procedure TForm8.MenuItem20Click(Sender: TObject);
Begin
  LoadClasses();
End;

Procedure TForm8.MenuItem21Click(Sender: TObject);
Begin
  LoadUnits();
End;

Procedure TForm8.MenuItem22Click(Sender: TObject);
Begin
  Case fMode Of
    mMethods: Begin
        MenuItem19.Checked := true;
        LoadMethods();
      End;
    mClasses: Begin
        MenuItem20.Checked := true;
        LoadClasses();
      End;
    mUnits, mNone: Begin // Default laden wir mal die Units dass ist am wenigsten "Verstörend" ;)
        MenuItem21.Checked := true;
        LoadUnits();
      End;
  End;
End;

Procedure TForm8.MenuItem23Click(Sender: TObject);
Var
  i: Integer;
  b: Boolean;
Begin
  // Mark all parents
  b := false;
  For i := 0 To GraphBox1.Graph.NodeCount - 1 Do Begin
    If GraphBox1.Graph.Node[i].Selected Then Begin
      b := true;
      GraphBox1.Graph.MarkFistGenParents(i);
    End;
  End;
  If b Then Begin
    GraphBox1.Invalidate;
  End
  Else Begin
    Showmessage('Error, no selected nodes, please select nodes first.');
  End;
End;

Procedure TForm8.MenuItem24Click(Sender: TObject);
Var
  i: Integer;
  b: Boolean;
Begin
  // Mark all parents recursive
  b := false;
  For i := 0 To GraphBox1.Graph.NodeCount - 1 Do Begin
    If GraphBox1.Graph.Node[i].Selected Then Begin
      b := true;
      GraphBox1.Graph.MarkAllParents(i);
    End;
  End;
  If b Then Begin
    GraphBox1.Invalidate;
  End
  Else Begin
    Showmessage('Error, no selected nodes, please select nodes first.');
  End;
End;

Procedure TForm8.MenuItem25Click(Sender: TObject);
Var
  i: Integer;
  b: Boolean;
Begin
  // Mark all Childs
  b := false;
  For i := 0 To GraphBox1.Graph.NodeCount - 1 Do Begin
    If GraphBox1.Graph.Node[i].Selected Then Begin
      b := true;
      GraphBox1.Graph.MarkFistGenChilds(i);
    End;
  End;
  If b Then Begin
    GraphBox1.Invalidate;
  End
  Else Begin
    Showmessage('Error, no selected nodes, please select nodes first.');
  End;
End;

Procedure TForm8.MenuItem26Click(Sender: TObject);
Var
  i: Integer;
  b: Boolean;
Begin
  // Mark all Childs recursive
  b := false;
  For i := 0 To GraphBox1.Graph.NodeCount - 1 Do Begin
    If GraphBox1.Graph.Node[i].Selected Then Begin
      b := true;
      GraphBox1.Graph.MarkAllChilds(i);
    End;
  End;
  If b Then Begin
    GraphBox1.Invalidate;
  End
  Else Begin
    Showmessage('Error, no selected nodes, please select nodes first.');
  End;
End;

Procedure TForm8.MenuItem27Click(Sender: TObject);
Begin
  // Mark Cycles
  GraphBox1.Graph.UndoMarkings;
  GraphBox1.Graph.MarkCycles();
  GraphBox1.Invalidate;
End;

Procedure TForm8.FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
Begin
  // Speichern
  SaveNodesPos();
End;

Procedure TForm8.MenuItem3Click(Sender: TObject);
Var
  i: integer;
  n: TNode;
Begin
  If MenuItem3.Checked Then Begin
    For i := 0 To GraphBox1.Graph.NodeCount - 1 Do Begin
      n := GraphBox1.Graph.Node[i];
      n.Position.x := n.Position.x - n.Position.x Mod Grid_step;
      n.Position.Y := n.Position.Y - n.Position.Y Mod Grid_step;
      GraphBox1.Graph.Node[i] := n;
    End;
    //fProject.Change();
    GraphBox1.Invalidate;
  End;
End;

Procedure TForm8.MenuItem4Click(Sender: TObject);
Const
  aborder = 15;
Var
  n: integer;
  w, h, i: integer;
  node: TNode;
Begin
  // Center out of range nodes
  n := GraphBox1.Graph.NodeCount;
  w := GraphBox1.Width - 2 * aborder;
  h := GraphBox1.Height - 2 * aborder;
  For i := 0 To n - 1 Do Begin
    node := GraphBox1.Graph.Node[i];
    If (node.Position.x > w) Or (node.Position.X < aborder) Or
      (node.Position.Y > h) Or (node.Position.y < aborder) Then Begin
      node.Position.x := aborder + random(w);
      node.Position.Y := aborder + random(h);
      If MenuItem3.Checked Then Begin
        node.Position.x := node.Position.x - node.Position.x Mod Grid_step;
        node.Position.Y := node.Position.Y - node.Position.Y Mod Grid_step;
      End;
      GraphBox1.Graph.Node[i] := node;
    End;
  End;
  GraphBox1.Invalidate;
End;

Procedure TForm8.MenuItem5Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm8.MenuItem6Click(Sender: TObject);
Var
  sl: TStringList;
  i: Integer;
Begin
  // Export as CSV
  //Exportieren der gesamten Liste, dann Fehler finden.
  If SaveDialog1.Execute Then Begin
    sl := TStringList.Create;
    sl.add('Caller;;;Calle;');
    sl.add('Filename;Classname;Name;Filename;Classname;Name;');
    For i := 0 To high(Calls) Do Begin
      sl.add(
        fMethods[Calls[i].Caller].FileName + ';' +
        fMethods[Calls[i].Caller].ClassName + ';' +
        fMethods[Calls[i].Caller].Name + ';' +
        fMethods[Calls[i].Calle].FileName + ';' +
        fMethods[Calls[i].Calle].ClassName + ';' +
        fMethods[Calls[i].Calle].Name + ';');
    End;
    sl.SaveToFile(SaveDialog1.FileName);
    sl.free;
  End;
End;

Procedure TForm8.MenuItem8Click(Sender: TObject);
Begin
  // Clear Marks
  GraphBox1.Graph.UndoMarkings;
  GraphBox1.Invalidate;
End;

Procedure TForm8.MenuItem9Click(Sender: TObject);
Begin
  // Export Marks as CSV
End;

Function TForm8.getMethodIndex(Value: String): integer;
Var
  i: Integer;
Begin
  // TODO: Via Binärer Suche Realisieren !, Frage wie weit kommt die Suche mit mehrfach gleichen enträgen zurecht ?
  result := -1;
  value := LowerCase(Value);
  For i := 0 To high(fMethods) Do Begin
    If fMethods[i].lName = Value Then Begin
      result := i;
      exit;
    End;
  End;
End;

Procedure TForm8.GraphboxMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Begin
  If fShowRectangle Then Begin
    GraphBox1.Graph.SelectRectangle(mDownPos.x, mDownPos.y, mMovePos.X, mMovePos.y, GraphBox1.Canvas);
  End;
  fShowRectangle := false;
  GraphBox1.Invalidate;
End;

Procedure TForm8.GraphboxMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
Var
  n: TNode;
  i: integer;
  bool: Boolean;
  mxx, myy, dx, dy: Integer;
Begin
  If ssleft In shift Then Begin
    bool := false;
    dx := mMovePos.x - x;
    dy := mMovePos.y - y;
    If MenuItem3.Checked Then Begin
      mxx := dx Mod Grid_step;
      myy := dy Mod Grid_step;
      dx := (dx Div Grid_step) * Grid_step;
      dy := (dy Div Grid_step) * Grid_step;
    End
    Else Begin
      mxx := 0;
      myy := 0;
    End;
    For i := 0 To GraphBox1.Graph.NodeCount - 1 Do Begin
      If GraphBox1.Graph.node[i].Selected Then Begin
        n := GraphBox1.Graph.node[i];
        n.Position.X := n.Position.X - dx;
        n.Position.Y := n.Position.Y - dy;
        GraphBox1.Graph.node[i] := n;
        //fProject.Change();
        bool := true;
      End;
    End;
    mMovePos.x := x + mxx;
    mMovePos.y := y + myy;
    fShowRectangle := Not bool;
    GraphBox1.Invalidate;
  End
  Else Begin
    mMovePos := point(x, y);
  End;
End;

Procedure TForm8.GraphboxMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Var
  index: Integer;
Begin
  mDownPos := point(x, y);
  mMovePos := point(x, y);
  (*
   * Anwahl: - Click auf nicht selectierte Node (dadurch abwahl aller anderen)
   *         - STRG + Click auf Node (alle anderen bleiben angewählt)
   *
   * Abwahl: - Click ins Freie --> Alle Abwählen
   *         - STRG + Click auf Selected --> nur eine abwählen
   *)

  If (Not (ssShift In shift)) Then Begin
    index := GraphBox1.Graph.GetNodeIndex(x, y, GraphBox1.Canvas);
    If (ssCtrl In Shift) And (index <> -1) Then Begin
      If GraphBox1.Graph.Node[index].Selected Then Begin
        GraphBox1.Graph.DeSelect(index);
        GraphBox1.Invalidate;
        exit; // Sonst würde der unten gleich wieder angewählt, das wollen wir hier aber nicht..
      End
      Else Begin
        GraphBox1.Graph.Select(index);
      End;
    End
    Else Begin
      If (index = -1) Or (Not GraphBox1.Graph.Node[index].Selected) Then Begin
        GraphBox1.Graph.DeSelectAll();
      End;
    End;
  End;
  GraphBox1.Graph.Select(GraphBox1.Graph.GetNodeIndex(x, y, GraphBox1.canvas));
  GraphBox1.Invalidate;
End;

Procedure TForm8.GraphboxPaint(Sender: TObject);
Begin
  If fShowRectangle Then Begin
    GraphBox1.Canvas.Brush.Style := bsClear;
    GraphBox1.Canvas.Pen.Color := clRed;
    GraphBox1.Canvas.Rectangle(mDownPos.x, mDownPos.y, mMovePos.X, mMovePos.y);
  End;
End;

Procedure TForm8.GraphboxDblClick(Sender: TObject);
Var
  index: integer;
  p: ^TMethodDeclaration;
  fn, cn, n: String;
Begin
  index := GraphBox1.Graph.GetNodeIndex(mDownPos.X, mDownPos.Y, GraphBox1.Canvas);
  If index <> -1 Then Begin
    p := GraphBox1.Graph.Node[index].UserData;
    fn := p^.FileName;
    cn := p^.ClassName;
    n := p^.Name;
    Case fMode Of
      mMethods: Begin
          // nichts
        End;
      mClasses: Begin
          n := '';
        End;
      mUnits: Begin
          cn := '';
          n := '';
        End;
    End;
    form9.LoadNode(fn, cn, n);
    form9.ShowModal;
  End;
End;

Procedure TForm8.LoadNodesPos;
Var
  i: Integer;
  ini: TIniFile;
  g: String;
  n: TNode;
Begin
  Case fMode Of
    mMethods: g := 'MethodsNodes';
    mClasses: g := 'ClassesNodes';
    mUnits: g := 'UnitsNodes';
    mNone: Exit; // irgendwas stimmt hier nicht, raus ..
  End;
  ini := TIniFile.Create(fNodeInfoFile);
  For i := 0 To GraphBox1.Graph.NodeCount - 1 Do Begin
    n := GraphBox1.Graph.Node[i];
    n.position.x := ini.ReadInteger(g, n.Name + 'X', n.Position.X);
    n.position.Y := ini.ReadInteger(g, n.Name + 'Y', n.Position.Y);
    GraphBox1.Graph.Node[i] := n;
  End;
  ini.free;
  MenuItem3Click(Nil); // ggf ausrichten am Raster
End;

Procedure TForm8.SaveNodesPos;
Var
  i: Integer;
  ini: TIniFile;
  g: String;
  n: TNode;
Begin
  Case fMode Of
    mMethods: g := 'MethodsNodes';
    mClasses: g := 'ClassesNodes';
    mUnits: g := 'UnitsNodes';
    mNone: exit;
  End;
  ini := TIniFile.Create(fNodeInfoFile);
  ini.CacheUpdates := true;
  For i := 0 To GraphBox1.Graph.NodeCount - 1 Do Begin
    n := GraphBox1.Graph.Node[i];
    ini.WriteInteger(g, n.Name + 'X', n.Position.X);
    ini.WriteInteger(g, n.Name + 'Y', n.Position.Y);
  End;
  ini.UpdateFile;
  ini.free;
End;

Procedure TForm8.LoadMethods;
Var
  i, si, ei: Integer;
  a, b: String;
Begin
  SaveNodesPos;
  fMode := mMethods;
  GraphBox1.Graph.Clear;
  For i := 0 To high(Calls) Do Begin
    a := fMethods[Calls[i].Caller].Name;
    b := fMethods[Calls[i].Calle].Name;
    si := GraphBox1.Graph.AddNode(fMethods[Calls[i].Caller].FileName + '.' + fMethods[Calls[i].Caller].ClassName + '.' + fMethods[Calls[i].Caller].lName, a, @fMethods[Calls[i].Caller]); // Caller
    ei := GraphBox1.Graph.AddNode(fMethods[Calls[i].Calle].FileName + '.' + fMethods[Calls[i].Calle].ClassName + '.' + fMethods[Calls[i].Calle].lName, b, @fMethods[Calls[i].Calle]); // Calle
    If (Not MenuItem22.Checked) Or (a <> b) Then Begin
      GraphBox1.Graph.AddEdge(si, ei, clblack, Nil, true);
    End;
  End;
  LoadNodesPos();
  GraphBox1.Invalidate;
End;

Procedure TForm8.LoadClasses;
Var
  i, si, ei: Integer;
  a, b: String;
Begin
  SaveNodesPos;
  fMode := mClasses;
  GraphBox1.Graph.Clear;
  For i := 0 To high(Calls) Do Begin
    If (fMethods[Calls[i].Caller].ClassName <> '') And (fMethods[Calls[i].Calle].ClassName <> '') Then Begin
      a := fMethods[Calls[i].Caller].ClassName;
      b := fMethods[Calls[i].Calle].ClassName;
      si := GraphBox1.Graph.AddNode(fMethods[Calls[i].Caller].FileName + '.' + fMethods[Calls[i].Caller].ClassName, a, @fMethods[Calls[i].Caller]); // Caller
      ei := GraphBox1.Graph.AddNode(fMethods[Calls[i].Calle].FileName + '.' + fMethods[Calls[i].Calle].ClassName, b, @fMethods[Calls[i].Calle]); // Calle
      If (Not MenuItem22.Checked) Or (a <> b) Then Begin
        GraphBox1.Graph.AddEdge(si, ei, clblack, Nil, true);
      End;
    End;
  End;
  LoadNodesPos();
  GraphBox1.Invalidate;
End;

Procedure TForm8.LoadUnits;
Var
  i, si, ei: Integer;
  a, b: String;
Begin
  SaveNodesPos;
  fMode := mUnits;
  GraphBox1.Graph.Clear;
  For i := 0 To high(Calls) Do Begin
    a := ExtractFileNameOnly(fMethods[Calls[i].Caller].FileName);
    b := ExtractFileNameOnly(fMethods[Calls[i].Calle].FileName);
    si := GraphBox1.Graph.AddNode(fMethods[Calls[i].Caller].FileName, a, @fMethods[Calls[i].Caller]); // Caller
    ei := GraphBox1.Graph.AddNode(fMethods[Calls[i].Calle].FileName, b, @fMethods[Calls[i].Calle]); // Calle
    If (Not MenuItem22.Checked) Or (a <> b) Then Begin
      GraphBox1.Graph.AddEdge(si, ei, clblack, Nil, true);
    End;
  End;
  LoadNodesPos();
  GraphBox1.Invalidate;
End;

Procedure TForm8.OnDeleteUserDataEvent(Sender: TObject; UserData: Pointer);
Begin
  // Nichts sind ja addressen in ein definiertes Array !
End;

Procedure TForm8.CreateCallerList(Const FFiles: TProjectFilesInfo;
  NodeInfoFile: String);
  Procedure Quick(li, re: integer);
  Var
    l, r: Integer;
    h: TMethodDeclaration;
    p: String;
  Begin
    If Li < Re Then Begin
      p := fMethods[Trunc((li + re) / 2)].lName; // Auslesen des Pivo Elementes
      l := Li;
      r := re;
      While l < r Do Begin
        While CompareStr(fMethods[l].lName, p) < 0 Do
          inc(l);
        While CompareStr(fMethods[r].lName, p) > 0 Do
          dec(r);
        If L <= R Then Begin
          h := fMethods[l];
          fMethods[l] := fMethods[r];
          fMethods[r] := h;
          inc(l);
          dec(r);
        End;
      End;
      quick(li, r);
      quick(l, re);
    End;
  End;

Var
  CallsCnt, MethodsCnt, i, j, l, index, k: integer;
  found: Boolean;
  unitfile: String;
Begin
  fNodeInfoFile := NodeInfoFile;
  // 1. Erstellen einer Liste aller Verfügbarer Funktionen aus Allen Dateien
  setlength(fMethods, 1024);
  MethodsCnt := 0;
  For i := 0 To high(FFiles) Do Begin
    For j := 0 To high(FFiles[i].Methods) Do Begin
      fMethods[MethodsCnt].ClassName := FFiles[i].Methods[j].ClassName;
      fMethods[MethodsCnt].Name := FFiles[i].Methods[j].Name;
      fMethods[MethodsCnt].lName := lowercase(FFiles[i].Methods[j].Name);
      fMethods[MethodsCnt].FileName := FFiles[i].Methods[j].Filename;
      fMethods[MethodsCnt].FilesInfo := point(i, j);
      inc(MethodsCnt);
      If MethodsCnt > high(fMethods) Then Begin
        setlength(fMethods, high(fMethods) + 1025);
      End;
    End;
  End;
  // 2. Sortieren der Liste (damit getMethodIndex functioniert)
  setlength(fMethods, MethodsCnt);
  Quick(0, MethodsCnt - 1);
  // 3. Durchgehen aller Identifiers aller Funktionen und schaun ob diese in der Liste aller Funktionen enthalten ist
  //    \-> ja, dann haben wir ne Kante für den Graphen ;)
  callsCnt := 0;
  setlength(Calls, 1024);

  For i := 0 To high(fMethods) Do Begin
    For k := 0 To FFiles[fMethods[i].FilesInfo.X].Methods[fMethods[i].FilesInfo.Y].IdentifierListCnt - 1 Do Begin
      l := getMethodIndex(FFiles[fMethods[i].FilesInfo.X].Methods[fMethods[i].FilesInfo.Y].IdentifierList[k]);
      If l >= 0 Then Begin
        // Theoretisch will der nun i auf l verlinken
        index := l;
        // Aber nicht nur l auch alle nachfolger die den Selben namen haben !
        While (index < high(fMethods)) And (fMethods[l].lName = fMethods[index].lName) Do Begin
          (*
           Das Problem ist, dass wir hier viel zu viele finden, weil wir ja keine echt Code analyse haben
           Ruft eine Funktion TXY.Create z.b. inherited create auf -> Dann findet die viel zu viele
           das muss nun eingeschränkt werden
           1. Durch die Uses (ist die ziel funktion in einer anderen unit geht das gar nicht -> Raus
           2. (Zukunft) über die Anzahl der Parameter, denn auch die muss stimmen ;)
              \-> Die Anzahl der Parameter der Funktionen ist noch einfach zu bestimmen, aber ob ein Identifier eine Nachfolgende Parameterliste hast nicht mehr ganz so einfach ...
              \=> Beides muss der ufpsparser bereit stellen, sonst geht gar nichts !
           *)
          // 1. Prüfen der Uses
          unitfile := lowercase(ExtractFileNameOnly(fMethods[index].FileName));
          found := unitfile = lowercase(ExtractFileNameOnly(fMethods[i].FileName)); // Alle "eigenen" Funktionen werden ja auf jeden Fall gefunden
          If Not found Then Begin
            For j := 0 To high(FFiles[fMethods[i].FilesInfo.X].FileInfo.aUses) Do Begin
              If lowercase(FFiles[fMethods[i].FilesInfo.X].FileInfo.aUses[j]) = unitfile Then Begin
                found := true;
                break;
              End;
            End;
          End;
          If Not found Then Begin
            For j := 0 To high(FFiles[fMethods[i].FilesInfo.X].FileInfo.aImplementationUses) Do Begin
              If lowercase(FFiles[fMethods[i].FilesInfo.X].FileInfo.aImplementationUses[j]) = unitfile Then Begin
                found := true;
                break;
              End;
            End;
          End;
          If found Then Begin
            calls[CallsCnt].Caller := i;
            calls[CallsCnt].Calle := index;
            inc(CallsCnt);
            If CallsCnt > high(Calls) Then Begin
              setlength(Calls, high(Calls) + 1025);
            End;
          End;
          inc(index);
        End;
      End;
    End;
  End;
  setlength(Calls, CallsCnt);
  MenuItem22Click(Nil);
  fShowRectangle := false;
End;

End.

