(******************************************************************************)
(* FPC Understand                                                  30.03.2023 *)
(*                                                                            *)
(* Version     : 0.31                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : Simple statical code analysis tool                           *)
(*                                                                            *)
(* License     : See the file license.md, located under:                      *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(* Warranty    : There is no warranty, neither in correctness of the          *)
(*               implementation, nor anything other that could happen         *)
(*               or go wrong, use at your own risk.                           *)
(*                                                                            *)
(* History     : 0.01 - Initialversion (dependency graph, Klass Analysis,     *)
(*                      Statistik)                                            *)
(*               0.02 - Counter ersetzt durch FPCParser                       *)
(*                      Details bei den Statistiken                           *)
(*                      Editieren von .lpi Files                              *)
(*                      Beim Doppelclick eine Auswahl was angezeigt werden    *)
(*                        soll                                                *)
(*                      Submenüs deutlich überarbeitet und verbessert         *)
(*                        => Code Refactoring arbeiten mit .lpi Dateien       *)
(*                      Automatisches Center der Knoten nach erst Erstellung  *)
(*               0.03 - Keywort suche mittels binärer Suche -> Speedup        *)
(*               0.04 - Start with Callgraph Evaluation                       *)
(*               0.05 - Add Visible menues in Callgraph Evaluation Screen     *)
(*               0.06 - Add chart statistics                                  *)
(*               0.07 - Merge pull reques by h-elsner                         *)
(*                      support different root folders for different OS       *)
(*               0.08 - Add Circle graphs to chart statistics                 *)
(*                      Add Barcharts to chart statistics                     *)
(*               0.09 - remove treeview in settings                           *)
(*                      Add chart statistic options                           *)
(*               0.10 - show method length in "show cyclomatic complexity"    *)
(*               0.11 - improve color selection funktion in chart statistics  *)
(*                      Better support for porting project files.             *)
(*               0.12 - Support project as start param                        *)
(*               0.13 - Merge pull request by User4martin                     *)
(*               0.14 - give access to searchpaths                            *)
(*               0.15 - Align nodes on grid if option is selected during      *)
(*                        "Arrange all visible nodes by dependency"           *)
(*               0.16 - ufpcParser could not handle class of declarations     *)
(*               0.17 - ufpcParser was case sensitiv in parsing uses clauses  *)
(*               0.18 - Add Scrollbars                                        *)
(*               0.19 - fix AV on adding nonproject files to .fpu             *)
(*                      take {$I into account                                 *)
(*               0.20 - fix, {$I search was case sensitive but should not     *)
(*               0.21 - inc UX after user request                             *)
(*               0.22 - Adjust handling when skipping .lpi file loading during*)
(*                      startup                                               *)
(*                      Enable Filedrop                                       *)
(*               0.23 - Reorder MainMenu                                      *)
(*               0.24 - FIX: graphical glitches in chart statistics           *)
(*                      ADD: more infos to chart statistics                   *)
(*               0.25 - ADD: Code preview / review feature                    *)
(*                      ADD: Mark nodes with comments                         *)
(*               0.26 - FIX: Line index after {$I ...}                        *)
(*               0.27 - ADD: Support recursive {$I ...}                       *)
(*                      FIX: Set carety when showing code editor              *)
(*               0.28 - FIX: invalid Rootfolder                               *)
(*               0.29 - FIX: automatically add .lpi path to searchpaths       *)
(*               0.30 - FIX: select root folder dialog was not clear enough   *)
(*               0.31 - ADD: Total CC to statistics                           *)
(*                                                                            *)
(* Known Bugs  : - if a project holds 2 units with the same name              *)
(*                 the dependency graph will merge them to one                *)
(*                                                                            *)
(* Missing     : - Callgraphen (über Klassen, über Echte Methoden,            *)
(*                   über Units ..)                                           *)
(*                                                                            *)
(******************************************************************************)
(*  Silk icon set 1.3 used                                                    *)
(*  ----------------------                                                    *)
(*  Mark James                                                                *)
(*   https://peacocksoftware.com/silk                                         *)
(******************************************************************************)
(*  This work is licensed under a                                             *)
(*  Creative Commons Attribution 2.5 License.                                 *)
(*  [ http://creativecommons.org/licenses/by/2.5/ ]                           *)
(******************************************************************************)
Unit Unit1;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, IniPropStorage,
  StdCtrls, ugraphs, ufpc_understand, ufpcparser, LvlGraphCtrl, Types;

Const
  Version = '0.31';
  ScrollDelta = 25;

Type

  { TForm1 }

  TForm1 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    IniPropStorage1: TIniPropStorage;
    Label1: TLabel;
    Label2: TLabel;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    GraphBox1: TGraphBox;
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
    MenuItem28: TMenuItem;
    MenuItem29: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem30: TMenuItem;
    MenuItem31: TMenuItem;
    MenuItem32: TMenuItem;
    MenuItem33: TMenuItem;
    MenuItem34: TMenuItem;
    MenuItem35: TMenuItem;
    MenuItem36: TMenuItem;
    MenuItem37: TMenuItem;
    MenuItem38: TMenuItem;
    MenuItem39: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem40: TMenuItem;
    MenuItem41: TMenuItem;
    MenuItem42: TMenuItem;
    MenuItem43: TMenuItem;
    MenuItem44: TMenuItem;
    MenuItem45: TMenuItem;
    MenuItem46: TMenuItem;
    MenuItem47: TMenuItem;
    MenuItem48: TMenuItem;
    MenuItem49: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem50: TMenuItem;
    MenuItem51: TMenuItem;
    MenuItem52: TMenuItem;
    MenuItem53: TMenuItem;
    MenuItem54: TMenuItem;
    MenuItem55: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    OpenDialog1: TOpenDialog;
    OpenDialog2: TOpenDialog;
    PopupMenu1: TPopupMenu;
    PopupMenu2: TPopupMenu;
    SaveDialog1: TSaveDialog;
    SaveDialog2: TSaveDialog;
    SaveDialog3: TSaveDialog;
    ScrollBar1: TScrollBar;
    ScrollBar2: TScrollBar;
    Separator1: TMenuItem;
    Separator10: TMenuItem;
    Separator2: TMenuItem;
    Separator3: TMenuItem;
    Separator4: TMenuItem;
    Separator5: TMenuItem;
    Separator6: TMenuItem;
    Separator7: TMenuItem;
    Separator8: TMenuItem;
    Separator9: TMenuItem;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
    Procedure FormCreate(Sender: TObject);
    Procedure FormDropFiles(Sender: TObject; Const FileNames: Array Of String);
    Procedure FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; Var Handled: Boolean);
    Procedure FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; Var Handled: Boolean);
    Procedure FormResize(Sender: TObject);
    Procedure MenuItem10Click(Sender: TObject);
    Procedure MenuItem11Click(Sender: TObject);
    Procedure MenuItem12Click(Sender: TObject);
    Procedure MenuItem13Click(Sender: TObject);
    Procedure MenuItem14Click(Sender: TObject);
    Procedure MenuItem15Click(Sender: TObject);
    Procedure MenuItem16Click(Sender: TObject);
    Procedure MenuItem17Click(Sender: TObject);
    Procedure MenuItem18Click(Sender: TObject);
    Procedure MenuItem19Click(Sender: TObject);
    Procedure MenuItem20Click(Sender: TObject);
    Procedure MenuItem21Click(Sender: TObject);
    Procedure MenuItem22Click(Sender: TObject);
    Procedure MenuItem23Click(Sender: TObject);
    Procedure MenuItem24Click(Sender: TObject);
    Procedure MenuItem25Click(Sender: TObject);
    Procedure MenuItem27Click(Sender: TObject);
    Procedure MenuItem28Click(Sender: TObject);
    Procedure MenuItem2Click(Sender: TObject);
    Procedure MenuItem31Click(Sender: TObject);
    Procedure MenuItem32Click(Sender: TObject);
    Procedure MenuItem3Click(Sender: TObject);
    Procedure MenuItem42Click(Sender: TObject);
    Procedure MenuItem43Click(Sender: TObject);
    Procedure MenuItem44Click(Sender: TObject);
    Procedure MenuItem46Click(Sender: TObject);
    Procedure MenuItem48Click(Sender: TObject);
    Procedure MenuItem4Click(Sender: TObject);
    Procedure MenuItem51Click(Sender: TObject);
    Procedure MenuItem52Click(Sender: TObject);
    Procedure MenuItem53Click(Sender: TObject);
    Procedure MenuItem54Click(Sender: TObject);
    Procedure MenuItem5Click(Sender: TObject);
    Procedure MenuItem6Click(Sender: TObject);
    Procedure MenuItem8Click(Sender: TObject);
    Procedure MenuItem9Click(Sender: TObject);
    Procedure ScrollBar1Change(Sender: TObject);
    Procedure ScrollBar2Change(Sender: TObject);
  private
    fProject: TProject;
    fFiles: TProjectFilesInfo;
    fdefcaption: String;
    mMovePos, mDownPos: TPoint;
    fShowRectangle: Boolean;

    Procedure ShowStartupInfo;
    Procedure HideStartupInfo;
    Procedure UpdateFileDependencies();

    Procedure GraphboxMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    Procedure GraphboxMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    Procedure GraphboxMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    Procedure GraphboxPaint(Sender: TObject);
    Procedure GraphboxDblClick(Sender: TObject);
    Procedure GraphboxResize(Sender: TObject);
    Function GetSelectedFileList(): TProjectFilesInfo;

    Procedure CalculateProject();
  public
    Procedure LoadProject(aFilename: String);
    Procedure SaveProject(aFilename: String);
  End;

Var
  Form1: TForm1;

Implementation

{$R *.lfm}

Uses LCLIntf, LCLType, IniFiles, Math, LazFileUtils
  , uDOMXML
  , Unit2 // Projekt Einstellungen
  , unit3 // Klassen Ansicht
  , unit4 // Code Counter
  , unit5 // CC
  // , unit6 // CC / Line Übersicht diverser Dateien
  // , unit7 // Project editor
  , unit8 // Callgraph
  // , unit9 // Node detail Info
  // , unit10 // Select Node form
  , Unit11 // Chart Statistics
  // , Unit12 // AddSearchpath
  , unit13 // Code Window
  ;

Const
  Grid_step = 8;

  { TForm1 }

Procedure TForm1.FormCreate(Sender: TObject);
Var
  lp: String;
Begin
  IniPropStorage1.IniFileName := GetAppConfigFile(false);
  fdefcaption := 'FPC Understand ver. ' + Version + ' by Corpsman';
  caption := fdefcaption;
  fShowRectangle := false;
  GraphBox1 := TGraphBox.Create(self);
  GraphBox1.Name := 'GraphBox1';
  GraphBox1.Parent := Self;
  GraphBox1.Top := Scale96ToForm(8);
  GraphBox1.Left := Scale96ToForm(8);
  GraphBox1.Width := Width - Scale96ToForm(16) - ScrollBar2.Width;
  GraphBox1.Height := Height - Scale96ToForm(16) - ScrollBar1.Height;
  GraphBox1.Anchors := [akBottom, akLeft, akRight, akTop];
  GraphBox1.Autoselect := false;
  GraphBox1.OnMouseUp := @GraphboxMouseUp;
  GraphBox1.OnMouseDown := @GraphboxMouseDown;
  GraphBox1.OnMouseMove := @GraphboxMouseMove;
  GraphBox1.OnPaint := @GraphboxPaint;
  GraphBox1.OnDblClick := @GraphboxDblClick;
  GraphBox1.OnResize := @GraphboxResize;
  GraphBox1.PopupMenu := PopupMenu1;
  fProject := TProject.Create();
  lp := IniPropStorage1.ReadString('LastProject', '');
  MenuItem15.Checked := IniPropStorage1.ReadBoolean('Grid', true);
  // Overrule last Project with a project that is passed as param.
  If ParamCount >= 1 Then Begin
    If FileExists(ParamStr(1)) Then Begin
      lp := ParamStr(1);
    End;
  End;
  ShowStartupInfo;
  If FileExists(lp) And (lp <> '') Then Begin
    LoadProject(lp);
  End;
End;

Procedure TForm1.FormDropFiles(Sender: TObject; Const FileNames: Array Of String
  );
Begin
  LoadProject(FileNames[0]);
End;

Procedure TForm1.FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; Var Handled: Boolean);
Begin
  If ssCtrl In shift Then Begin
    ScrollBar1.Position := ScrollBar1.Position + ScrollDelta;
  End
  Else Begin
    ScrollBar2.Position := ScrollBar2.Position + ScrollDelta;
  End;
End;

Procedure TForm1.FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; Var Handled: Boolean);
Begin
  If ssCtrl In shift Then Begin
    ScrollBar1.Position := ScrollBar1.Position - ScrollDelta;
  End
  Else Begin
    ScrollBar2.Position := ScrollBar2.Position - ScrollDelta;
  End;
End;

Procedure TForm1.FormResize(Sender: TObject);
Begin
  If label1.Visible Then Begin
    ShowStartupInfo;
  End;
End;

Procedure TForm1.FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
Begin
  IniPropStorage1.WriteBoolean('Grid', MenuItem15.Checked);
  If fProject.Changed Then Begin
    If id_no = Application.MessageBox('Close without save, you will loose configurations. Close anyway?', 'Warning', MB_ICONQUESTION Or MB_YESNO) Then Begin
      CanClose := false;
      exit;
    End;
  End;
  If CanClose Then Begin
    fProject.Free;
  End;
End;

Procedure TForm1.Button1Click(Sender: TObject);
Begin
  // Click to Load *.lpi
  form2.LoadProjectToLCL(fProject);
  If form2.SelectLPIAndShow Then Begin
    If form2.ModalResult = mrOK Then Begin
      form2.GetProjectFromLCL(fProject);
      caption := fdefcaption + ': ' + fProject.Name;
      CalculateProject();
    End;
    HideStartupInfo;
  End;
End;

Procedure TForm1.Button2Click(Sender: TObject);
Begin
  // Load a FPU-Project
  MenuItem3Click(Nil);
End;

Procedure TForm1.MenuItem10Click(Sender: TObject);
Const
  aborder = 15;
Var
  n: integer;
  s, c: Double;
  w, h, i: integer;
  node: TNode;
Begin
  // Order All Nodes as Circle
  ScrollBar1.Position := 0;
  ScrollBar2.Position := 0;
  n := GraphBox1.Graph.NodeCount;
  w := GraphBox1.Width Div 2;
  h := GraphBox1.Height Div 2;
  For i := 0 To n - 1 Do Begin
    SinCos(2 * pi * i / n, s, c);
    node := GraphBox1.Graph.Node[i];
    node.Position.x := round(w + (w - aborder) * s);
    node.Position.Y := round(h + (h - aborder) * c);
    If MenuItem15.Checked Then Begin
      node.Position.x := node.Position.x - node.Position.x Mod Grid_step;
      node.Position.Y := node.Position.Y - node.Position.Y Mod Grid_step;
    End;
    GraphBox1.Graph.Node[i] := node;
  End;
  GraphboxResize(Nil);
  GraphBox1.Invalidate;
End;

Procedure TForm1.MenuItem11Click(Sender: TObject);
Var
  n: TNode;
  i: Integer;
Begin
  // Hide Externals = Alle die die nicht in fFiles sind !
  For i := 0 To GraphBox1.Graph.NodeCount - 1 Do Begin
    n := GraphBox1.Graph.Node[i];
    n.Visible := (NameInList(fFiles, n.Name) <> -1);
    If Not n.Visible Then Begin // Wir schalten nur ab, nicht an !
      GraphBox1.Graph.Node[i] := n;
    End;
  End;
  GraphBox1.Invalidate;
End;

Procedure TForm1.MenuItem12Click(Sender: TObject);
Begin
  // Show all
  GraphBox1.Graph.ShowAllNodes();
  GraphBox1.Invalidate;
End;

Procedure TForm1.MenuItem13Click(Sender: TObject);
Begin
  // Show Statistiks
  If form4.CountFiles(GetSelectedFileList(), fProject) Then Begin
    form4.ShowModal;
  End
  Else Begin
    showmessage('Error, nothing to load.');
  End;
End;

Procedure TForm1.MenuItem14Click(Sender: TObject);
Begin
  // Show Classes
  If form3.LoadClasses(GetSelectedFileList(), fProject) Then Begin
    form3.ShowModal;
  End
  Else Begin
    showmessage('Error, nothing to load.');
  End;
End;

Procedure TForm1.MenuItem15Click(Sender: TObject);
Var
  i: integer;
  n: TNode;
Begin
  // Enable Disable "Grid"
  If MenuItem15.Checked Then Begin
    For i := 0 To GraphBox1.Graph.NodeCount - 1 Do Begin
      n := GraphBox1.Graph.Node[i];
      n.Position.x := n.Position.x - n.Position.x Mod Grid_step;
      n.Position.Y := n.Position.Y - n.Position.Y Mod Grid_step;
      GraphBox1.Graph.Node[i] := n;
    End;
    fProject.Change();
    GraphBox1.Invalidate;
  End;
End;

Procedure TForm1.MenuItem16Click(Sender: TObject);
Begin
  // Reload Dependencies
  CalculateProject();
End;

Procedure TForm1.MenuItem17Click(Sender: TObject);
Begin
  // Mark Cycles
  GraphBox1.Graph.UndoMarkings;
  GraphBox1.Graph.MarkCycles();
  GraphBox1.Invalidate;
End;

Procedure TForm1.MenuItem18Click(Sender: TObject);
Begin
  // Clear Marks
  GraphBox1.Graph.UndoMarkings;
  GraphBox1.Invalidate;
End;

Procedure TForm1.MenuItem19Click(Sender: TObject);
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

Procedure TForm1.MenuItem20Click(Sender: TObject);
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

Procedure TForm1.MenuItem21Click(Sender: TObject);
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

Procedure TForm1.MenuItem22Click(Sender: TObject);
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

Procedure TForm1.MenuItem23Click(Sender: TObject);
Begin
  // MarkNodesWithNoParents
  GraphBox1.Graph.UndoMarkings;
  GraphBox1.Graph.MarkNodesWithNoParents();
  GraphBox1.Invalidate;
End;

Procedure TForm1.MenuItem24Click(Sender: TObject);
Begin
  // MarkNodesWithNoChildrens
  GraphBox1.Graph.UndoMarkings;
  GraphBox1.Graph.MarkNodesWithNoChildrens();
  GraphBox1.Invalidate;
End;

Procedure TForm1.MenuItem25Click(Sender: TObject);
Begin
  // Show CC
  If form5.LoadFunctions(GetSelectedFileList(), fProject) Then Begin
    form5.ShowModal;
  End
  Else Begin
    showmessage('Error, nothing to load.');
  End;
End;

Procedure TForm1.MenuItem44Click(Sender: TObject);
Begin
  // Show Callgraph
  form8.CreateCallerList(GetSelectedFileList(), fProject.FileName);
  form8.ShowModal;
End;

Procedure TForm1.MenuItem46Click(Sender: TObject);
Begin
  // Show Chart Statistics
  form11.InitCharts(GetSelectedFileList(), fProject);
  form11.ShowModal;
End;

Procedure TForm1.MenuItem48Click(Sender: TObject);

  Function IndexInLevel(ALvlNode: TLvlGraphNode; Out ACount: Integer): Integer;
  Var
    l: Integer;
  Begin
    Result := 0;
    ACount := 0;
    For l := 0 To ALvlNode.Level.Count - 1 Do Begin
      If Not ALvlNode.Level.Nodes[l].Visible Then
        Continue;
      Inc(ACount);
      If l < ALvlNode.IndexInLevel Then
        Inc(Result);
    End;
  End;

Var
  i, j, LvlIdx, LvlCnt, cnt, xCnt, yCnt, h, w: Integer;
  f: Double;
  LvlGrph: TLvlGraph;
  LvlNode: TLvlGraphNode;
  node: TNode;
Begin
  // Order All Nodes with LvlGraph
  GraphBox1.Offset := Point(0, 0);
  ScrollBar1.Position := 0;
  ScrollBar2.Position := 0;
  cnt := GraphBox1.Graph.NodeCount;
  xCnt := GraphBox1.Width Div (DefaultDistanceX);
  yCnt := GraphBox1.Height Div (DefaultDistanceY);
  f := cnt / (xCnt * yCnt) * 1.5;
  xCnt := round(xCnt * f);

  LvlGrph := TLvlGraph.create;
  // TODO: This will fail if 2 nodes have the same caption
  For i := 0 To GraphBox1.Graph.NodeCount - 1 Do Begin
    If GraphBox1.Graph.Node[i].Visible Then Begin
      LvlGrph.GetNode(GraphBox1.Graph.Node[i].Caption, True);
    End;
  End;
  For i := 0 To GraphBox1.Graph.NodeCount - 1 Do Begin
    For j := 0 To high(GraphBox1.Graph.Node[i].Edges) Do Begin
      If GraphBox1.Graph.Node[GraphBox1.Graph.Node[i].Edges[j].StartIndex].Visible And
        GraphBox1.Graph.Node[GraphBox1.Graph.Node[i].Edges[j].EndIndex].Visible Then Begin
        LvlGrph.GetEdge(
          GraphBox1.Graph.Node[GraphBox1.Graph.Node[i].Edges[j].StartIndex].Caption,
          GraphBox1.Graph.Node[GraphBox1.Graph.Node[i].Edges[j].EndIndex].Caption,
          True
          );
      End;
    End;
  End;

  LvlGrph.FindIndependentGraphs;
  LvlGrph.CreateTopologicalLevels(False, True);
  LvlGrph.MarkBackEdges;
  LvlGrph.MinimizeEdgeLens(False);
  LvlGrph.LimitLevelHeights(10, 0);
  // lgesSeparate slower with bigger unit counts
  LvlGrph.SplitLongEdges(lgesSeparate);
  //LvlGrph.SplitLongEdges(lgesMergeTarget);
  LvlGrph.MinimizeCrossings;

  h := GraphBox1.Height Div (LvlGrph.LevelCount);
  For i := 0 To GraphBox1.Graph.NodeCount - 1 Do Begin
    node := GraphBox1.Graph.Node[i];
    If Not node.Visible Then Continue;
    LvlNode := LvlGrph.GetNode(node.Caption, False);
    If LvlNode = Nil Then Continue;
    LvlIdx := IndexInLevel(LvlNode, LvlCnt);
    w := GraphBox1.Width Div (LvlCnt);
    node.Position.x := (w Div 2) + LvlIdx * w;
    node.Position.y := (h Div 2) + LvlNode.Level.Index * h;
    If MenuItem15.Checked Then Begin // Align on grid if needed
      node.Position.x := node.Position.x - node.Position.x Mod Grid_step;
      node.Position.Y := node.Position.Y - node.Position.Y Mod Grid_step;
    End;
    GraphBox1.Graph.Node[i] := node;
  End;
  LvlGrph.Free;
  fProject.Change();
  GraphBox1.Invalidate;
End;

Procedure TForm1.MenuItem27Click(Sender: TObject);
Var
  i: Integer;
  n: TNode;
Begin
  // Hide Selected
  For i := 0 To GraphBox1.Graph.NodeCount - 1 Do Begin
    If GraphBox1.Graph.Node[i].Selected Then Begin
      n := GraphBox1.Graph.Node[i];
      n.Visible := false;
      GraphBox1.Graph.Node[i] := n;
    End;
  End;
  GraphBox1.Invalidate;
End;

Procedure TForm1.MenuItem28Click(Sender: TObject);
Var
  i: Integer;
  n: TNode;
Begin
  // Show Selected
  For i := 0 To GraphBox1.Graph.NodeCount - 1 Do Begin
    If GraphBox1.Graph.Node[i].Selected Then Begin
      n := GraphBox1.Graph.Node[i];
      n.Visible := true;
      GraphBox1.Graph.Node[i] := n;
    End;
  End;
  GraphBox1.Invalidate;
End;

Procedure TForm1.MenuItem2Click(Sender: TObject);
Begin
  // New Project
  fProject.Free;
  fProject := TProject.Create();
  GraphBox1.Graph.Clear;
  GraphBox1.Offset := Point(0, 0);
  ScrollBar1.Position := 0;
  ScrollBar2.Position := 0;
  GraphBox1.Invalidate;
  caption := fdefcaption;
  IniPropStorage1.WriteString('LastProject', '');
  ShowStartupInfo;
End;

Procedure TForm1.MenuItem31Click(Sender: TObject);
Var
  i: integer;
  aList: TProjectFilesInfo;
  fn: String;
  sp: TPathList;
  AtLeastOneSelected: Boolean;
Begin
  // Open Selected Node Folder
  aList := GetSelectedFileList();
  If aList = fFiles Then Begin
    // Entweder es wurde nichts gewählt, oder es wurde eine Datei gewählt die nicht teil des Projektes ist.
    sp := RelativePathListToAbsolutePathList(fProject.RootFolder, fProject.SearchPaths);
    AtLeastOneSelected := false;
    For i := 0 To GraphBox1.Graph.NodeCount - 1 Do Begin
      If GraphBox1.Graph.Node[i].Selected Then Begin
        AtLeastOneSelected := true;
        fn := SearchFileInSearchPaths(sp, GraphBox1.Graph.Node[i].Caption);
        If fn <> '' Then Begin
          OpenURL(ExtractFilePath(fn));
        End
        Else Begin
          showmessage('Error, unable to locate: ' + GraphBox1.Graph.Node[i].Caption);
        End;
      End;
    End;
    If Not AtLeastOneSelected Then Begin
      showmessage('Error, nothing selected.');
    End;
  End
  Else Begin
    For i := 0 To high(aList) Do Begin
      fn := aList[i].Filename;
      fn := ConcatRelativePath(fProject.RootFolder, fn);
      OpenURL(ExtractFilePath(fn));
    End;
  End;
End;

Procedure TForm1.MenuItem32Click(Sender: TObject);
Var
  aList: TProjectFilesInfo;
  sp: TPathList;
  AtLeastOneSelected: Boolean;
  i: Integer;
  fn: String;
  LPIFile: TDOMXML;
  NeedStore: Boolean;
  lpiFilename: String;
Begin
  // Add Selected node to lpi file
  lpiFilename := IncludeTrailingPathDelimiter(fProject.RootFolder) + fProject.LPISource;
  LPIFile := TDOMXML.Create;
  If Not LPIFile.LoadFromFile(lpiFilename) Then Begin
    LPIFile.free;
    showmessage('Error, no valid .lpi file.');
    exit;
  End;
  aList := GetSelectedFileList();
  If aList = fFiles Then Begin
    // Entweder es wurde nichts gewählt, oder es wurde eine Datei gewählt die nicht teil des Projektes ist.
    sp := RelativePathListToAbsolutePathList(fProject.RootFolder, fProject.SearchPaths);
    AtLeastOneSelected := false;
    NeedStore := true;
    For i := 0 To GraphBox1.Graph.NodeCount - 1 Do Begin
      If GraphBox1.Graph.Node[i].Selected Then Begin
        AtLeastOneSelected := true;
        fn := SearchFileInSearchPaths(sp, GraphBox1.Graph.Node[i].Caption);
        If fn = '' Then Begin
          showmessage('Error, unable to find: ' + GraphBox1.Graph.Node[i].Caption + LineEnding +
            'Please select by hand.');
          If OpenDialog2.Execute Then Begin
            fn := OpenDialog2.FileName;
          End;
        End;
        If fn <> '' Then Begin
          If AddFileToLPIFile(fProject.RootFolder, LPIFile, fn) = -1 Then Begin
            NeedStore := false; // Ein Fehler wir speichern nix !
          End;
        End
        Else Begin
          showmessage(GraphBox1.Graph.Node[i].Caption + ' will not be added.');
        End;
      End;
    End;
    If Not AtLeastOneSelected Then Begin
      NeedStore := false;
      showmessage('Error, nothing selected.');
    End;
  End
  Else Begin
    NeedStore := true;
    For i := 0 To high(aList) Do Begin
      fn := aList[i].Filename;
      fn := ConcatRelativePath(fProject.RootFolder, fn);
      If AddFileToLPIFile(fProject.RootFolder, LPIFile, fn) = -1 Then Begin
        NeedStore := false; // Ein Fehler wir speichern nix !
      End;
    End;
  End;
  If NeedStore Then Begin
    LPIFile.Indent := '  ';
    Try
      LPIFile.SaveToFile(lpiFilename);
    Except
      ShowMessage('Error, unable to store: ' + fProject.LPISource);
    End;
    CalculateProject; // Wir haben im Projekt was geänder, dann muss die Graphbox auch aktualisiert werden.
  End;
  LPIFile.free;
End;

Procedure TForm1.MenuItem3Click(Sender: TObject);
Begin
  // Load Project
  If OpenDialog1.Execute Then Begin
    LoadProject(OpenDialog1.FileName);
  End;
End;

Procedure TForm1.MenuItem42Click(Sender: TObject);
Var
  lpiFilename, fn: String;
  LPIFile: TDOMXML;
  NeedStore: Boolean;
  aList: TProjectFilesInfo;
  i: Integer;
Begin
  // Remove Selected Node from .lpi File
  aList := GetSelectedFileList();
  If aList = fFiles Then Begin
    showmessage('Nothing valid selected.');
  End;
  lpiFilename := IncludeTrailingPathDelimiter(fProject.RootFolder) + fProject.LPISource;
  LPIFile := TDOMXML.Create;
  If Not LPIFile.LoadFromFile(lpiFilename) Then Begin
    LPIFile.free;
    showmessage('Error, no valid .lpi file.');
    exit;
  End;
  NeedStore := true;
  For i := 0 To high(aList) Do Begin
    fn := aList[i].Filename;
    fn := ConcatRelativePath(fProject.RootFolder, fn);
    If RemoveFileFromLPIFile(fProject.RootFolder, LPIFile, fn) = rrError Then Begin
      NeedStore := false; // Ein Fehler wir speichern nix !
    End;
  End;
  If NeedStore Then Begin
    LPIFile.Indent := '  ';
    Try
      LPIFile.SaveToFile(lpiFilename);
    Except
      ShowMessage('Error, unable to store: ' + fProject.LPISource);
    End;
    CalculateProject; // Wir haben im Projekt was geänder, dann muss die Graphbox auch aktualisiert werden.
  End;
  LPIFile.free;
End;

Procedure TForm1.MenuItem43Click(Sender: TObject);
Var
  p: TPoint;
  b: TBitmap;
  png: TPortableNetworkGraphic;
Begin
  // Export as image
  If SaveDialog2.Execute Then Begin
    p := GraphBox1.Graph.GetMaxDimension;
    b := TBitmap.Create;
    b.width := p.X + 10;
    b.Height := p.y + 10;
    GraphBox1.Graph.PaintTo(b.canvas, 0, 0, rect(0, 0, p.X + 10, p.y + 10));
    png := TPortableNetworkGraphic.Create;
    png.Assign(b);
    b.free;
    png.SaveToFile(SaveDialog2.FileName);
    png.free;
  End;
End;

Procedure TForm1.MenuItem4Click(Sender: TObject);
Begin
  // Save Project
  If fProject.Filename <> '' Then Begin
    SaveProject(fProject.Filename);
  End
  Else Begin
    MenuItem5Click(Nil);
  End;
End;

Procedure TForm1.MenuItem51Click(Sender: TObject);
Begin
  // New Project with .lpi File
  MenuItem2Click(Nil);
  Button1Click(Nil);
End;

Procedure TForm1.MenuItem52Click(Sender: TObject);
Var
  list: TProjectFilesInfo;
Begin
  // Open Code
  list := GetSelectedFileList();
  If length(list) > 0 Then Begin
    form13.OpenFile(fProject, list[0].Filename);
  End;
End;

Procedure TForm1.MenuItem53Click(Sender: TObject);
Var
  c: TLineComments;
Begin
  c := fProject.GetLineComments();
  If high(c) = -1 Then Begin
    showmessage('Nothing to export.');
    exit;
  End;
  If SaveDialog3.Execute Then Begin
    ExportCommentsAsCSV(c, SaveDialog3.FileName);
  End;
End;

Procedure TForm1.MenuItem54Click(Sender: TObject);
Var
  c: TLineComments;
  i, j: Integer;
  n: TNode;
  nam: String;
Begin
  // Mark Files with comments
  c := fProject.GetLineComments();
  For i := 0 To high(c) Do Begin
    nam := ExtractFileNameOnly(c[i].Filename);
    nam := ExtractFileNameWithoutExt(nam);
    For j := 0 To GraphBox1.Graph.NodeCount - 1 Do Begin
      n := GraphBox1.Graph.Node[j];
      If GraphBox1.Graph.Node[j].Name = nam Then Begin
        n := GraphBox1.Graph.Node[j];
        n.Marked := true;
        GraphBox1.Graph.Node[j] := n;
        GraphBox1.Invalidate;
      End;
    End;
  End;
End;

Procedure TForm1.MenuItem5Click(Sender: TObject);
Begin
  // Save Project As
  If SaveDialog1.Execute Then Begin
    SaveProject(SaveDialog1.Filename);
  End;
End;

Procedure TForm1.MenuItem6Click(Sender: TObject);
Begin
  Close;
End;

Procedure TForm1.MenuItem8Click(Sender: TObject);
Begin
  form2.LoadProjectToLCL(fProject);
  If form2.ShowModal = mrOK Then Begin
    form2.GetProjectFromLCL(fProject);
    caption := fdefcaption + ': ' + fProject.Name;
    CalculateProject();
  End;
End;

Procedure TForm1.MenuItem9Click(Sender: TObject);
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
      If MenuItem15.Checked Then Begin
        node.Position.x := node.Position.x - node.Position.x Mod Grid_step;
        node.Position.Y := node.Position.Y - node.Position.Y Mod Grid_step;
      End;
      GraphBox1.Graph.Node[i] := node;
    End;
  End;
  GraphBox1.Invalidate;
End;

Procedure TForm1.ScrollBar1Change(Sender: TObject);
Begin
  GraphBox1.Offset := point(ScrollBar1.Position, GraphBox1.Offset.Y);
End;

Procedure TForm1.ScrollBar2Change(Sender: TObject);
Begin
  GraphBox1.Offset := point(GraphBox1.Offset.x, ScrollBar2.Position);
End;

Procedure TForm1.UpdateFileDependencies;
Var
  i, j, si, ei: Integer;
  n: TNode;
  BringToScreen: Boolean;
  //s: String;
Begin
  // 1. Alle bisherigen als "ungültig" markieren
  BringToScreen := GraphBox1.Graph.NodeCount = 0;
  GraphBox1.Graph.DelallEdges;
  GraphBox1.Graph.MarkAllNodesAsNotAdded;
  // 2. Alle Beziehungen eintragen
  For i := 0 To high(fFiles) Do Begin
    // TODO: Start here with Fixing, the known Bug!
    //si := GraphBox1.Graph.AddNode(ExtractRelativePath(fProject.RootFolder, fFiles[i].Filename), ExtractFileNameOnly(fFiles[i].filename)); // Die Datei ist teil des Projektes, also muss sie auch eine Node bekommen !
    si := GraphBox1.Graph.AddNode(ExtractFileNameOnly(fFiles[i].filename), ExtractFileNameOnly(fFiles[i].filename)); // Die Datei ist teil des Projektes, also muss sie auch eine Node bekommen !
    For j := 0 To high(fFiles[i].FileInfo.aUses) Do Begin
      ei := GraphBox1.Graph.AddNode(fFiles[i].FileInfo.aUses[j], fFiles[i].FileInfo.aUses[j]);
      GraphBox1.Graph.AddEdge(si, ei, clblack, Nil, true);
    End;
    For j := 0 To high(fFiles[i].FileInfo.aImplementationUses) Do Begin
      ei := GraphBox1.Graph.AddNode(fFiles[i].FileInfo.aImplementationUses[j], fFiles[i].FileInfo.aImplementationUses[j]);
      GraphBox1.Graph.AddEdge(si, ei, clred, Nil, true);
    End;
  End;
  // 3. Alle die nicht erneut hinzugefügt wurden fliegen wieder raus
  GraphBox1.Graph.DeleteAllNodesNotMarkedAsAdded;
  // Alle Knoten die zwar eingetragen sind, aber nicht in der FileList stehen werden nun Grau eingefärbt, damit kann man dann gleich "Fehlkonfigurationen" sehen..
  For i := 0 To GraphBox1.Graph.NodeCount - 1 Do Begin
    n := GraphBox1.Graph.Node[i];
    If NameInList(fFiles, GraphBox1.Graph.Node[i].Name) = -1 Then Begin
      n.Color := clSilver;
    End
    Else Begin
      n.Color := clWhite;
      n.Visible := true;
    End;
    GraphBox1.Graph.Node[i] := n;
  End;
  If BringToScreen Then Begin // Beim 1. Mal machen wir das gleich für den User ;)
    MenuItem9Click(Nil);
  End;
  GraphboxResize(Nil);
  GraphBox1.Invalidate;
End;

Procedure TForm1.ShowStartupInfo;
Begin
  label1.BringToFront;
  label2.BringToFront;

  Label1.top := height Div 2 - Scale96ToFont(88);
  Label1.left := (width - label1.Width) Div 2;

  Button2.left := (width - button2.Width) Div 2;
  Button2.top := Label1.Top + Label1.Height + Scale96ToForm(8);

  Label2.left := (width - label2.Width) Div 2;
  Label2.top := button2.Top + button2.Height + Scale96ToForm(8);

  Button1.left := (width - button1.Width) Div 2;
  Button1.top := label2.Top + label2.Height + Scale96ToForm(8);

  label1.visible := true;
  label2.visible := true;
  button1.Visible := true;
  button2.Visible := true;
End;

Procedure TForm1.HideStartupInfo;
Begin
  label1.visible := false;
  label2.visible := false;
  button1.Visible := false;
  button2.Visible := false;
End;

Procedure TForm1.GraphboxMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Var
  index: Integer;
Begin
  x := x + GraphBox1.Offset.X;
  y := y + GraphBox1.Offset.Y;
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

Procedure TForm1.GraphboxMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Begin
  If fShowRectangle Then Begin
    GraphBox1.Graph.SelectRectangle(mDownPos.x, mDownPos.y, mMovePos.X, mMovePos.y, GraphBox1.Canvas);
  End;
  fShowRectangle := false;
  GraphBox1.Invalidate;
End;

Procedure TForm1.GraphboxMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
Var
  n: TNode;
  i: integer;
  bool: Boolean;
  mxx, myy, dx, dy: Integer;
Begin
  x := x + GraphBox1.Offset.X;
  y := y + GraphBox1.Offset.Y;
  If ssleft In shift Then Begin
    bool := false;
    dx := mMovePos.x - x;
    dy := mMovePos.y - y;
    If MenuItem15.Checked Then Begin
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
        fProject.Change();
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

Procedure TForm1.GraphboxPaint(Sender: TObject);
Begin
  If fShowRectangle Then Begin
    GraphBox1.Canvas.Brush.Style := bsClear;
    GraphBox1.Canvas.Pen.Color := clRed;
    GraphBox1.Canvas.Rectangle(mDownPos.x - GraphBox1.Offset.X, mDownPos.y - GraphBox1.Offset.y, mMovePos.X - GraphBox1.Offset.X, mMovePos.y - GraphBox1.Offset.y);
  End;
End;

Procedure TForm1.GraphboxDblClick(Sender: TObject);
Begin
  // Popup zur Auswahl was gemacht werden soll ;)
  PopupMenu2.PopUp;
End;

Procedure TForm1.GraphboxResize(Sender: TObject);
Var
  p: TPoint;
Begin
  p := GraphBox1.Graph.GetMaxDimension;
  ScrollBar1.max := p.X;
  ScrollBar2.max := p.y;
End;

Function TForm1.GetSelectedFileList: TProjectFilesInfo;
Var
  i, index: Integer;
Begin
  result := Nil;
  (*
   * 2 Möglichkeiten.
   *)
   // 2. Es ist was angewählt, dann nur die
  For i := 0 To GraphBox1.Graph.NodeCount - 1 Do Begin
    If GraphBox1.Graph.Node[i].Selected Then Begin
      index := NameInList(fFiles, GraphBox1.Graph.Node[i].Name);
      If index <> -1 Then Begin
        setlength(result, high(result) + 2);
        result[high(result)] := fFiles[index];
      End;
    End;
  End;
  // 1. es ist nichts angewählt, dann alle
  If length(result) = 0 Then Begin
    result := fFiles;
  End;
End;

Procedure TForm1.CalculateProject;
Var
  FileList: TFileList;
  PathList: TPathList;
  i: Integer;
  FPCParser: TFPCParser;
  f: TProjectFileInfo;
  t: Int64;
  SearchPaths: TStringlist;
Begin
  t := GetTickCount64;
  fFiles := Nil;
  FileList := GetFileList(fProject.RootFolder, fProject.LPISource, RelativeFileListToAbsoluteFileList(fProject.RootFolder, fProject.Files));
  PathList := GetSearchPathList(fProject.RootFolder, fProject.LPISource, RelativePathListToAbsolutePathList(fProject.RootFolder, fProject.SearchPaths));
  // Store Searchpathlist relative
  SearchPaths := TStringList.Create;
  SearchPaths.Add(IncludeTrailingPathDelimiter(fProject.RootFolder));
  For i := 0 To high(PathList) Do Begin
    SearchPaths.Add(IncludeTrailingPathDelimiter(PathList[i].Path));
    PathList[i].Path := ExcludeTrailingPathDelimiter(ExtractRelativePath(fProject.RootFolder, PathList[i].Path));
  End;
  fProject.SearchPaths := PathList;
  FPCParser := TFPCParser.Create;
  For i := 0 To high(FileList) Do Begin
    If FileList[i].Enabled Then Begin
      f.Filename := ExtractRelativePath(IncludeTrailingPathDelimiter(fProject.RootFolder), FileList[i].FileName);
      If FPCParser.ParseFile(FileList[i].FileName, SearchPaths) Then Begin
        f.FileInfo := FPCParser.FileInfo;
        f.Methods := FPCParser.ProcInfos;
        setlength(fFiles, high(fFiles) + 2);
        fFiles[high(fFiles)] := f;
      End
      Else Begin
        showmessage('Error, unable to analyce: ' + f.Filename);
      End;
    End;
  End;
  SearchPaths.free;
  FPCParser.free;
  UpdateFileDependencies;
  MenuItem11Click(Nil); // Hide Externals via default
  t := GetTickCount64 - t;
  //t := t Div 1000;
  //If t > 2 Then Begin
  //ShowMessage(inttostr(t));
  //End;
  caption := fdefcaption + ': ' + fProject.Name + ', loaded in ' + inttostr(t) + 'ms';
End;

Procedure TForm1.LoadProject(aFilename: String);
Var
  ini: TIniFile;
  index, i: Integer;
  maxx, maxy, x, y: LongInt;
  n: String;
  node: TNode;
Begin
  If Not FileExists(aFilename) Then Begin
    showmessage('Error, could not find: ' + aFilename);
  End;
  HideStartupInfo;
  IniPropStorage1.WriteString('LastProject', aFilename);
  fProject.LoadFromFile(aFilename);
  // Laden der der Knoten Informationen
  ini := TIniFile.Create(aFilename);
  GraphBox1.Graph.Clear;
  maxx := 0;
  maxy := 0;
  For i := 0 To ini.ReadInteger('Nodes', 'Count', 0) - 1 Do Begin
    x := ini.ReadInteger('Nodes', 'Node' + inttostr(i) + 'X', 0);
    y := ini.ReadInteger('Nodes', 'Node' + inttostr(i) + 'Y', 0);
    n := ini.ReadString('Nodes', 'Node' + inttostr(i) + 'Name', '');
    // TODO: Add code here, that handles the new naming of nodes
    index := GraphBox1.Graph.AddNode(n, n);
    node := GraphBox1.Graph.Node[index];
    node.Position.X := x;
    node.Position.Y := y;
    GraphBox1.Graph.Node[index] := node;
    maxx := max(maxx, x);
    maxy := max(maxy, y);
  End;
  ini.free;
  caption := fdefcaption + ': ' + fProject.Name;
  GraphBox1.Offset := point(0, 0);
  ScrollBar1.Position := 0;
  ScrollBar2.Position := 0;
  ScrollBar1.Max := maxx;
  ScrollBar2.Max := maxy;
  CalculateProject;
End;

Procedure TForm1.SaveProject(aFilename: String);
Var
  ini: TIniFile;
  n: TNode;
  i: Integer;
Begin
  IniPropStorage1.WriteString('LastProject', aFilename);
  fProject.SaveToFile(aFilename);
  // Dazu Legen der
  ini := TIniFile.Create(aFilename);
  ini.CacheUpdates := true;
  ini.WriteInteger('Nodes', 'Count', GraphBox1.Graph.NodeCount);
  For i := 0 To GraphBox1.Graph.NodeCount - 1 Do Begin
    n := GraphBox1.Graph.Node[i];
    ini.WriteInteger('Nodes', 'Node' + inttostr(i) + 'X', n.Position.X);
    ini.WriteInteger('Nodes', 'Node' + inttostr(i) + 'Y', n.Position.Y);
    ini.WriteString('Nodes', 'Node' + inttostr(i) + 'Name', n.Name);
  End;
  ini.UpdateFile;
  ini.free;
End;

End.

