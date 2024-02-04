(******************************************************************************)
(* ugraphs.pas                                                     23.11.2022 *)
(*                                                                            *)
(* Version     : 0.03                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : All you need to work with Graphs                             *)
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
(* Known Issues: none                                                         *)
(*                                                                            *)
(* History     : 0.01 - Initial version merge from old ugraphs versions       *)
(*               0.02 - Node Color                                            *)
(*               0.03 - use Caption instead of Name for Dimension calculation *)
(*                                                                            *)
(******************************************************************************)
Unit ugraphs;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Controls, Graphics, FileUtil, math;

Const
  DefaultDistanceY = 40;
  DefaultDistanceX = 80;
  DefaultTextborderX = 5;
  DefaultTextborderY = 5;
  PfeilDx = 5;
  PfeilDy = 5;
  (*
   * Historie: 0..4 = ?
   *              5 = Hinzufügen Node.Color
   *)
  GraphsVersion: integer = 5; // Die Version in der die Dateien der Klasse gespeichert sind.

Type

  TEdge = Record
    StartIndex: Integer; // Index des Besitzenden Knotens
    EndIndex: Integer; // Index des Zielknoten
    EdgeColor: TColor; // Farbe der Kante auf den Knoten
    EdgeCaption: String; // Die Beschriftung der Kante
    Directed: Boolean; // Eine Gerichtete Kante = Mit einer Pfeilspitze
    UserData: Pointer; // Falls die Kante auch ihre Eigenen Userdatan hat ??
    Selected: Boolean; // Für gui Spass, dann wird das Node mit Rotem Rahmen Gerendert
    // Ab hier wirds ominös - bzw nicht gespeichert
    CyclicMark: Boolean; // Wird von MarkCyclicInclusions benötigt
  End;

  TNode = Record
    Name: String; // Der Primärschlüssel, darf nur 1 mal im Graphen vor kommen
    Caption: String; // Die Caption
    Level: Integer; // Level, für die Sortierung
    Position: TPoint; // Position des Knoten
    Edges: Array Of TEdge; // Liste aller Kanten auf die anderen Nodes
    Marked: Boolean; // Wenn True, dann wird der Knoten mit Grün gezeichnet
    Visible: Boolean; // Wenn True, dann ist der Knoten gerade Sichtbar
    UserData: Pointer; // Nutzdaten die an den Knoten geknüpft werden können, benötigen eine Speicher Callback
    Color: TColor; // Die Hintergrundfarbe des Knotens, default = clWhite
    //-- Ab hier kommen Teile die nicht Gespeichert werden und nur für die Internen Algorithmen genutzt werden
    Visited: Boolean; // Internen Flag, Muss nicht gespeichert werden !!
    Selected: Boolean; // Für gui Spass, dann wird das Node mit Rotem Rahmen Gerendert
    Added: Boolean; // s.u. Zum erkennen, ob in einem neuen Einlesevorgang das Modul hinzugefügt wurde.
  End;

  TGraph = Class;

  TDeleteUserDataEvent = Procedure(Sender: TObject; UserData: Pointer) Of Object;
  TSaveUserDataEvent = Procedure(Sender: TObject; Const Stream: TStream; Const UserData: Pointer) Of Object;
  TLoadUserDataEvent = Function(Sender: TObject; Const Stream: TStream): Pointer Of Object;

  (*
   * Result = True -> Der Aufrufer rendert das Node nicht mehr, weil es behandelt wurde
   *        = False -> Der Aufrufer rendert das Node als wäre nichts gewesen
   *)
  TDrawNodeEvent = Function(Sender: TGraph; Const aCanvas: TCanvas; Const NodeIndex: Integer): Boolean Of Object;
  TDrawEdgeEvent = Function(Sender: TGraph; Const aCanvas: TCanvas; Const EdgeIndex: Integer): Boolean Of Object;

  TNodePrepareCanvasEvent = Procedure(Sender: TGraph; Const aCanvas: TCanvas; Const NodeIndex: Integer) Of Object;

  { TGraph }

  TGraph = Class
  private
    FChanged: Boolean;
    Fnodes: Array Of TNode;
    FLevelLefts: Array Of Integer; // Merkt sich die "Linkeste" position der jeweiligen Einfügtiefe (wird nur beim Add gebraucht, so dass die Nodes nicht alle auf einander erzeugt werden).
    Function AddNode2(Name, Caption: String; Depth: Integer): Integer; // Fügt ein Node ins Array ein, und gibt dessen index zurück
    Function GetEdge(Index: integer): TEdge;
    Function GetNode(index: integer): TNode;
    Procedure NodeToStream(Const Stream: TStream; Const Node: TNode);
    Procedure EdgeToStream(Const Stream: TStream; Const Edge: TEdge);
    Function NodeFromStream(Const Stream: TStream; FileVersion: integer): TNode;
    Function EdgeFromStream(Const Stream: TStream; FileVersion: integer): TEdge;
    Procedure SetEdge(Index: integer; AValue: TEdge);
    Procedure SetNode(index: integer; AValue: TNode);
    Procedure UnVisit(); // Löscht das Visit Flag auf allen Knoten
  public

    OnLoadNodeUserData: TLoadUserDataEvent;
    OnSaveNodeUserData: TSaveUserDataEvent;
    OnDeleteNodeUserData: TDeleteUserDataEvent;

    OnLoadEdgeUserData: TLoadUserDataEvent;
    OnSaveEdgeUserData: TSaveUserDataEvent;
    OnDeleteEdgeUserData: TDeleteUserDataEvent;

    OnPrepareNodeCanvas: TNodePrepareCanvasEvent; // Wird als aller letztes for dem eigentlichen Zeichnen aufgerufen (das Canvas ist hier schon komplett fertig konfiguriert)
    OnDrawNode: TDrawNodeEvent; // Wird beim Rendern jedes Knotens aufgerufen mit bereits imitialisierten Canvas
    OnDrawEdge: TDrawEdgeEvent;
    OnDrawEdgeCaption: TDrawEdgeEvent;

    Property Changed: Boolean read FChanged; // True, wenn sich der Graph irgendwie geändert hat
    Property Node[index: integer]: TNode read GetNode write SetNode;
    Property Edge[Index: integer]: TEdge read GetEdge write SetEdge;

    Constructor create;
    Destructor destroy; override;

    Function NodeCount: Integer; // Anzahl der im Graphen befindlichen Knoten
    Function EdgeCount: Integer; // Anzahl aller Kanten im Graphen

    Procedure Clear; // Einfach alle Daten Löschen ( wie Destroy, nur das die Instanz eben erhalten bleibt)

    Procedure PaintTo(Const Canvas: TCanvas; OffX, OffY: Integer; R: TRect); // Zeichnet den Graphen auf ein Canvas


    // Fügt einen Knoten ein
    // TODO: Die unnützen Parameter Platt machen !
    Function AddNode(NodeName, NodeCaption: String; OverwriteIfExists: Boolean = true; Depth: integer = 0): integer; overload;
    Function AddNode(NodeName, NodeCaption: String; NodeUserData: Pointer; OverwriteIfExists: Boolean = true; Depth: integer = 0): integer; overload;
    // Fügt einen Knoten ein und legt gleichzeitig eine Kante von NodeName nach NodeClient an.
    //Function AddNode(NodeName, NodeCaption, NodeClient, NodeClientCaption: String; Depth: Integer; EdgeColor: Tcolor): integer; overload;
    //Function AddNode(NodeName, NodeCaption: String; NodeUserData: Pointer; NodeClient, NodeClientCaption: String; Depth: Integer; EdgeColor: TColor): integer; overload;

    Function FindNode(NodeName: String): Integer; // -1 = nicht gefunden, sonst Index

    Procedure DelNode(Index: Integer); // Löscht einen einzelnen Knoten und Alle seine Kanten (zu ihm und von ihm weg)
    Procedure DelEdge(StartNodeIndex, EndNodeIndex: Integer); // Löschen der Kante von, Nach


    (*
     * Will man eine Verzeichnissstruktur neu Einlesen, aber die Knoten Meta Informationen erhalten
     * dann, kann man dies in dem man alle Kanten Löscht und alle Dateien neu Einliest.
     * Dies "löscht" aber keine Existierenden Knoten, sollten diese im neuen Leseschritt nicht mehr
     * vorhanden sein.
     * Umgangen werden kann das mit den beiden Untenstehenden Routinen
     * Ablauf :
     *    1. MarkAllNodesAsNotAdded
     *    2. Beliebig viele AddNode...
     *    3. DeleteAllNodesNotMarkedAsAdded
     *)
    Procedure DeleteAllNodesNotMarkedAsAdded;
    Procedure MarkAllNodesAsNotAdded;

    Function GetMaxDimension: TPoint; // Gibt die Maximale Ausbreitung der Knoten aus, Damit man weis wie Groß das Bild Sein muss, auf dem alle Knoten angezeigt werden können.

    (*
     * Laden /speichern
     *)
    Procedure SaveToStream(Const Stream: TStream);
    Function LoadFromStream(Const Stream: TStream): Boolean;
    Procedure SaveToFile(Const Filename: String);
    Function LoadFromFile(Const Filename: String): Boolean;


    // Fügt eine Kante ein, wenn es die Knoten nicht gibt, werden sie via AddSingleNode angelegt
    Procedure AddEdge(StartNode, StartNodeCaption, EndNode, EndNodeCaption: String; EdgeColor: TColor; Userdata: Pointer = Nil; Directed: Boolean = false; EdgeLabel: String = ''); overload; Deprecated;
    Procedure AddEdge(StartNode, EndNode: Integer; EdgeColor: TColor; Userdata: Pointer = Nil; Directed: Boolean = false; EdgeCaption: String = ''); overload;
    Procedure DelallEdges(); // Löscht alle Kanten heraus

    Function GetNodeIndex(x, y: integer; Const Canvas: TCanvas): Integer; // Ermittelt ob sich an der x,y Koordinate ein Knoten befindet -1 bei Fehler
    Function GetNodeIndexByName(NodeName: String): integer; // -1 Node nicht gefunden
    Function GetNodeUserData(index: integer): Pointer; // Info Pointer des Knotens nur zum Lesen
    Procedure SetNodeUserData(Index: integer; aValue: Pointer);
    Function GetNodeName(index: Integer): String; // Name des Knoten
    Function GetNodeClientList(index: integer): TBoundArray; // Zurück gegeben werden die Indicees der Clients

    Function GetSelected(index: integer): Boolean; // True, wenn der Knoten Selektiert ist
    Procedure Select(index: integer); // Selectiert einen Knoten (Randfarbe wird Rot)
    Procedure SelectRectangle(x1, y1, x2, y2: integer; Const Canvas: TCanvas); // selektiert alle Knoten im Rechteck (x1/y1) .. (x2/y2) Fügt nur Selektierungen Hinzu, vorher vorhandene Selektierungen bleiben Bestehen
    Procedure DeSelect(index: integer); // Selektiert einen einzelnen Knoten
    Procedure DeSelectAll(); // Löscht die Selektierung aller Knoten

    Procedure DelAllSelectedNodes(); // Löscht alle Selektierten Knoten

    Function GetNodePosition(Index: Integer): TPoint; // Position des Knotens
    Procedure SetNodePosition(Index: integer; NewPosition: Tpoint);

    Function GetMarked(index: integer): Boolean; // True, wenn der Knoten Markiert ist
    Procedure MarkNode(Index: integer); // Markiert einen Knoten (Füllfarbe wird Grün)
    Procedure UnMarkNode(Index: integer); // Entfernt die Knoten Markierung
    Procedure DelMarkedNodes(); // Löscht alle Marikerten Knoten
    Procedure MarkFistGenChilds(Index: Integer); // Markiert alle direkten Kind Knoten
    Procedure MarkAllChilds(Index: integer); // Markiert alle Kind Knoten
    Procedure MarkFistGenParents(Index: integer); // Markiert alle direkten Eltern Knoten
    Procedure MarkAllParents(Index: integer); // Markiert alle Eltern Knoten
    Procedure MarkPathsBetweenNodes(StartNode, EndNode: integer); // Markiert alle Knoten auf dem Weg von Startnode zu endnode (wenn es einen gibt).
    Procedure MarkNodesWithMultipleParents(); // Markiert alle Knoten auf die mehr als eine Kante Zeigt.
    Procedure MarkNodesWithNoParents(); // Markiert alle Knoten auf die keine Kante Zeigt.
    Procedure MarkNodesWithNoChildrens(); // Markiert alle Knoten auf die keine Kind Knoten haben
    Procedure UndoMarkings(); // Nimmt alle Markierungen weg
    (*
     * Es werden zwar Alle Zyklen gefunden, jedoch auch alle mit der Selben Farbe angezeigt, hat man also einen Graphen
     * mit sehr vielen evtl. sich überlappenden Zyklen, kann man nichts bis nicht mehr viel erkennen.
     *)
    Procedure MarkCycles();

    Procedure HideNode(index: integer); // Blendet einen Knoten Sammt Kanten beim Zeichnen aus
    Procedure ShowNode(index: integer); // Blendet einen Knoten Sammt Kanten beim Zeichnen aus
    Function IsNodeVisible(Index: integer): Boolean; // True, wenn der Knoten Sichtbar ist
    Procedure ShowAllNodes(); // Zeigt wieder alle Knoten an
  End;

  (*
   * Eine LCL Componente für TGraph
   *)
  { TgraphBox }

  TGraphBox = Class(TGraphicControl)
  protected
    Procedure Paint; override;
    Procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    Procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    Procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    Procedure Resize; override;
  private
    fAutoselect: Boolean;
    fSelected: integer;
    fGraph: TGraph;
    Procedure SetAutoselect(AValue: Boolean);
  public
    Property Autoselect: Boolean read fAutoselect write SetAutoselect; // Wenn True, dann wird im OnMouseDown / Up Ereigniss Mit Selected gearbeitet.
    //ScaleOnResize: Boolean; // Wenn True, dann werden beim Resize alle relative nodes mit verschoben

    Property Graph: TGraph read fGraph;

    Property OnPaint;
    Property OnResize;
    Property OnMouseDown;
    Property OnMouseUp;
    Property OnMouseMove;
    Property OnDblClick;

    // Gibt den Aktuell Selektierten Knoten zurück, wenn keiner Selektiert ist, dann -1
    Property SelectedNode: Integer read fSelected;

    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;
  End;

Implementation

Uses uvectormath;

Function InInterval(Value, Lower, Upper: integer): Boolean;
Begin
  result := (value >= min(lower, upper)) And (value <= max(lower, upper));
End;

{ TGraph }

Constructor TGraph.create;
Begin
  Inherited create;

  FChanged := false;
  setlength(Fnodes, 0);
  SetLength(FLevelLefts, 0);

  OnLoadNodeUserData := Nil;
  OnSaveNodeUserData := Nil;
  OnDeleteNodeUserData := Nil;

  OnLoadEdgeUserData := Nil;
  OnSaveEdgeUserData := Nil;
  OnDeleteEdgeUserData := Nil;

  OnPrepareNodeCanvas := Nil;
  OnDrawNode := Nil;
  OnDrawEdge := Nil;
  OnDrawEdgeCaption := Nil;
End;

Destructor TGraph.destroy;
Begin
  clear;
End;

Procedure TGraph.Clear;
Var
  i: Integer;
Begin
  For i := 0 To high(Fnodes) Do Begin
    setlength(Fnodes[i].Edges, 0);
    If assigned(Fnodes[i].UserData) Then Begin
      If Not assigned(OnDeleteNodeUserData) Then
        Raise Exception.Create('Error ' + ClassName + '.OnDeleteNodeUserData not set.');
      OnDeleteNodeUserData(self, Fnodes[i].UserData);
    End;
  End;
  setlength(Fnodes, 0);
  SetLength(FLevelLefts, 0);
  FChanged := false;
End;

Function TGraph.AddNode2(Name, Caption: String; Depth: Integer): Integer;
Var
  i: Integer;
Begin
  // Suchen obs das ding schon gibt
  For i := 0 To High(Fnodes) Do Begin
    If lowercase(FNodes[i].Name) = Lowercase(name) Then Begin
      Fnodes[i].Added := true;
      result := i;
      exit;
    End;
  End;
  // Einfügen
  FChanged := True;
  setlength(Fnodes, high(Fnodes) + 2);
  Fnodes[high(Fnodes)].Name := name;
  Fnodes[high(Fnodes)].Caption := Caption;
  Fnodes[high(Fnodes)].Level := Depth;
  Fnodes[high(Fnodes)].Marked := false;
  Fnodes[high(Fnodes)].Visited := false;
  Fnodes[high(Fnodes)].UserData := Nil;
  Fnodes[high(Fnodes)].Visible := true;
  Fnodes[high(Fnodes)].Color := clWhite;
  Fnodes[high(Fnodes)].Selected := false;
  Fnodes[high(Fnodes)].Added := true;
  setlength(Fnodes[high(Fnodes)].Edges, 0);
  If Depth <= High(FLevelLefts) Then Begin
    FLevelLefts[depth] := FLevelLefts[depth] + 1;
    Fnodes[high(Fnodes)].Position := point((FLevelLefts[depth] + 1) * defaultdistanceX, (Depth + 1) * DefaultDistanceY);
  End
  Else Begin
    // Müsste immer + 1 sein
    setlength(FLevelLefts, Depth + 1);
    FLevelLefts[depth] := 0;
    Fnodes[high(Fnodes)].Position := point((FLevelLefts[depth] + 1) * defaultdistanceX, (Depth + 1) * DefaultDistanceY);
  End;
  result := high(Fnodes);
End;

Function TGraph.GetEdge(Index: integer): TEdge;
Var
  i: Integer;
Begin
  If index = -1 Then Begin
    Raise Exception.Create('Error, invalid index.');
  End;
  For i := 0 To high(Fnodes) Do Begin
    If Index < length(Fnodes[i].Edges) Then Begin
      result := Fnodes[i].Edges[Index];
      exit;
    End
    Else Begin
      index := index - length(Fnodes[i].Edges);
    End;
  End;
  Raise Exception.Create('Error, invalid index.');
End;

Function TGraph.GetNode(index: integer): TNode;
Begin
  result := Fnodes[index];
End;

Procedure TGraph.EdgeToStream(Const Stream: TStream; Const Edge: TEdge);
Var
  i: integer;
Begin
  stream.Write(Edge.EndIndex, sizeof(Edge.EndIndex));
  stream.Write(Edge.EdgeColor, sizeof(Edge.EdgeColor));
  stream.WriteAnsiString(Edge.EdgeCaption);
  stream.Write(Edge.Directed, sizeof(Edge.Directed));
  stream.Write(Edge.Selected, sizeof(Edge.Selected));
  If assigned(Edge.UserData) Then Begin
    If Not assigned(OnSaveEdgeUserData) Then
      Raise Exception.Create('Error ' + ClassName + '.OnSaveEdgeUserData not set.');
    i := 1;
    stream.Write(i, sizeof(i));
    OnSaveEdgeUserData(self, Stream, Edge.UserData);
  End
  Else Begin
    i := 0;
    stream.Write(i, sizeof(i));
  End;
End;

Procedure TGraph.NodeToStream(Const Stream: TStream; Const Node: TNode);
Var
  i: integer;
Begin
  stream.WriteAnsiString(node.Name);
  stream.WriteAnsiString(node.Caption);
  stream.Write(node.Level, sizeof(node.Level));
  stream.Write(node.Position, sizeof(node.Position));
  stream.Write(node.Marked, sizeof(node.Marked));
  stream.Write(node.Visible, sizeof(node.Visible));
  stream.Write(node.Color, sizeof(node.Color));
  If assigned(node.UserData) Then Begin
    If Not Assigned(OnSaveNodeUserData) Then
      Raise Exception.Create('Error ' + ClassName + '.OnSaveNodeUserData not set.');
    i := 1;
    stream.Write(i, sizeof(i));
    OnSaveNodeUserData(self, stream, node.UserData);
  End
  Else Begin
    i := 0;
    stream.Write(i, sizeof(i));
  End;
  i := high(node.Edges) + 1;
  stream.Write(i, sizeof(i));
  For i := 0 To high(Node.Edges) Do Begin
    EdgeToStream(Stream, node.Edges[i]);
  End;
End;

Function TGraph.NodeFromStream(Const Stream: TStream; FileVersion: integer
  ): TNode;
Var
  i: integer;
Begin
  result.Name := stream.ReadAnsiString;
  result.Caption := stream.ReadAnsiString;
  stream.read(result.Level, sizeof(result.Level));
  stream.read(result.Position, sizeof(result.Position));
  stream.read(result.Marked, sizeof(result.Marked));
  stream.read(result.Visible, sizeof(result.Visible));
  If FileVersion > 4 Then Begin
    stream.read(result.Color, sizeof(result.Color));
  End
  Else Begin
    result.Color := clWhite;
  End;
  result.Selected := false;
  result.Visited := false;
  result.Added := false;
  i := 0;
  stream.read(i, sizeof(i));
  If i = 1 Then Begin
    If Not Assigned(OnLoadNodeUserData) Then
      Raise Exception.Create('Error ' + ClassName + '.OnLoadNodeUserData not set.');
    result.UserData := OnLoadNodeUserData(self, stream);
  End
  Else Begin
    result.UserData := Nil;
  End;
  i := 0;
  stream.read(i, sizeof(i));
  setlength(result.Edges, i);
  For i := 0 To high(result.Edges) Do Begin
    result.Edges[i] := EdgeFromStream(Stream, FileVersion);
  End;
End;

Function TGraph.EdgeFromStream(Const Stream: TStream; FileVersion: integer
  ): TEdge;
Var
  i: integer;
Begin
  result.EndIndex := -1;
  result.EdgeColor := clblack;
  result.StartIndex := -1; // Muss vom Aufrufer definiert werden
  stream.read(result.EndIndex, sizeof(result.EndIndex));
  stream.read(result.EdgeColor, sizeof(result.EdgeColor));
  result.EdgeCaption := stream.ReadAnsiString();
  stream.Read(result.Directed, sizeof(result.Directed));
  stream.Read(result.Selected, sizeof(result.Selected));
  i := 0;
  stream.read(i, sizeof(i));
  If i = 1 Then Begin
    If Not Assigned(OnLoadEdgeUserData) Then
      Raise Exception.Create('Error ' + ClassName + '.OnLoadEdgeUserData not set.');
    result.UserData := OnLoadEdgeUserData(self, stream);
  End
  Else Begin
    result.UserData := Nil;
  End;
End;

Procedure TGraph.SetEdge(Index: integer; AValue: TEdge);
Var
  i: Integer;
Begin
  If index = -1 Then Begin
    Raise Exception.Create('Error, invalid index.');
  End;
  For i := 0 To high(Fnodes) Do Begin
    If Index < length(Fnodes[i].Edges) Then Begin
      // TODO: ggf sauber Frei geben der Userdaten !
      Fnodes[i].Edges[Index] := AValue;
      exit;
    End
    Else Begin
      index := index - length(Fnodes[i].Edges);
    End;
  End;
  Raise Exception.Create('Error, invalid index.');
End;

Procedure TGraph.SetNode(index: integer; AValue: TNode);
Begin
  Fnodes[index] := AValue;
End;

Procedure TGraph.PaintTo(Const Canvas: TCanvas; OffX, OffY: Integer; R: TRect);
Var
  x, y, i, j, k: Integer;
  a: Array Of TPoint;
  s: String;
  ei, w, h: Integer;
  drawIt: Boolean;
Begin
  // TODO: das so umschreiben dass das auch Mehrzeilige Strings kann und dann noch so ist wie in Graphen_algos !
  a := Nil;
  h := canvas.TextHeight('8') + 2 * DefaultTextborderY;
  // Löschen
  canvas.Pen.Color := clBlack;
  canvas.brush.color := clwhite;
  //canvas.brush.color := $00BFBFBF;
  canvas.brush.Style := bssolid;
  canvas.Rectangle(r);
  // 1. Rendern aller Kanten
  ei := 0;
  For i := 0 To High(Fnodes) Do Begin
    If Fnodes[i].Visible Then Begin
      For j := 0 To high(Fnodes[i].Edges) Do Begin // Rendern aller Kanten zu anderen Knoten
        If Fnodes[Fnodes[i].Edges[j].EndIndex].Visible Then Begin
          canvas.Pen.color := Fnodes[i].Edges[j].EdgeColor;
          If assigned(OnDrawEdge) Then Begin
            drawIt := Not OnDrawEdge(self, canvas, ei + j);
          End
          Else Begin
            drawIt := true;
          End;
          If drawIt Then Begin
            If i = Fnodes[i].Edges[j].EndIndex Then Begin
              w := round((canvas.TextWidth(Fnodes[i].Caption) * 0.5 + 2 * DefaultTextborderX));
              // Die von der LCL unterstützte Bezier Routine kann nur mit 4*I Punktwolken umgehen !!
              //Warum geht das net ?
              setlength(a, 8);
              // Halbbogen unten rum
              a[0] := point(fnodes[i].Position.x, fnodes[i].Position.y + h Div 2);
              a[1] := point(fnodes[i].Position.x, fnodes[i].Position.y + h Div 2 + DefaultDistanceY Div 2);
              a[2] := point(fnodes[i].Position.x + w, fnodes[i].Position.y + h Div 2 + DefaultDistanceY Div 2);
              a[3] := point(fnodes[i].Position.x + w, fnodes[i].Position.y);
              // Halbbogen oben Rum
              a[4] := point(fnodes[i].Position.x + w, fnodes[i].Position.y);
              a[5] := point(fnodes[i].Position.x + w, fnodes[i].Position.y - h Div 2 - DefaultDistanceY Div 2);
              a[6] := point(fnodes[i].Position.x, fnodes[i].Position.y - h Div 2 - DefaultDistanceY Div 2);
              a[7] := point(fnodes[i].Position.x, fnodes[i].Position.y - h Div 2);
            End
            Else Begin
              setlength(a, 4);
              a[0] := point(fnodes[i].Position.x, fnodes[i].Position.y + h Div 2);
              a[1] := point(fnodes[i].Position.x, fnodes[i].Position.y + DefaultDistanceY Div 2 + h Div 2);
              a[2] := point(fnodes[Fnodes[i].Edges[j].EndIndex].Position.x, fnodes[Fnodes[i].Edges[j].EndIndex].Position.y - DefaultDistanceY Div 2 - h Div 2);
              a[3] := point(fnodes[Fnodes[i].Edges[j].EndIndex].Position.x, fnodes[Fnodes[i].Edges[j].EndIndex].Position.y - h Div 2);
            End;
            // rein Rechnen des Allgemeinen Offsets
            For k := 0 To high(a) Do Begin
              a[k] := point(a[k].X + offx, a[k].y + offy);
            End;
            canvas.PolyBezier(a, false, false);
            // Malen der Pfeilspitze
            If Fnodes[i].Edges[j].Directed Then Begin
              canvas.MoveTo(a[high(a)].X, a[high(a)].y);
              canvas.lineTo(a[high(a)].X - PfeilDx, a[high(a)].y - PfeilDy);
              canvas.MoveTo(a[high(a)].X, a[high(a)].y);
              canvas.lineTo(a[high(a)].X + PfeilDx, a[high(a)].y - PfeilDy);
            End;
          End;
        End;
      End;
    End;
    ei := ei + length(Fnodes[i].Edges);
  End;
  // 1.1 Rendern aller Kanten Label
  ei := 0;
  For i := 0 To High(Fnodes) Do Begin
    If Fnodes[i].Visible Then Begin
      For j := 0 To high(Fnodes[i].Edges) Do Begin // Rendern aller Kanten zu anderen Knoten
        If (Fnodes[Fnodes[i].Edges[j].EndIndex].Visible) And (Fnodes[i].Edges[j].EdgeCaption <> '') Then Begin
          If assigned(OnDrawEdge) Then Begin
            drawIt := Not OnDrawEdgeCaption(self, canvas, ei + j);
          End
          Else Begin
            drawIt := true;
          End;
          If drawIt Then Begin
            If i = Fnodes[i].Edges[j].EndIndex Then Begin
              // Eine Kante die auf sich selbst zeigt und eine Caption hat
              x := (fnodes[i].Position.x);
              y := (fnodes[i].Position.Y);
              w := round((canvas.TextWidth(Fnodes[i].Caption) * 0.5 + 2 * DefaultTextborderX));
              x := x + w;
              y := y - Canvas.TextHeight(Fnodes[i].Edges[j].EdgeCaption) Div 2;
            End
            Else Begin
              // Normale Kante
              x := (fnodes[i].Position.x + fnodes[Fnodes[i].Edges[j].EndIndex].Position.x) Div 2;
              y := (fnodes[i].Position.Y + fnodes[Fnodes[i].Edges[j].EndIndex].Position.Y) Div 2;
              x := x - Canvas.TextWidth(Fnodes[i].Edges[j].EdgeCaption) Div 2;
              y := y - Canvas.TextHeight(Fnodes[i].Edges[j].EdgeCaption) Div 2;
            End;
            x := x + OffX;
            y := y + OffY;
            canvas.TextOut(x, y, Fnodes[i].Edges[j].EdgeCaption);
          End;
        End;
      End;
    End;
    ei := ei + length(Fnodes[i].Edges);
  End;
  // 2. Rendern aller Nodes
  For i := 0 To High(Fnodes) Do Begin
    canvas.Pen.Style := psSolid;
    If Fnodes[i].Selected Then Begin
      canvas.Pen.Color := clred;
    End
    Else Begin
      canvas.Pen.Color := clblack;
    End;
    canvas.brush.color := Fnodes[i].Color;
    canvas.brush.Style := bssolid;
    If assigned(OnDrawNode) Then Begin
      DrawIt := Not OnDrawNode(Self, canvas, i);
    End
    Else
      drawIt := true;
    If drawIt Then Begin
      s := Fnodes[i].Caption;
      w := canvas.TextWidth(s) + 2 * DefaultTextborderX;

      //w1 := max(20, w1);
      //h1 := max(20, h1);
      x := Fnodes[i].Position.x;
      y := Fnodes[i].Position.y;
      If Fnodes[i].Visible Then Begin
        If Fnodes[i].Marked Then Begin
          canvas.Brush.Color := cllime;
        End;
        If assigned(OnPrepareNodeCanvas) Then Begin
          OnPrepareNodeCanvas(self, Canvas, i);
        End;
        canvas.Ellipse(x - w Div 2 + offx, y - h Div 2 + offy, x + w Div 2 + offx, y + h Div 2 + offy);
        canvas.TextOut(x - w Div 2 + DefaultTextborderX + offx, y - h Div 2 + DefaultTextborderY + offy, s);
      End
      Else Begin // Wenn ein Node Unsichtbar ist, dann Rendern wir nur eine Siluette
        If Not Fnodes[i].Selected Then Begin
          canvas.Pen.Color := clGray;
          canvas.Pen.Style := psDot;
        End;
        canvas.font.color := clgray;
        If assigned(OnPrepareNodeCanvas) Then Begin
          OnPrepareNodeCanvas(self, Canvas, i);
        End;
        canvas.Ellipse(x - w Div 2 + offx, y - h Div 2 + offy, x + w Div 2 + offx, y + h Div 2 + offy);
        canvas.TextOut(x - w Div 2 + DefaultTextborderX + offx, y - h Div 2 + DefaultTextborderY + offy, s);
        canvas.font.color := clblack;
        canvas.Pen.Style := psSolid;
        canvas.Pen.Color := clblack;
      End;
    End;
    canvas.brush.color := clwhite;
  End;
End;

Function TGraph.AddNode(NodeName, NodeCaption: String;
  OverwriteIfExists: Boolean; Depth: integer): integer;
Begin
  result := AddNode(NodeName, NodeCaption, Nil, OverwriteIfExists, Depth);
End;

Function TGraph.AddNode(NodeName, NodeCaption: String; NodeUserData: Pointer;
  OverwriteIfExists: Boolean; Depth: integer): integer;
Var
  ni: integer;
Begin
  result := -1;
  ni := AddNode2(Nodename, NodeCaption, Depth);
  If assigned(Fnodes[ni].UserData) Then Begin
    If Not assigned(OnDeleteNodeUserData) Then
      Raise Exception.Create('Error ' + ClassName + '.FreeNodeUserData not set.');
    OnDeleteNodeUserData(self, Fnodes[ni].UserData);
  End;
  Fnodes[ni].UserData := NodeUserData;
  FChanged := True;
  result := ni;
End;

Function TGraph.FindNode(NodeName: String): Integer;
Var
  i: Integer;
Begin
  result := -1;
  For i := 0 To high(Fnodes) Do Begin
    If Fnodes[i].Name = NodeName Then Begin
      result := i;
      exit;
    End;
  End;
End;

//Function TGraph.AddNode(NodeName, NodeCaption, NodeClient,
//  NodeClientCaption: String; Depth: Integer; EdgeColor: Tcolor): integer;
//Begin
//  result := AddNode(NodeName, NodeCaption, Nil, NodeClient, NodeClientCaption, Depth, EdgeColor);
//End;

//Function TGraph.AddNode(NodeName, NodeCaption: String; NodeUserData: Pointer;
//  NodeClient, NodeClientCaption: String; Depth: Integer; EdgeColor: TColor
//  ): integer;
//Var
//  ni, ci: Integer;
//  i: Integer;
//Begin
//  // Einfügen
//  ni := AddNode2(nodename, NodeCaption, Depth);
//  result := ni;
//  ci := AddNode2(NodeClient, NodeClientCaption, Fnodes[ni].Level + 1);
//  If assigned(Fnodes[ni].UserData) Then Begin
//    //If Not assigned(FreeNodeUserData) Then
//      //Raise Exception.Create('Error ' + ClassName + '.FreeNodeInfo not set.');
//    //FreeNodeUserData(self, Fnodes[ni].UserData);
//  End;
//  FChanged := True;
//  Fnodes[ni].UserData := NodeUserData;
//  //Gibt es die Kante schon ?
//  For i := 0 To high(fnodes[ni].Edges) Do Begin
//    If Fnodes[ni].Edges[i].EndIndex = ci Then Begin
//      Fnodes[ni].Edges[i].EdgeColor := EdgeColor;
//      exit;
//    End;
//  End;
//  // Erzeugen der Richtigen Kannten
//  setlength(Fnodes[ni].Edges, high(Fnodes[ni].Edges) + 2);
//  Fnodes[ni].Edges[high(Fnodes[ni].Edges)].EdgeColor := EdgeColor;
//  Fnodes[ni].Edges[high(Fnodes[ni].Edges)].EndIndex := ci;
//End;

Procedure TGraph.AddEdge(StartNode, StartNodeCaption, EndNode,
  EndNodeCaption: String; EdgeColor: TColor; Userdata: Pointer;
  Directed: Boolean; EdgeLabel: String);
Var
  ni: Integer;
  ci: Integer;

Begin
  // Einfügen
  ni := AddNode2(StartNode, StartNodeCaption, 0);
  ci := AddNode2(EndNode, EndNodeCaption, Fnodes[ni].Level + 1);
  AddEdge(ni, ci, EdgeColor, Userdata, Directed, EdgeLabel);
End;

Procedure TGraph.AddEdge(StartNode, EndNode: Integer; EdgeColor: TColor;
  Userdata: Pointer; Directed: Boolean; EdgeCaption: String);
Var
  i: Integer;
Begin
  //Gibt es die Kante schon ?
  For i := 0 To high(fnodes[StartNode].Edges) Do Begin
    If Fnodes[StartNode].Edges[i].EndIndex = EndNode Then Begin
      Fnodes[StartNode].Edges[i].EdgeColor := EdgeColor;
      Fnodes[StartNode].Edges[i].EdgeCaption := EdgeCaption;
      Fnodes[StartNode].Edges[i].Directed := Directed;
      // TODO: Implementieren eines Freigeben einer Kanten Userdata !
      Fnodes[StartNode].Edges[i].UserData := Userdata;
      exit;
    End;
  End;
  FChanged := True;

  // Erzeugen der Richtigen Kannten
  setlength(Fnodes[StartNode].Edges, high(Fnodes[StartNode].Edges) + 2);
  Fnodes[StartNode].Edges[high(Fnodes[StartNode].Edges)].StartIndex := StartNode;
  Fnodes[StartNode].Edges[high(Fnodes[StartNode].Edges)].EndIndex := EndNode;
  Fnodes[StartNode].Edges[high(Fnodes[StartNode].Edges)].EdgeColor := EdgeColor;
  Fnodes[StartNode].Edges[high(Fnodes[StartNode].Edges)].EdgeCaption := EdgeCaption;
  Fnodes[StartNode].Edges[high(Fnodes[StartNode].Edges)].Directed := Directed;
  Fnodes[StartNode].Edges[high(Fnodes[StartNode].Edges)].UserData := Userdata; // Noch nicht Supportet aber mal drin ;)
  Fnodes[StartNode].Edges[high(Fnodes[StartNode].Edges)].Selected := false;
  Fnodes[StartNode].Edges[high(Fnodes[StartNode].Edges)].CyclicMark := false;
End;

Procedure TGraph.DelEdge(StartNodeIndex, EndNodeIndex: Integer);
Var
  i, j: integer;
Begin
  For i := 0 To high(Fnodes[StartNodeIndex].Edges) Do Begin
    If Fnodes[StartNodeIndex].Edges[i].EndIndex = EndNodeIndex Then Begin
      FChanged := True;
      For j := i To high(Fnodes[StartNodeIndex].Edges) - 1 Do
        Fnodes[StartNodeIndex].Edges[j] := Fnodes[StartNodeIndex].Edges[j + 1];
      setlength(Fnodes[StartNodeIndex].Edges, high(Fnodes[StartNodeIndex].Edges));
      exit;
    End;
  End;
End;

Function TGraph.GetMaxDimension: TPoint;
Var
  i, x, y: Integer;
Begin
  x := 0;
  y := 0;
  For i := 0 To high(Fnodes) Do Begin
    x := max(x, Fnodes[i].Position.x + DefaultDistancex Div 2);
    y := max(y, Fnodes[i].Position.y + DefaultDistancey Div 2);
  End;
  result := point(x, y);
End;

Function TGraph.GetNodeIndex(x, y: integer; Const Canvas: TCanvas): Integer;
Var
  i: Integer;
  s: String;
  xx, yy, w, h: Integer;
Begin
  result := -1;
  For i := 0 To high(Fnodes) Do Begin
    s := Fnodes[i].Caption;
    w := canvas.TextWidth(s) + 2 * DefaultTextborderX;
    h := canvas.TextHeight(s) + 2 * DefaultTextborderY;
    xx := Fnodes[i].Position.x;
    yy := Fnodes[i].Position.y;
    If InInterval(x, xx - w Div 2, xx + w Div 2) And
      InInterval(y, yy - h Div 2, yy + h Div 2) Then Begin
      result := i;
      exit;
    End;
  End;
End;

Function TGraph.GetNodeIndexByName(NodeName: String): integer;
Var
  i: Integer;
Begin
  result := -1;
  For i := 0 To High(Fnodes) Do
    If lowercase(FNodes[i].Name) = Lowercase(NodeName) Then Begin
      result := i;
      exit;
    End;
End;

Function TGraph.GetNodePosition(Index: Integer): TPoint;
Begin
  If (index >= 0) And (index <= High(Fnodes)) Then Begin
    result := Fnodes[Index].Position;
  End
  Else Begin
    result := point(0, 0);
  End;
End;

Function TGraph.GetNodeUserData(index: integer): Pointer;
Begin
  result := Nil;
  If (index >= 0) And (index <= High(Fnodes)) Then
    result := Fnodes[index].UserData;
End;

Procedure TGraph.SetNodeUserData(Index: integer; aValue: Pointer);
Begin
  If (index >= 0) And (index <= High(Fnodes)) Then Begin
    If Fnodes[index].UserData <> aValue Then Begin
      If assigned(Fnodes[index].UserData) Then
        OnDeleteNodeUserData(self, Fnodes[index].UserData);
      Fnodes[index].UserData := aValue;
      FChanged := true;
    End;
  End;
End;

Procedure TGraph.SetNodePosition(Index: integer; NewPosition: Tpoint);
Begin
  If (index >= 0) And (index <= High(Fnodes)) Then Begin
    Fnodes[Index].Position := NewPosition;
    FChanged := True;
  End;
End;

Procedure TGraph.DelNode(Index: Integer);
Var
  i, j, k: Integer;
Begin
  If (index >= 0) And (index <= High(Fnodes)) Then Begin
    FChanged := True;
    // Erst mal werden Alle Kanten auf den Knoten Gelöscht
    For i := 0 To high(Fnodes) Do Begin
      // Entfernen der Reverenz auf den Knoten
      For k := high(Fnodes[i].Edges) Downto 0 Do Begin
        If Fnodes[i].Edges[k].EndIndex = Index Then Begin
          If assigned(Fnodes[i].Edges[k].UserData) Then Begin
            // TODO: Sauber Freigeben
          End;
          For j := k To high(Fnodes[i].Edges) - 1 Do
            Fnodes[i].Edges[j] := Fnodes[i].Edges[j + 1];
          setlength(Fnodes[i].Edges, high(Fnodes[i].Edges));
        End;
      End;
      // Ernidrigen aller Referenzen die nun nicht mehr stimmen
      For k := high(Fnodes[i].Edges) Downto 0 Do Begin
        If Fnodes[i].Edges[k].EndIndex > Index Then
          Fnodes[i].Edges[k].EndIndex := Fnodes[i].Edges[k].EndIndex - 1;
        If Fnodes[i].Edges[k].StartIndex > Index Then
          Fnodes[i].Edges[k].StartIndex := Fnodes[i].Edges[k].StartIndex - 1;
      End;
    End;
    // Jetzt kann der Knoten gefahrlost gelöscht werden
    If assigned(Fnodes[index].UserData) Then Begin
      // TODO: Sauber Freigeben
    End;
    For i := index To high(Fnodes) - 1 Do Begin
      Fnodes[i] := Fnodes[i + 1];
    End;
    setlength(Fnodes, high(Fnodes));
  End;
End;

Function TGraph.NodeCount: Integer;
Begin
  result := high(Fnodes) + 1;
End;

Function TGraph.EdgeCount: Integer;
Var
  i: Integer;
Begin
  result := 0;
  For i := 0 To high(Fnodes) Do Begin
    result := result + Length(Fnodes[i].Edges);
  End;
End;

Procedure TGraph.DeleteAllNodesNotMarkedAsAdded;
Var
  i: integer;
Begin
  For i := high(Fnodes) Downto 0 Do Begin
    If Not Fnodes[i].Added Then Begin
      DelNode(i);
    End;
  End;
End;

Procedure TGraph.MarkAllNodesAsNotAdded;
Var
  i: Integer;
Begin
  For i := 0 To high(Fnodes) Do Begin
    Fnodes[i].Added := false;
  End;
End;

Procedure TGraph.SaveToStream(Const Stream: TStream);
Var
  i: integer;
Begin
  i := high(Fnodes) + 1;
  Stream.write(GraphsVersion, sizeof(GraphsVersion));
  Stream.Write(i, SizeOf(i));
  For i := 0 To high(Fnodes) Do Begin
    NodeToStream(Stream, fnodes[i]);
  End;
  i := high(FLevelLefts) + 1;
  Stream.Write(i, SizeOf(i));
  For i := 0 To high(FLevelLefts) Do Begin
    Stream.Write(FLevelLefts[i], sizeof(FLevelLefts[i]));
  End;
  FChanged := false;
End;

Function TGraph.LoadFromStream(Const Stream: TStream): Boolean;
Var
  FileVersion, maxr, i, j: integer;
Begin
  result := false;
  Clear;
  // Lesen der File Version
  FileVersion := -1;
  Stream.read(FileVersion, SizeOf(FileVersion));
  If FileVersion > GraphsVersion Then Begin
    Raise Exception.Create('Invalid file version.');
    Stream.free;
    exit;
  End;
  i := 0;
  Stream.read(i, SizeOf(i));
  setlength(fnodes, i);
  maxr := 0;
  For i := 0 To high(Fnodes) Do Begin
    fnodes[i] := NodeFromStream(Stream, FileVersion);
    For j := 0 To high(Fnodes[i].Edges) Do Begin
      Fnodes[i].Edges[j].StartIndex := i;
    End;
    maxr := max(fnodes[i].Position.x, maxr);
  End;
  i := 0;
  Stream.read(i, SizeOf(i));
  setlength(FLevelLefts, i);
  For i := 0 To high(FLevelLefts) Do Begin
    Stream.read(FLevelLefts[i], sizeof(FLevelLefts[i]));
    // Versuch, beim Neu laden, dafür zu sorgen dass neu erstellte Knoten nicht zu weit Rechts erzeugt werden
    FLevelLefts[i] := min((maxr Div DefaultDistanceX), FLevelLefts[i]);
  End;
  FChanged := false;
  result := true;
End;

Procedure TGraph.SaveToFile(Const Filename: String);
Var
  f: TFileStream;
Begin
  f := TFileStream.Create(Filename, fmCreate Or fmOpenWrite);
  SaveToStream(f);
  f.free;
End;

Function TGraph.LoadFromFile(Const Filename: String): Boolean;
Var
  f: TFileStream;
Begin
  result := false;
  If Not FileExists(Filename) Then exit;
  f := TFileStream.Create(Filename, fmOpenRead);
  result := LoadFromStream(f);
  f.free;
End;

Procedure TGraph.MarkNode(Index: integer);
Begin
  If (Index < 0) Or (Index > High(Fnodes)) Then exit;
  FChanged := True;
  Fnodes[Index].Marked := true;
End;

Function TGraph.GetMarked(index: integer): Boolean;
Begin
  result := false;
  If (Index < 0) Or (Index > High(Fnodes)) Then exit;
  result := Fnodes[index].Marked;
End;

Procedure TGraph.UnMarkNode(Index: integer);
Begin
  If (Index < 0) Or (Index > High(Fnodes)) Then exit;
  FChanged := True;
  Fnodes[Index].Marked := false;
End;

Procedure TGraph.DelMarkedNodes;
Var
  i: integer;
Begin
  For i := high(Fnodes) Downto 0 Do Begin
    If Fnodes[i].Marked Then DelNode(i);
  End;
End;

Procedure TGraph.DelallEdges;
Var
  i: Integer;
Begin
  For i := 0 To high(Fnodes) Do Begin
    setlength(Fnodes[i].Edges, 0);
  End;
  FChanged := True;
End;

Procedure TGraph.MarkFistGenChilds(Index: Integer);
Var
  i: integer;
Begin
  If (index < 0) Or (index > high(Fnodes)) Then exit;
  FChanged := True;
  Fnodes[index].Marked := true;
  For i := 0 To high(Fnodes[index].Edges) Do
    Fnodes[Fnodes[index].Edges[i].EndIndex].Marked := true;
End;

Procedure TGraph.MarkAllChilds(Index: integer);
  Procedure SubMark(i: integer);
  Var
    j: integer;
  Begin
    If Fnodes[i].Marked Then exit;
    Fnodes[i].Marked := true;
    For j := 0 To high(Fnodes[i].Edges) Do
      SubMark(Fnodes[i].Edges[j].EndIndex);
  End;

Var
  k: integer;
  a: Array Of Boolean;
Begin
  a := Nil;
  If (index < 0) Or (index > high(Fnodes)) Then exit;
  FChanged := True;
  // Bakup und Reset der Marker
  setlength(a, high(Fnodes) + 1);
  For k := 0 To high(Fnodes) Do Begin
    a[k] := Fnodes[k].Marked;
    Fnodes[k].Marked := false;
  End;
  SubMark(index);
  // Restore der Marker die vorher schon da waren
  For k := 0 To high(Fnodes) Do Begin
    Fnodes[k].Marked := a[k] Or Fnodes[k].Marked;
  End;
  setlength(a, 0);
End;

Procedure TGraph.MarkFistGenParents(Index: integer);
Var
  i, j: integer;
Begin
  If (index < 0) Or (index > high(Fnodes)) Then exit;
  FChanged := True;
  Fnodes[index].Marked := true;
  For i := 0 To high(Fnodes) Do
    For j := 0 To high(Fnodes[i].Edges) Do Begin
      If Fnodes[i].Edges[j].EndIndex = Index Then Begin
        Fnodes[i].Marked := true;
        break;
      End;
    End;
End;

Procedure TGraph.MarkAllParents(Index: integer);

  Procedure SubMark(i: integer);
  Var
    j: Integer;
    k: Integer;
  Begin
    If Fnodes[i].Marked Then exit;
    Fnodes[i].Marked := true;
    For j := 0 To high(Fnodes) Do Begin
      For k := 0 To high(Fnodes[j].Edges) Do Begin
        If Fnodes[j].Edges[k].EndIndex = i Then Begin
          SubMark(j);
        End;
      End;
    End;
  End;

Var
  k: integer;
  a: Array Of Boolean;
Begin
  a := Nil;
  If (index < 0) Or (index > high(Fnodes)) Then exit;
  FChanged := True;
  // Bakup und Reset der Marker
  setlength(a, high(Fnodes) + 1);
  For k := 0 To high(Fnodes) Do Begin
    a[k] := Fnodes[k].Marked;
    Fnodes[k].Marked := false;
  End;
  SubMark(index);
  // Restore der Marker die vorher schon da waren
  For k := 0 To high(Fnodes) Do Begin
    Fnodes[k].Marked := a[k] Or Fnodes[k].Marked;
  End;
  setlength(a, 0);
End;

Procedure TGraph.MarkPathsBetweenNodes(StartNode, EndNode: integer);

  Function Trace(Index: integer): Boolean;
  Var
    i: Integer;
  Begin
    result := false;
    If index = EndNode Then Begin
      result := true;
      exit;
    End;
    If Fnodes[Index].Visited Then exit;
    fnodes[index].Visited := true;
    For i := 0 To high(fnodes[index].Edges) Do Begin
      If trace(fnodes[index].Edges[i].EndIndex) Then Begin
        fnodes[index].Marked := true;
        result := true;
        // break; -- Wenn das Drin ist, dann wird der Rekursiv zuerst erreichbare Pfad markiert aber nicht alle.
      End;
    End;
  End;

Begin
  If (StartNode < 0) Or (StartNode > high(Fnodes)) Then exit;
  If (EndNode < 0) Or (EndNode > high(Fnodes)) Then exit;
  FChanged := True;
  UnVisit();
  Fnodes[StartNode].Marked := true;
  Fnodes[EndNode].Marked := true;
  Trace(StartNode);
End;

Procedure TGraph.MarkNodesWithMultipleParents;
Var
  i: integer;
  a: Array Of Integer;
  j: Integer;
Begin
  a := Nil;
  FChanged := True;
  setlength(a, high(Fnodes) + 1);
  For i := 0 To high(a) Do
    a[i] := 0;
  For i := 0 To high(Fnodes) Do
    For j := 0 To high(fnodes[i].Edges) Do Begin
      a[fnodes[i].Edges[j].EndIndex] := a[fnodes[i].Edges[j].EndIndex] + 1;
    End;
  For i := 0 To high(a) Do
    If a[i] >= 2 Then
      Fnodes[i].Marked := true;
End;

Procedure TGraph.MarkNodesWithNoParents;
Var
  i: integer;
  a: Array Of Integer;
  j: Integer;
Begin
  a := Nil;
  FChanged := True;
  setlength(a, high(Fnodes) + 1);
  For i := 0 To high(a) Do
    a[i] := 0;
  For i := 0 To high(Fnodes) Do
    For j := 0 To high(fnodes[i].Edges) Do Begin
      a[fnodes[i].Edges[j].EndIndex] := a[fnodes[i].Edges[j].EndIndex] + 1;
    End;
  For i := 0 To high(a) Do
    If a[i] = 0 Then
      Fnodes[i].Marked := true;
End;

Procedure TGraph.MarkNodesWithNoChildrens;
Var
  i: Integer;
Begin
  For i := 0 To high(Fnodes) Do
    If high(fnodes[i].Edges) = -1 Then Begin
      FChanged := True;
      Fnodes[i].Marked := true;
    End;
End;

Procedure TGraph.HideNode(index: integer);
Begin
  If (index < 0) Or (index > high(Fnodes)) Then exit;
  Fnodes[index].Visible := false;
  FChanged := True;
End;

Procedure TGraph.ShowNode(index: integer);
Begin
  If (index < 0) Or (index > high(Fnodes)) Then exit;
  FChanged := True;
  Fnodes[index].Visible := true;
End;

Function TGraph.IsNodeVisible(Index: integer): Boolean;
Begin
  result := false;
  If (index < 0) Or (index > high(Fnodes)) Then exit;
  result := Fnodes[index].Visible;
End;

Procedure TGraph.ShowAllNodes;
Var
  i: integer;
Begin
  For i := 0 To high(Fnodes) Do
    Fnodes[i].Visible := true;
  FChanged := True;
End;

Procedure TGraph.UndoMarkings;
Var
  i: Integer;
Begin
  For i := 0 To high(Fnodes) Do
    Fnodes[i].Marked := false;
  FChanged := True;
End;

Function TGraph.GetSelected(index: integer): Boolean;
Begin
  result := false;
  If (index < 0) Or (index > high(Fnodes)) Then exit;
  result := Fnodes[index].Selected;
End;

Function TGraph.GetNodeName(index: Integer): String;
Begin
  result := '';
  If (index < 0) Or (index > high(Fnodes)) Then exit;
  result := Fnodes[index].Name;
End;

Function TGraph.GetNodeClientList(index: integer): TBoundArray;
Var
  i: integer;
Begin
  result := Nil;
  If (index < 0) Or (index > high(Fnodes)) Then exit;
  setlength(result, high(Fnodes[index].Edges) + 1);
  For i := 0 To high(Fnodes[index].Edges) Do
    result[i] := Fnodes[index].Edges[i].EndIndex;
End;

Procedure TGraph.Select(index: integer);
Begin
  If (index < 0) Or (index > high(Fnodes)) Then exit;
  FChanged := True;
  Fnodes[index].Selected := true;
End;

Procedure TGraph.DeSelect(index: integer);
Begin
  If (index < 0) Or (index > high(Fnodes)) Then exit;
  FChanged := True;
  Fnodes[index].Selected := false;
End;

Procedure TGraph.DeSelectAll;
Var
  i, j: integer;
Begin
  For i := high(Fnodes) Downto 0 Do Begin
    Fnodes[i].Selected := false;
    For j := 0 To high(Fnodes[i].Edges) Do Begin
      Fnodes[i].Edges[j].Selected := false;
    End;
  End;
  FChanged := True;
End;

Procedure TGraph.DelAllSelectedNodes;
Var
  i: integer;
Begin
  For i := high(Fnodes) Downto 0 Do Begin
    If Fnodes[i].Selected Then DelNode(i);
  End;
End;

Procedure TGraph.SelectRectangle(x1, y1, x2, y2: integer; Const Canvas: TCanvas
  );
Var
  w, h, xx, yy, nx1, nx2, ny1, ny2, i: integer;
  s: String;
Begin
  For i := 0 To high(Fnodes) Do Begin
    s := Fnodes[i].Caption;
    w := canvas.TextWidth(s) + 2 * DefaultTextborderX;
    h := canvas.TextHeight(s) + 2 * DefaultTextborderY;
    xx := Fnodes[i].Position.x;
    yy := Fnodes[i].Position.y;
    nx1 := xx - w Div 2;
    nx2 := xx + w Div 2;
    ny1 := yy - h Div 2;
    ny2 := yy + h Div 2;
    Fnodes[i].Selected := Fnodes[i].Selected Or RectIntersectRect(v2(x1, y1), v2(x2, y2), v2(nx1, ny1), v2(nx2, ny2));
  End;
End;

Procedure TGraph.UnVisit;
Var
  i: integer;
Begin
  For i := 0 To high(Fnodes) Do Begin
    Fnodes[i].Visited := false;
  End;
  FChanged := True;
End;

Procedure TGraph.MarkCycles;

  Function dfs(node, SuchNode: integer): Boolean;
  Var
    j: integer;
  Begin
    result := false;
    If node = SuchNode Then Begin
      result := true;
      exit;
    End;
    If Not Fnodes[node].Visited Then Begin
      Fnodes[node].Visited := true;
      For j := 0 To high(Fnodes[node].Edges) Do Begin
        If dfs(Fnodes[node].Edges[j].EndIndex, SuchNode) Then Begin
          Fnodes[Node].Edges[j].CyclicMark := true;
          result := true;
        End;
      End;
    End;
  End;

Var
  i, j: integer;
Begin
  // Erst mal alle Markierungen entfernen
  For i := 0 To high(Fnodes) Do Begin
    For j := 0 To high(Fnodes[i].Edges) Do
      Fnodes[i].Edges[j].CyclicMark := false;
  End;
  For i := 0 To high(Fnodes) Do Begin
    // Hier die Farbe für einen evtl. neu gefundenen Zyklus Setzen
    unvisit();
    Fnodes[i].Visited := true;
    For j := 0 To high(Fnodes[i].Edges) Do
      If dfs(Fnodes[i].Edges[j].EndIndex, i) Then Begin
        Fnodes[i].Edges[j].CyclicMark := true;
      End;
  End;
  For i := 0 To High(Fnodes) Do Begin
    For j := 0 To High(Fnodes[i].Edges) Do
      If Fnodes[i].Edges[j].CyclicMark Then
        Fnodes[Fnodes[i].Edges[j].EndIndex].Marked := true;
  End;
  FChanged := True;
End;

{ TgraphBox }

Constructor TGraphBox.Create(AOwner: TComponent);
Begin
  Inherited Create(AOwner);
  Visible := true;
  ControlStyle := ControlStyle + [csReplicatable]; // Was genau macht diese Zeile ?
  With GetControlClassDefaultSize Do // Was genau macht diese Zeile ?
    SetInitialBounds(0, 0, CX, CY);
  fGraph := TGraph.create;
  fSelected := -1;
  Autoselect := true;
End;

Destructor TGraphBox.Destroy;
Begin
  fGraph.Free;
  Inherited Destroy;
End;

Procedure TGraphBox.Paint;
Begin
  If csDesigning In ComponentState Then Begin
    Canvas.Brush.Color := Color;
    Canvas.Pen.Style := psDash;
    Canvas.Pen.Color := clBlack;
    Canvas.Brush.Style := bsClear;
    Canvas.Rectangle(0, 0, Self.Width - 1, Self.Height - 1);
    Canvas.Line(0, 0, Self.Width - 1, Self.Height - 1);
    Canvas.Line(Self.Width - 1, 0, 0, Self.Height - 1);
    exit;
  End;
  // TODO: Hier ist sicher noch luft nach oben ..
  fGraph.PaintTo(canvas, 0, 0, ClientRect);
  Inherited paint;
End;

Procedure TGraphBox.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
Var
  OldSelected: integer;
Begin
  If Autoselect Then Begin
    fGraph.DeSelectAll();
    OldSelected := fSelected;
    fSelected := fGraph.GetNodeIndex(x, y, canvas);
    fGraph.Select(fSelected);
  End;
  Inherited MouseDown(Button, Shift, X, Y);
  If Autoselect And (OldSelected <> fSelected) Then Begin
    Invalidate;
  End;
End;

Procedure TGraphBox.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
Begin
  If Autoselect Then Begin
    fGraph.DeSelectAll();
    fSelected := fGraph.GetNodeIndex(x, y, canvas);
    fGraph.Select(fSelected);
  End;
  Inherited MouseUp(Button, Shift, X, Y);
  If Autoselect Then Begin
    Invalidate;
  End;
End;

Procedure TGraphBox.MouseMove(Shift: TShiftState; X, Y: Integer);
Begin
  Inherited MouseMove(Shift, X, Y);
End;

Procedure TGraphBox.Resize;
//Var
//  i: Integer;
//  percent: Single;
Begin
  Inherited Resize;
  //If (fOldWidth <> width) Or (fOldHeight <> height) Then Begin
  //  If (fOldWidth = -1) Or (fOldHeight = -1) Then Begin
  //    fOldHeight := Height;
  //    fOldWidth := Width;
  //    exit;
  //  End;
  //  // Anpassen der "Relativen" Knoten
  //  If ScaleOnResize Then Begin
  //    For i := 0 To high(FNodes) Do Begin
  //      If FNodes[i].NodeIsRelative Then Begin
  //        percent := FNodes[i].Position.x / fOldWidth;
  //        FNodes[i].Position.x := round(percent * Width);
  //        percent := FNodes[i].Position.y / fOldHeight;
  //        FNodes[i].Position.y := round(percent * height);
  //      End;
  //    End;
  //  End;
  //  // Übernehmen der Neuen Dimension
  //  fOldHeight := max(Height, 1);
  //  fOldWidth := max(Width, 1);
  //  // Invalidate; // Scheint nicht Notwendig
  //End;
End;

Procedure TGraphBox.SetAutoselect(AValue: Boolean);
Begin
  If fAutoselect = AValue Then Exit;
  fAutoselect := AValue;
  If Not fAutoselect Then fSelected := -1;
End;

End.

