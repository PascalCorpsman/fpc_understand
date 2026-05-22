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
Unit Unit3;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  Menus, laz.VirtualTrees, ufpc_understand;

Type

  TNodeData = Record
    Columns: Array Of String;
    FileIndex: integer; // Index in fList
  End;

  PNodeData = ^TNodeData;

  TItem = Record
    parent: String;
    Name: String;
    FileName: String;
    Line: String;
    IsInterface: Boolean;
    FileIndex: integer; // Index in fList
  End;

  { TForm3 }

  TForm3 = Class(TForm)
    Button1: TButton;
    LazVirtualStringTree1: TLazVirtualStringTree;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    PopupMenu1: TPopupMenu;
    SaveDialog1: TSaveDialog;
    StatusBar1: TStatusBar;
    Procedure Button1Click(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure FormDestroy(Sender: TObject);
    Procedure LazVirtualStringTree1DblClick(Sender: TObject);
    Procedure LazVirtualStringTree1FreeNode(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    Procedure LazVirtualStringTree1GetNodeDataSize(Sender: TBaseVirtualTree;
      Var NodeDataSize: Integer);
    Procedure LazVirtualStringTree1GetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      Var CellText: String);
    Procedure MenuItem1Click(Sender: TObject);
  private
    CSV_export: TStringList;
    fList: TProjectFilesInfo; // Das wird aktuell noch nicht verwendet, könnte aber
    fProject: TProject;
  public
    Function LoadClasses(aList: TProjectFilesInfo; Project: TProject): Boolean;
  End;

Var
  Form3: TForm3;

Implementation

{$R *.lfm}

Uses
  math
  , unit13; // Code Window

Type
  PRoot = ^TRoot;

  TRoot = Record
    RootElement: TItem;
    Childs: Array Of PRoot;
  End;

  { TClassSorter }

  TClassSorter = Class
  private
    fRoots: Array Of PRoot;
    fWithInterfaces: Boolean;
    Procedure FreeRoot(Var Root: PRoot);
    Procedure SortChilds(Var Root: PRoot);
    Function InsertRootEle(Const Element: PRoot): Boolean;
  public
    Constructor Create(WithInterfaces: Boolean); virtual;
    Destructor Destroy(); override;
    Procedure AddItem(Const Item: TItem);
    Procedure Finish();
  End;

  { TClassSorter }

Constructor TClassSorter.Create(WithInterfaces: Boolean);
Var
  it: TItem;
  r: PRoot;
Begin
  Inherited Create;
  fRoots := Nil;
  fWithInterfaces := WithInterfaces;
  If WithInterfaces Then Begin
    // Im mode Cobra wäre das Falsch, aber ein "leeres" Node sieht auch doof aus ..
    it.parent := '';
    it.Name := 'IUnknown';
    it.Line := '';
    it.FileName := '';
    it.FileIndex := -1;
    new(r);
    r^.RootElement := it;
    r^.Childs := Nil;
    setlength(fRoots, 2);
    fRoots[0] := r;
  End
  Else Begin
    setlength(fRoots, 1);
  End;
  it.parent := '';
  it.Name := 'TObject';
  it.Line := '';
  it.FileName := '';
  it.FileIndex := -1;
  new(r);
  r^.RootElement := it;
  r^.Childs := Nil;
  fRoots[high(fRoots)] := r;
End;

Destructor TClassSorter.Destroy;
Var
  i: Integer;
Begin
  For i := 0 To high(fRoots) Do Begin
    FreeRoot(fRoots[i]);
  End;
End;

Procedure TClassSorter.FreeRoot(Var Root: PRoot);
Var
  i: Integer;
Begin
  For i := 0 To high(Root^.Childs) Do Begin
    FreeRoot(Root^.Childs[i]);
  End;
  Dispose(root);
End;

Procedure TClassSorter.SortChilds(Var Root: PRoot);

  Procedure Quick(li, re: integer);
  Var
    l, r: Integer;
    p: String;
    h: PRoot;
  Begin
    If Li < Re Then Begin
      p := lowercase(root^.Childs[Trunc((li + re) / 2)]^.RootElement.Name); // Auslesen des Pivo Elementes
      l := Li;
      r := re;
      While l < r Do Begin
        While CompareStr(lowercase(root^.Childs[l]^.RootElement.Name), p) < 0 Do
          inc(l);
        While CompareStr(lowercase(root^.Childs[r]^.RootElement.Name), p) > 0 Do
          dec(r);
        If L <= R Then Begin
          h := root^.Childs[l];
          root^.Childs[l] := root^.Childs[r];
          root^.Childs[r] := h;
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
  If assigned(root^.Childs) Then Begin
    Quick(0, high(root^.Childs));
  End;
  For i := 0 To high(Root^.Childs) Do Begin
    SortChilds(Root^.Childs[i]);
  End;
End;

Function TClassSorter.InsertRootEle(Const Element: PRoot): Boolean;

  Function InsertRootElement(Root: PRoot): Boolean;
  Var
    i: Integer;
    n, p: String;
  Begin
    result := false;
    // Der (..) Teil der das Interface angibt muss hier abgeschnitten werden.
    n := lowercase(Root^.RootElement.Name);
    p := lowercase(Element^.RootElement.parent);
    If pos('(', n) <> 0 Then Begin
      delete(n, pos('(', n), length(n));
      n := trim(n);
    End;
    If pos('(', p) <> 0 Then Begin
      delete(p, pos('(', p), length(p));
      p := trim(p);
    End;
    If n = p Then Begin
      setlength(root^.Childs, high(root^.Childs) + 2);
      root^.Childs[high(root^.Childs)] := Element;
      result := true;
    End
    Else Begin
      For i := 0 To high(root^.Childs) Do Begin
        If InsertRootElement(root^.Childs[i]) Then exit(true);
      End;
    End;
  End;

Var
  i: Integer;
Begin
  result := false;
  For i := 0 To high(fRoots) Do Begin
    If InsertRootElement(fRoots[i]) Then exit(true);
  End;
End;

Procedure TClassSorter.AddItem(Const Item: TItem);
Var
  r: PRoot;
Begin
  // Finish Richtet das dann ;)
  new(r);
  r^.RootElement := Item;
  r^.Childs := Nil;
  setlength(fRoots, high(fRoots) + 2);
  fRoots[high(fRoots)] := r;
End;

Procedure TClassSorter.Finish;
Var
  i: Integer;
  it, Ele: PRoot;
Begin
  For i := high(fRoots) Downto ifthen(fWithInterfaces, 2, 1) Do Begin
    ele := fRoots[i];
    setlength(fRoots, i);
    // Das Parent von Ele ist nicht im Baum
    If Not InsertRootEle(Ele) Then Begin
      new(it);
      it^.RootElement.parent := 'TObject';
      it^.RootElement.Name := ele^.RootElement.parent;
      it^.RootElement.FileName := '';
      it^.RootElement.Line := '';
      it^.RootElement.FileIndex := -1;
      it^.Childs := Nil;
      If Not InsertRootEle(it) Then Begin
        Raise Exception.Create('Logic error in TClassSorter.Finish(1)');
      End;
      If Not InsertRootEle(ele) Then Begin
        Raise Exception.Create('Logic error in TClassSorter.Finish(2)');
      End;
    End;
  End;
  (*
   * Ganz Am Ende sortieren wir die Kinder Klassen Alphabetisch ;)
   *)
  SortChilds(fRoots[0]);
  If fWithInterfaces Then Begin
    SortChilds(fRoots[1]);
  End;
End;

{ TForm3 }

Procedure TForm3.FormCreate(Sender: TObject);
Begin
  caption := 'Class info';
  button1.Align := alBottom;
  LazVirtualStringTree1.Align := alClient;
  CSV_export := TStringList.Create;
End;

Procedure TForm3.FormDestroy(Sender: TObject);
Begin
  CSV_export.free;
End;

Procedure TForm3.LazVirtualStringTree1DblClick(Sender: TObject);
Var
  n: PVirtualNode;
  aData: PNodeData;
Begin
  // Open Code
  n := LazVirtualStringTree1.GetFirstSelected();
  If assigned(n) Then Begin
    aData := LazVirtualStringTree1.GetNodeData(n);
    If length(adata^.Columns) >= 3 Then Begin
      If adata^.Columns[1] <> '' Then Begin
        //adata^.Columns[0] := Root^.RootElement.Name;
        //adata^.Columns[1] := Root^.RootElement.FileName;
        //adata^.Columns[2] := Root^.RootElement.Line;
        Form13.OpenFile(fProject, adata^.Columns[1], strtointdef(adata^.Columns[2], -1));
      End;
    End;
  End;
End;

Procedure TForm3.LazVirtualStringTree1FreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
Var
  Data: PNodeData;
Begin
  Data := LazVirtualStringTree1.GetNodeData(Node);
  If Assigned(Data) Then Begin
    setlength(data^.Columns, 0);
  End;
End;

Procedure TForm3.LazVirtualStringTree1GetNodeDataSize(Sender: TBaseVirtualTree;
  Var NodeDataSize: Integer);
Begin
  NodeDataSize := SizeOf(TNodeData);
End;

Procedure TForm3.LazVirtualStringTree1GetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  Var CellText: String);
Var
  Data: PNodeData;
Begin
  CellText := '';
  Data := LazVirtualStringTree1.GetNodeData(Node);
  If (Column >= 0) And (Column <= high(data^.Columns)) Then Begin
    CellText := Data^.Columns[Column];
  End;
End;

Procedure TForm3.MenuItem1Click(Sender: TObject);
Begin
  // Export as CSV
  If SaveDialog1.Execute Then Begin
    CSV_export.SaveToFile(SaveDialog1.FileName);
  End;
End;

Function TForm3.LoadClasses(aList: TProjectFilesInfo; Project: TProject
  ): Boolean;
Var
  ClassCount: integer;
  InterfaceCount: integer;

  Function Traverse(Root: PRoot; aRoot: PVirtualNode): PVirtualNode;
  Var
    i: Integer;
    aData: PNodeData;
  Begin
    result := LazVirtualStringTree1.AddChild(aRoot);
    aData := LazVirtualStringTree1.GetNodeData(result);
    If Root^.RootElement.FileName <> '' Then Begin
      CSV_export.Add(Format('%s;%s;%s;%s', [Root^.RootElement.Name, Root^.RootElement.parent, Root^.RootElement.FileName, Root^.RootElement.Line]));
    End;
    aData^.FileIndex := root^.RootElement.FileIndex;
    setlength(adata^.Columns, 3);
    adata^.Columns[0] := Root^.RootElement.Name;
    adata^.Columns[1] := Root^.RootElement.FileName;
    adata^.Columns[2] := Root^.RootElement.Line;

    If (Root^.RootElement.FileName <> '') Then Begin
      If (Root^.RootElement.IsInterface) Then Begin
        inc(InterfaceCount);
      End
      Else Begin
        inc(ClassCount);
      End;
    End;

    For i := 0 To high(Root^.Childs) Do Begin
      Traverse(Root^.Childs[i], result);
    End;
  End;

Var
  CS: TClassSorter;
  i, j, k, l, m: Integer;
  aRoot, aroot2: PVirtualNode;
  it: TItem;
  interfaces: String;
  WithInterfaces: Boolean;
Begin
  fProject := Project;
  result := false;
  If high(aList) = -1 Then exit;
  fList := aList;

  WithInterfaces := false;
  For i := 0 To high(aList) Do Begin
    For j := 0 To high(aList[i].FileInfo.aClasses) Do Begin
      If aList[i].FileInfo.aClasses[j].isInterface Then Begin
        WithInterfaces := true;
        break;
      End;
    End;
    If WithInterfaces Then break;
  End;
  cs := TClassSorter.Create(WithInterfaces);
  For i := 0 To high(aList) Do Begin
    For j := 0 To high(aList[i].FileInfo.aClasses) Do Begin
      it.FileName := aList[i].Filename;
      it.Line := inttostr(aList[i].FileInfo.aClasses[j].LineInFile);
      it.Name := aList[i].FileInfo.aClasses[j].Name;
      it.FileIndex := i;
      it.IsInterface := aList[i].FileInfo.aClasses[j].isInterface;
      If aList[i].FileInfo.aClasses[j].isInterface Then Begin
        it.parent := 'IUnknown';
      End
      Else Begin
        it.parent := 'TObject';
      End;
      If assigned(aList[i].FileInfo.aClasses[j].Parents) Then Begin
        interfaces := '';
        // Wir suchen aus der Parent Liste die Echte Klasse heraus und machen die Interfaces zu einer Kommaliste
        // Hat die Klasse nur Interfaces als Parent, dann bleibt TObject das Parent ;)
        For k := 0 To high(aList[I].FileInfo.aClasses[j].Parents) Do Begin
          For m := 0 To high(aList) Do Begin
            For l := 0 To high(aList[m].FileInfo.aClasses) Do Begin
              If (aList[m].FileInfo.aClasses[l].Name = aList[i].FileInfo.aClasses[j].Parents[k]) Then Begin
                If aList[m].FileInfo.aClasses[l].isInterface Then Begin
                  If interfaces <> '' Then interfaces := interfaces + ', ';
                  interfaces := interfaces + aList[m].FileInfo.aClasses[l].Name;
                End
                Else Begin
                  it.parent := aList[i].FileInfo.aClasses[j].Parents[k];
                End;
              End;
            End;
          End;
        End;
        If interfaces <> '' Then Begin
          it.Name := it.Name + ' (' + interfaces + ')';
        End;
      End;
      cs.AddItem(it);
    End;
  End;
  cs.Finish(); // Sortiert die Klassen Korrekt und fügt ggf Dummy abhängigkeiten ein so dass ein Zusammenhängender Baum entsteht.
  (*
   * Die Einträge dürfen leider nicht sortiert werden
   * Da sonst Kinder vor Eltern in den  LazVirtualStringTree1 geadded werden würden !
   *)
  ClassCount := 0;
  InterfaceCount := 0;
  LazVirtualStringTree1.Clear;
  CSV_export.Clear;
  CSV_export.Add('Class;Parent;File;Line');
  aroot := Traverse(cs.fRoots[0], Nil);
  If WithInterfaces Then Begin
    aroot2 := Traverse(cs.fRoots[1], Nil);
  End;
  cs.free;
  LazVirtualStringTree1.FullExpand(aRoot);
  If WithInterfaces Then Begin
    LazVirtualStringTree1.FullExpand(aroot2);
  End;
  If WithInterfaces Then Begin
    StatusBar1.Panels[0].Text := format('%d interfaces, %d classes', [InterfaceCount, ClassCount]);
  End
  Else Begin
    StatusBar1.Panels[0].Text := format('%d classes', [ClassCount]);
  End;
  result := true;
End;

Procedure TForm3.Button1Click(Sender: TObject);
Begin
  close;
End;

End.

