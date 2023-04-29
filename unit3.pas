Unit Unit3;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  Menus, laz.VirtualTrees, ufpc_understand;

Type

  TNodeData = Record
    Columns: Array Of String;
  End;

  PNodeData = ^TNodeData;

  TItem = Record
    parent: String;
    Name: String;
    FileName: String;
    Line: String;
  End;

  { TForm3 }

  TForm3 = Class(TForm)
    Button1: TButton;
    LazVirtualStringTree1: TLazVirtualStringTree;
    MenuItem1: TMenuItem;
    PopupMenu1: TPopupMenu;
    SaveDialog1: TSaveDialog;
    StatusBar1: TStatusBar;
    Procedure Button1Click(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure FormDestroy(Sender: TObject);
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
  public
    Function LoadClasses(aList: TProjectFilesInfo): Boolean;
  End;

Var
  Form3: TForm3;

Implementation

{$R *.lfm}

Type
  PRoot = ^TRoot;

  TRoot = Record
    RootElement: TItem;
    Childs: Array Of PRoot;
  End;

  { TClassSorter }

  TClassSorter = Class
  private
    //fItems: TItemList;
    fRoots: Array Of PRoot;
    Procedure FreeRoot(Var Root: PRoot);
    Procedure SortChilds(Var Root: PRoot);
    Function InsertRootEle(Const Element: PRoot): Boolean;
  public
    Constructor Create(); virtual;
    Destructor Destroy(); override;
    Procedure AddItem(Const Item: TItem);
    Procedure Finish();
  End;

  { TClassSorter }

Constructor TClassSorter.Create;
Var
  it: TItem;
  r: PRoot;
Begin
  Inherited Create;
  fRoots := Nil;
  it.parent := '';
  it.Name := 'TObject';
  it.Line := '';
  it.FileName := '';
  new(r);
  r^.RootElement := it;
  r^.Childs := Nil;
  setlength(fRoots, 1);
  fRoots[0] := r;
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
  Begin
    result := false;
    If lowercase(Root^.RootElement.Name) = lowercase(Element^.RootElement.parent) Then Begin
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
  For i := high(fRoots) Downto 1 Do Begin
    ele := fRoots[i];
    setlength(fRoots, i);
    // Das Parent von Ele ist nicht im Baum
    If Not InsertRootEle(Ele) Then Begin
      new(it);
      it^.RootElement.parent := 'TObject';
      it^.RootElement.Name := ele^.RootElement.parent;
      it^.RootElement.FileName := '';
      it^.RootElement.Line := '';
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
   * Ganz Am Ende sortieren wir die KlinderKlassen Alphabetisch ;)
   *)
  SortChilds(fRoots[0]);
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

Function TForm3.LoadClasses(aList: TProjectFilesInfo
  ): Boolean;
Var
  ClassCount: integer;

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
    setlength(adata^.Columns, 3);
    adata^.Columns[0] := Root^.RootElement.Name;
    adata^.Columns[1] := Root^.RootElement.FileName;
    adata^.Columns[2] := Root^.RootElement.Line;

    If Root^.RootElement.FileName <> '' Then inc(ClassCount);
    For i := 0 To high(Root^.Childs) Do Begin
      Traverse(Root^.Childs[i], result);
    End;
  End;

Var
  CS: TClassSorter;
  i, j: Integer;
  aRoot: PVirtualNode;
  it: TItem;
Begin
  result := false;
  If high(aList) = -1 Then exit;
  cs := TClassSorter.Create();
  For i := 0 To high(aList) Do Begin
    For j := 0 To high(aList[i].FileInfo.aClasses) Do Begin
      it.FileName := aList[i].Filename;
      it.Line := inttostr(aList[i].FileInfo.aClasses[j].LineInFile);
      it.Name := aList[i].FileInfo.aClasses[j].Name;
      If assigned(aList[i].FileInfo.aClasses[j].Parents) Then Begin
        it.parent := aList[i].FileInfo.aClasses[j].Parents[0];
        // Wie gehen wir mit Interfaces um die ja mehrere Parents sind, keine Ahnung, aber so kriegt der User es wenigstens mit.
        If length(aList[i].FileInfo.aClasses[j].Parents) > 1 Then Begin
          ShowMessage('Warning: ' + aList[i].FileInfo.aClasses[j].Name + ' has multiple "Parents" visualization may fail.');
        End;
      End
      Else Begin
        it.parent := 'TObject';
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
  LazVirtualStringTree1.Clear;
  CSV_export.Clear;
  CSV_export.Add('Class;Parent;File;Line');
  aroot := Traverse(cs.fRoots[0], Nil);
  cs.free;
  LazVirtualStringTree1.FullExpand(aRoot);
  StatusBar1.Panels[0].Text := format('%d classes', [ClassCount]);
  result := true;
End;

Procedure TForm3.Button1Click(Sender: TObject);
Begin
  close;
End;

End.

