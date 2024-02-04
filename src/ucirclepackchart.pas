(******************************************************************************)
(* uCirclePackChart                                                08.08.2023 *)
(*                                                                            *)
(* Version     : 0.03                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : Realises a circlepacking component                           *)
(*                                                                            *)
(*               Inspired by https://github.com/pmenzel/packCircles           *)
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
(* History     : 0.01 - Initial version                                       *)
(*               0.02 - Add: NeedReEvaluation                                 *)
(*                      FIX: OnAfterPaint missing, Memleak                    *)
(*               0.03 - switch value from integer to single                   *)
(*                                                                            *)
(******************************************************************************)
Unit ucirclepackchart;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Controls, Graphics;

Const
  CirclePackFileVersoin: integer = 1;

Type

  TFloatPoint = Record
    x, y: Single;
  End;

  TColorSet = Record
    BrushColor: TColor;
    PenColor: TColor;
    PenWitdh: integer;
    FontColor: TColor;
  End;

  PCircle = ^TCircle;

  TCircle = Record
    // Everything that a user is allowed to access to
    Caption: String;
    Color: TColorSet;
    SelectedColor: TColorSet;
    Value: Single; // The Value that determines the Diameter of the circle
    UserData: Pointer;
    Selected: Boolean; // If True, then Selected Brush / Selected will be used (will not be stored!)

    // Everything that the user is not allowed to access (can and will be overwriten by TPackedCircleChart)
    Center: TFloatPoint; // Location where the circle should be if unscaled
    ScaledRadius: Single; // Scalled radius of the Circle Proportional to Value
    ScaledCenter: TFloatPoint; // Scaled Location where to render the Circle

    (*
     * During calculation of the circles positions the outmost circle of circles is stored in the sibling pointers
     *)
    PrevSibling: PCircle; // Pointer to previos Sibling (nil if first child)
    NextSibling: PCircle; // Pointer to next Sibling
    Next: PCircle; // Pointers to every "next" created circle -> will form a list of all circles in the order they where created
  End;

  TCircles = Array Of TCircle;

  TOnSaveUserData = Procedure(Sender: TObject; Const Stream: TStream; aUserData: Pointer) Of Object;
  TOnLoadUserData = Function(Sender: TObject; Const Stream: TStream): Pointer Of Object;
  TOnDeleteCirclesUserData = Procedure(Sender: TObject; aUserData: Pointer) Of Object;
  TOnCirclePaint = Procedure(Sender: TObject; Const aElement: PCircle) Of Object;

  { TPackedCircleChart }

  TPackedCircleChart = Class(TGraphicControl)
  private
    fbb_topright, fbb_bottomleft: TFloatPoint;
    fFirstCircle: PCircle; // Einsprung in das 1. Element
    fLastCircle: PCircle;
    FIterator: PCircle;
    fChanged: Boolean;
    FCircleCount: Integer;
    fNeedRecalculation: Boolean; // True nach mindestens einem Add / Delete
    fOnAfterCirclePaint: TOnCirclePaint;
    fOnAfterPaint: TNotifyEvent;
    fOnBeforeCirclePaint: TOnCirclePaint;
    fOnDeleteCirclesUserData: TOnDeleteCirclesUserData;
    fOnLoadUserData: TOnLoadUserData;
    fOnSaveUserData: TOnSaveUserData;

    Procedure ReCalculateIfNeeded;

    Procedure CalcCirclesPositionsFromValue;
    Procedure ScaleCirclesPositionsToClientRect; // Rechnet ScaledCenter und Radius entsprechend um, so dass alle Kreice in ClientRect passen (nur nach CalcCirclesPositionsFromValue aufrufen !)
    Procedure FreeUserData(Const aCircle: PCircle);
  protected
    Procedure Paint; override;
    Procedure DoOnResize; override; // call OnResize
  public
    Property Changed: Boolean read fChanged;
    Property CircleCount: Integer read FCircleCount;

    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy(); override;

    Function LoadFromStream(Const Stream: TStream): Boolean;
    Function LoadFromFile(aFilename: String): Boolean;
    Function SaveToStream(Const Stream: TStream): Boolean;
    Function SaveToFile(aFilename: String): Boolean;

    Function AddCircle(Const aCircle: TCircle): boolean;
    Function DelCircle(Const aCircle: PCircle): Boolean;

    Procedure Clear;

    Procedure DeselectAll; // Iterates through all Childs and "desectes" them but without changing the iterator values !

    // Gives the Circle which collides with x,y
    // false if none exist
    // Returns a pointer to give write access to the circle
    Function GetCircleAtPos(x, y: integer; Out aElement: PCircle): Boolean;

    (*
     * Iterator Pattern to walk all Circles
     *)
    Function Iterator: PCircle;
    Function IterFirst: PCircle;
    Function IterNext: PCircle;

    Procedure NeedReEvaluation; // Call this routine if you manually changed the value of a already included cirlce
  published
    Property Color;
    Property ShowHint;

    Property OnAfterCirclePaint: TOnCirclePaint read fOnAfterCirclePaint write fOnAfterCirclePaint;
    Property OnAfterPaint: TNotifyEvent read fOnAfterPaint write fOnAfterPaint;
    Property OnBeforeCircletPaint: TOnCirclePaint read fOnBeforeCirclePaint write fOnBeforeCirclePaint;
    Property OnClick;
    Property OnDeleteCirclesUserData: TOnDeleteCirclesUserData read fOnDeleteCirclesUserData write fOnDeleteCirclesUserData;
    Property OnDblClick;
    Property OnLoadUserData: TOnLoadUserData read fOnLoadUserData write fOnLoadUserData;
    Property OnMouseUp;
    Property OnMouseMove;
    Property OnMouseDown;
    Property OnSaveUserData: TOnSaveUserData read fOnSaveUserData write fOnSaveUserData;
    Property OnShowHint;
    Property OnResize;
  End;

Function DefaultCircle(): TCircle;

Implementation

Uses math;

Const
  Epsilon = 1E-6;

Function FloatPoint(x, y: Single): TFloatPoint;
Begin
  result.x := x;
  result.y := y;
End;

Function DefaultCircle(): TCircle;
Begin
  result.Caption := '';
  result.Color.BrushColor := clWhite;
  result.Color.PenColor := clBlack;
  result.Color.PenWitdh := 1;
  result.Color.FontColor := clBlack;

  result.SelectedColor.BrushColor := clWhite;
  result.SelectedColor.PenColor := clred;
  result.SelectedColor.PenWitdh := 1;
  result.SelectedColor.FontColor := clBlack;

  result.Value := 1;
  result.Center := FloatPoint(0, 0);
  result.UserData := Nil;
  result.Selected := false;

  result.ScaledRadius := -1;
  result.ScaledCenter := FloatPoint(0, 0);
  result.PrevSibling := Nil;
  result.NextSibling := Nil;
  result.Next := Nil;
End;

Function clamp(aValue, aMin, aMax: Single): Single;
Begin
  If aValue < aMin Then Begin
    result := aMin;
  End
  Else Begin
    If aValue > aMax Then Begin
      result := aMax;
    End
    Else Begin
      result := aValue;
    End;
  End;
End;

Operator + (a, b: TFloatPoint): TFloatPoint;
Begin
  result.x := a.x + b.x;
  result.y := a.y + b.y;
End;

{ TPackedCircleChart }

Constructor TPackedCircleChart.Create(AOwner: TComponent);
Begin
  Inherited Create(AOwner);
  Color := clWhite;
  fChanged := false;
  fNeedRecalculation := false;
  fOnAfterCirclePaint := Nil;
  fOnAfterPaint := Nil;
  fOnBeforeCirclePaint := Nil;
  fOnDeleteCirclesUserData := Nil;
  fOnLoadUserData := Nil;
  fOnSaveUserData := Nil;
  fFirstCircle := Nil;
  Clear;
End;

Destructor TPackedCircleChart.Destroy;
Begin
  Clear;
  Inherited Destroy;
End;

Procedure TPackedCircleChart.Clear;
Var
  i, j: PCircle;
Begin
  i := fFirstCircle;
  While assigned(i) Do Begin
    j := i;
    i := i^.Next;
    FreeUserData(j);
    Dispose(j);
  End;
  FCircleCount := 0;
  fFirstCircle := Nil;
  fLastCircle := Nil;
  FIterator := Nil;
  fNeedRecalculation := false;
  Invalidate;
End;

Function TPackedCircleChart.LoadFromStream(Const Stream: TStream): Boolean;
Var
  LoadedFileVersion: integer;
  j, i: integer;
  c: TCircle;
  b: UInt8;
Begin
  result := false;
  Clear;
  LoadedFileVersion := 0;
  stream.Read(LoadedFileVersion, sizeof(LoadedFileVersion));
  If (LoadedFileVersion > CirclePackFileVersoin) Or (LoadedFileVersion <= 0) Then exit; // Da ist offensichtlich was falsch ..
  j := 0;
  stream.Read(j, SizeOf(j));
  For i := 0 To j - 1 Do Begin
    c := DefaultCircle();
    c.Caption := stream.ReadAnsiString;
    stream.Read(c.Color, SizeOf(c.Color));
    stream.Read(c.SelectedColor, SizeOf(c.SelectedColor));
    stream.Read(c.Value, SizeOf(c.Value));
    b := 2;
    stream.Read(b, sizeof(b));
    If b = 1 Then Begin
      If Not assigned(OnLoadUserData) Then Begin
        Raise exception.create('Error, stream with defined userdata, but no OnLoadUserData property set.');
        exit;
      End;
      c.UserData := OnLoadUserData(self, Stream);
    End;
    AddCircle(c);
  End;
  result := true;
End;

Function TPackedCircleChart.LoadFromFile(aFilename: String): Boolean;
Var
  fs: TFileStream;
Begin
  result := false;
  Try
    fs := TFileStream.Create(aFilename, fmOpenRead);
    result := LoadFromStream(fs);
    fs.free;
    Invalidate;
  Except
    Raise;
    // Nichts, das Result ist ja schon false..
  End;
End;

Function TPackedCircleChart.SaveToStream(Const Stream: TStream): Boolean;
Var
  i: PCircle;
  b: UInt8;
Begin
  result := false;
  // Global Settings
  stream.Write(CirclePackFileVersoin, sizeof(CirclePackFileVersoin));

  stream.Write(FCircleCount, sizeof(FCircleCount));
  i := fFirstCircle;
  While assigned(i) Do Begin
    stream.WriteAnsiString(i^.Caption);
    stream.Write(i^.Color, SizeOf(i^.Color));
    stream.Write(i^.SelectedColor, SizeOf(i^.SelectedColor));
    stream.Write(i^.Value, SizeOf(i^.Value));
    If assigned(i^.UserData) Then Begin
      b := 1;
      stream.Write(b, SizeOf(b));
      If Not assigned(OnSaveUserData) Then Begin
        Raise exception.create('Error, TCircle with defined userdata, but no OnSaveUserData property set.');
        exit;
      End;
      OnSaveUserData(self, Stream, i^.UserData);
    End
    Else Begin
      b := 0;
      stream.Write(b, SizeOf(b));
    End;
    // TODO: Include more fields..
    i := i^.Next;
  End;
  result := true;
End;

Function TPackedCircleChart.SaveToFile(aFilename: String): Boolean;
Var
  fs: TFileStream;
Begin
  result := false;
  Try
    fs := TFileStream.Create(aFilename, fmCreate Or fmOpenWrite);
    result := SaveToStream(fs);
    fs.free;
  Except
    Raise;
    // Nichts, das Result ist ja schon false..
  End;
End;

Function TPackedCircleChart.AddCircle(Const aCircle: TCircle): boolean;
Var
  p: PCircle;
Begin
  result := false;
  If assigned(fFirstCircle) Then Begin
    new(p);
    p^ := aCircle;
    p^.Next := Nil;
    fLastCircle^.Next := p;
    fLastCircle := p;
  End
  Else Begin
    New(fFirstCircle);
    fFirstCircle^ := aCircle;
    fFirstCircle^.Next := Nil; // Egal wie das Ein / Aushängen in die Lineare liste stellen wir hier sicher sonst knallts ggf.
    fLastCircle := fFirstCircle;
  End;
  inc(FCircleCount);
  fNeedRecalculation := true;
  result := true;
End;

Function TPackedCircleChart.DelCircle(Const aCircle: PCircle): Boolean;
Var
  i: PCircle;
Begin
  result := false;
  If fFirstCircle = aCircle Then Begin
    fFirstCircle := fFirstCircle^.Next;
    FreeUserData(aCircle);
    Dispose(aCircle);
    dec(FCircleCount);
    fNeedRecalculation := true;
    result := true;
  End
  Else Begin
    i := fFirstCircle;
    While assigned(i) And (i^.Next <> aCircle) Do Begin
      i := i^.Next;
    End;
    If assigned(i) And (i^.Next = aCircle) Then Begin
      If aCircle = fLastCircle Then Begin
        fLastCircle := i;
        fLastCircle^.Next := Nil;
        FreeUserData(aCircle);
        Dispose(aCircle);
        dec(FCircleCount);
        fNeedRecalculation := true;
        result := true;
      End
      Else Begin
        i^.Next := aCircle^.Next; // Aushängen von ACircle
        FreeUserData(aCircle);
        Dispose(aCircle);
        dec(FCircleCount);
        fNeedRecalculation := true;
        result := true;
      End;
    End;
  End;
End;

Procedure TPackedCircleChart.ReCalculateIfNeeded;
Begin
  If fNeedRecalculation Then Begin
    CalcCirclesPositionsFromValue;
    ScaleCirclesPositionsToClientRect
  End;
  fNeedRecalculation := false;
End;

Procedure TPackedCircleChart.CalcCirclesPositionsFromValue;
  Procedure bound(Const n: PCircle); // Update the Global bounding box of all circles
  Begin
    fbb_bottomleft.x := MIN(n^.center.x - abs(n^.Value), fbb_bottomleft.x);
    fbb_bottomleft.y := MIN(n^.center.y - abs(n^.Value), fbb_bottomleft.y);
    fbb_topright.x := MAX(n^.center.x + abs(n^.Value), fbb_topright.x);
    fbb_topright.y := MAX(n^.center.y + abs(n^.Value), fbb_topright.y);
  End;

  Procedure place(Const a, b, c: PCircle); // Place C near b, c
  Var
    da, db, dx, dy, dc, cos_, theta, x, h: Double;
  Begin
    da := abs(b^.Value) + abs(c^.Value);
    db := abs(a^.Value) + abs(c^.Value);
    dx := b^.center.x - a^.center.x;
    dy := b^.center.y - a^.center.y;
    dc := sqrt(dx * dx + dy * dy);
    If (dc > 0.0) Then Begin
      cos_ := (db * db + dc * dc - da * da) / (2 * db * dc);
      cos_ := clamp(cos_, -1, 1);
      theta := ArcCos(cos_);
      x := cos_ * db;
      h := sin(theta) * db;
      dx := dx / dc;
      dy := dy / dc;
      c^.center.x := a^.center.x + x * dx + h * dy;
      c^.center.y := a^.center.y + x * dy - h * dx;
    End
    Else Begin
      c^.center.x := a^.center.x + db;
      c^.center.y := a^.center.y;
    End;
  End;

  Function distance(a: PCircle): Double;
  Begin
    result := sqrt(sqr(a^.Center.x) + sqr(a^.Center.y));
  End;

  Function intersects(a, b: PCircle): boolean; // True if a intersects b
  Var
    dx, dy, dr: Double;
  Begin
    dx := b^.Center.x - a^.Center.x;
    dy := b^.Center.y - a^.Center.y;
    dr := abs(a^.Value) + abs(b^.Value);
    If (sqr(dr) - epsilon > sqr(dx) + sqr(dy)) Then // overlap is bigger than epsilon
      result := true
    Else
      result := false;
  End;

  Procedure splice(a, b: PCircle);
  Begin
    a^.NextSibling := b;
    b^.PrevSibling := a;
  End;

  Procedure insert(a, b: PCircle);
  Var
    c: PCircle;
  Begin
    c := a^.NextSibling;
    a^.NextSibling := b;
    b^.PrevSibling := a;
    b^.NextSibling := c;
    If assigned(c) Then c^.PrevSibling := b;
  End;

Var
  j, k, a, b, c, n, nearestnode: PCircle;
  isect, skip: Boolean;
  sj, sk, nearestdist, dist_n: Double;
Begin
  (*
   * Berechnet alle Positionen aller Kreise aufgrund ihrer "Values" neu
   *)
  // Reset Bounding Box
  fbb_topright := FloatPoint(0, 0);
  fbb_bottomleft := FloatPoint(0, 0);

  (* Create first circle. *)
  a := fFirstCircle;
  b := Nil;
  c := Nil;

  a^.center.x := -1 * abs(a^.Value);
  bound(a);

  (* Create second circle. *)
  If Not assigned(a^.Next) Then Begin
    a^.NextSibling := Nil;
    a^.PrevSibling := Nil;
    exit;
  End;
  b := a^.Next;
  b^.Center.x := abs(b^.Value);
  b^.Center.y := 0;
  bound(b);

  (* Create third circle. *)
  If Not assigned(b^.Next) Then Begin
    a^.NextSibling := b;
    a^.PrevSibling := b;
    b^.NextSibling := a;
    b^.PrevSibling := a;
    exit;
  End;
  c := b^.Next;
  place(a, b, c);
  bound(c);

  // make initial chain of a <-> b <-> c
  a^.NextSibling := c;
  a^.PrevSibling := b;
  b^.NextSibling := a;
  b^.PrevSibling := c;
  c^.NextSibling := b;
  c^.PrevSibling := a;

  If Not assigned(c^.Next) Then Begin
    exit;
  End;

  b := c;

  (* add remaining nodes *)
  skip := false;
  c := c^.Next;
  While assigned(c) Do Begin
    // Determine the node a in the chain, which is nearest to the center
    // The new node c will be placed NextSibling to a (unless overlap occurs)
    // NB: This search is only done the first time for each new node, i.e.
    // not again after splicing
    If (Not skip) Then Begin
      n := a;
      nearestnode := n;
      nearestdist := MaxSingle;
      Repeat
        dist_n := distance(n);
        If (dist_n < nearestdist) Then Begin
          nearestdist := dist_n;
          nearestnode := n;
        End;
        n := n^.NextSibling;
      Until (n = a);
      a := nearestnode;
      b := nearestnode^.NextSibling;
    End;

    (* a corresponds to C_m, and b corresponds to C_n in the paper *)
    place(a, b, c);

    (* for debugging: initial placement of c that may ovelap *)
    isect := false;
    j := b^.NextSibling;
    k := a^.PrevSibling;
    sj := abs(b^.Value);
    sk := abs(a^.Value);
    //j = b.NextSibling, k = a.previous, sj = b._.r, sk = a._.r;
    Repeat
      If (sj <= sk) Then Begin
        If (intersects(j, c)) Then Begin
          splice(a, j);
          b := j;
          skip := true;
          isect := true;
          break;
        End;
        sj := sj + abs(j^.Value);
        j := j^.NextSibling;
      End
      Else Begin
        If (intersects(k, c)) Then Begin
          splice(k, b);
          a := k;
          skip := true;
          isect := true;
          break;
        End;
        sk := sk + abs(k^.Value);
        k := k^.PrevSibling;
      End;
    Until (j = k^.NextSibling);

    If (Not isect) Then Begin
      (* Update node chain. *)
      insert(a, c);
      b := c;
      bound(c);
      skip := false;
      c := c^.Next;
    End;
  End;
End;

Procedure TPackedCircleChart.ScaleCirclesPositionsToClientRect;
Var
  DimX, Dimy: Integer;
  maxx, maxy, minx, miny, cx, cy, aScalex, aScaley, aScale: Single;
  i: PCircle;
Begin
  If Not assigned(fFirstCircle) Then exit;
  maxx := fbb_topright.x;
  maxy := fbb_topright.y;
  minx := fbb_bottomleft.x;
  miny := fbb_bottomleft.y;
  // The Unscaled dimension
  DimX := ceil(maxx - minx);
  Dimy := ceil(maxy - miny);
  // The unscaled Center
  cx := (maxx + minx) / 2;
  cy := (maxy + miny) / 2;
  // All Radii and positions need now to be scaled in that way, that dimx and dimy will fit width / height
  aScalex := Width / (DimX);
  aScaley := Height / (Dimy);
  aScale := min(ascalex, ascaley); // We use the same scaling in x- / y-Direction
  cx := round(cx * aScale);
  cy := round(cy * aScale);
  i := fFirstCircle;
  While assigned(i) Do Begin
    // First scale all to (0,0)
    i^.ScaledCenter := FloatPoint(aScale * i^.Center.x, aScale * i^.Center.y);
    // Second translate to fit into Window
    i^.ScaledCenter := i^.ScaledCenter + FloatPoint(width / 2 - cx, height / 2 - cy);
    //  // Last Scale down the radius
    i^.ScaledRadius := aScale * abs(i^.Value);
    i := i^.Next;
  End;
End;

Procedure TPackedCircleChart.FreeUserData(Const aCircle: PCircle);
Begin
  If assigned(aCircle^.UserData) Then Begin
    If Assigned(fOnDeleteCirclesUserData) Then Begin
      OnDeleteCirclesUserData(self, aCircle^.UserData);
    End
    Else Begin
      Raise exception.create('Error, freeing PCircle with userdata, but no OnDeleteCirclesUserData set.');
    End;
  End;
End;

Procedure TPackedCircleChart.Paint;

  Procedure PlotTextAtPos(Const p: Tpoint; Const aText: String);
  Var
    Lines: TStringArray;
    t, i: Integer;
  {$IFDEF Windows}
    s: String;
  {$ENDIF}
  Begin
    If pos(LineEnding, aText) <> 0 Then Begin
{$IFDEF Windows}
      s := aText;
      s := StringReplace(s, #10, '', [rfReplaceAll]);
      lines := s.Split([#13]);
{$ELSE}
      lines := aText.Split([LineEnding]);
{$ENDIF}
      t := p.y - (canvas.TextHeight('8') * length(Lines)) Div 2;
      // TODO: This implements a "center" layout, in theory you could provide left and right as well..
      For i := 0 To high(lines) Do Begin
        canvas.TextOut(
          p.x - (Canvas.TextWidth(lines[i]) Div 2),
          t + i * Canvas.TextHeight('8'),
          lines[i]
          );
      End;
    End
    Else Begin
      canvas.TextOut(
        p.x - Canvas.TextWidth(aText) Div 2,
        p.Y - Canvas.TextHeight(aText) Div 2,
        aText
        );
    End;
  End;

Var
  i: PCircle;
Begin
  Inherited Paint;
  ReCalculateIfNeeded;
  // Clear background
  canvas.Brush.Color := Color;
  canvas.Pen.Color := Color;
  canvas.Rectangle(0, 0, Width, Height);
  i := fFirstCircle;
  While assigned(i) Do Begin
    If assigned(fOnBeforeCirclePaint) Then Begin
      fOnBeforeCirclePaint(self, i);
    End;
    // 1. Rendern des Segmentes
    If i^.Selected Then Begin
      Canvas.Brush.Color := i^.SelectedColor.BrushColor;
      canvas.Pen.Color := i^.SelectedColor.PenColor;
      canvas.Pen.Width := i^.SelectedColor.PenWitdh;
      canvas.Font.Color := i^.SelectedColor.FontColor;
    End
    Else Begin
      Canvas.Brush.Color := i^.Color.BrushColor;
      canvas.Pen.Color := i^.Color.PenColor;
      canvas.Pen.Width := i^.Color.PenWitdh;
      canvas.Font.Color := i^.Color.FontColor;
    End;
    canvas.Ellipse(
      round(i^.ScaledCenter.X - i^.ScaledRadius), round(i^.ScaledCenter.Y - i^.ScaledRadius),
      round(i^.ScaledCenter.X + i^.ScaledRadius), round(i^.ScaledCenter.Y + i^.ScaledRadius)
      );
    PlotTextAtPos(point(round(i^.ScaledCenter.X), round(i^.ScaledCenter.y)), i^.Caption);
    i := i^.Next;
  End;
  If assigned(OnAfterPaint) Then Begin
    OnAfterPaint(Self);
  End;
End;

Procedure TPackedCircleChart.DoOnResize;
Begin
  If fNeedRecalculation Then Begin
    ReCalculateIfNeeded();
  End
  Else Begin
    ScaleCirclesPositionsToClientRect;
  End;
  Invalidate;
  Inherited DoOnResize;
End;

Procedure TPackedCircleChart.DeselectAll;
Var
  i: PCircle;
Begin
  i := fFirstCircle;
  While assigned(i) Do Begin
    i^.Selected := false;
    i := i^.Next;
  End;
  Invalidate;
End;

Function TPackedCircleChart.GetCircleAtPos(x, y: integer; Out aElement: PCircle
  ): Boolean;
Var
  i: PCircle;
  dx, dy: Single;
Begin
  ReCalculateIfNeeded();
  result := false;
  i := fFirstCircle;
  While assigned(i) Do Begin
    dx := x - i^.ScaledCenter.x;
    dy := y - i^.ScaledCenter.y;
    If sqr(i^.ScaledRadius) >= sqr(dx) + sqr(dy) Then Begin
      result := true;
      aElement := i;
      exit;
    End;
    i := i^.Next;
  End;
End;

Function TPackedCircleChart.Iterator: PCircle;
Begin
  result := FIterator;
End;

Function TPackedCircleChart.IterFirst: PCircle;
Begin
  FIterator := fFirstCircle;
  result := FIterator;
End;

Function TPackedCircleChart.IterNext: PCircle;
Begin
  If assigned(FIterator) Then Begin
    FIterator := FIterator^.Next;
    result := FIterator;
  End
  Else Begin
    result := Nil;
  End;
End;

Procedure TPackedCircleChart.NeedReEvaluation;
Begin
  fNeedRecalculation := true;
  Invalidate;
End;

End.

