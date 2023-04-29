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
Unit ufpcparser;

{$MODE ObjFPC}{$H+}

Interface

Uses
  classes, SysUtils, upascallexer;

Const
  LookbackCnt = 10; // wie viele sollten das denn sein ??

Type

  TProcInfo = Record
    Method: String; // Procedure / Function / Constructor / Destructor
    ClassName: String; // Kann auch gerne "leer" sein, wenn die Function / Procedur in keiner Klasse enthalten ist
    Name: String; // Der Eigentliche Name der Methode
    CC: integer; // Die Zyklomatische Komplexität der jeweiligen Function / Procedur
    CaseCC: integer; // Die Zyklomatische Komplexität der jeweiligen Function / Procedur nur durch "Switch" Cases
    LineInFile: Integer; // Die Zeile in der das Schlüsselwort "Procedure" / "Funktion" Steht
    BeginLineInFile: Integer; // Die Zeile in der der Quellcode tatsächlich begint
    Filename: String; // Die Quelldatei in der die Funktion steht
    // Da könnte man auch noch verwursten, welche anderen Methoden diese Methode so aufruft..
  End;

  TProcInfos = Array Of TProcInfo;

  TClassInfo = Record
    Name: String; // Name der Klasse
    Parents: Array Of String; // Von Wem wird die Klasse abgeleitet ? (das sind mehrere da es ja auch Interfaces gibt) = Nil -> TObject
    LineInFile: integer; // Die 1. Implementierung, das ist dann ggf auch eine Forwärtsdeklaration, wenn man das anders will kann man das in  AddClassToClassList machen und der Doppelte überschreibt dann die Line
  End;

  TFileInfo = Record
    NumberofCommentLines: integer;
    NumberOfCodeLines: Integer;
    NumberOfEmptyLines: integer;
    NumberOfTotalLines: integer;
    aClasses: Array Of TClassInfo; // Alle Klassen in dieser Datei
    aUses: Array Of String; // Alle Uses oberhalb von Implementation
    aImplementationUses: Array Of String; // Alle "Uses" unterhalb von Implementation
    // Noch Mehr
  End;

  TState = (
    sIdle // Top Level Ebene hier kann "alles" gefunden werden
    , sInClassStart // Wir haben "= class" gefunden -> Entscheiden ob wir das interface einer Klasse einlesen oder nur ne Forwärtsdeclaration
    , sInParams // Wir werten "Parameter" einer Funktion / Classe irgendwas aus -> wir ignorieren alles bis zum Passenden ) und gehen in den Vorherigen State zurück
    , sInClassParents // Scan der Parents Commaliste einer Klasse (Normalerweise nur 1 + ggf interfaces)
    , sInClassInterface // Wir Parsen ein "echtes" Klassen Interface -> Scip bis "end;"
    , sParseFunProcName // Einlesen der Aktuellen Funktion
    , sInFunProc // Das eigentliche Messen der CC in der gefundenen Methode (inclusive der CC aller genesteten Methoden)
    , sScanIfComplexity // Scant den Teil zwischen "if" und "Then"
    , sScanUses // Liest einen Uses Teil ein.
    );


  TLookBack = Record // Rückblick auf die "Letzten" gesehenen Tokens
    // LookbackStr: String; // Debug
    LookbackPtr: integer;
    Lookback: Array[0..LookbackCnt - 1] Of TToken;
  End;

  { TFPCParser }

  TFPCParser = Class
  private
    UsesMerge: Boolean; // True, wenn eine Uses der Form "blub.Blab" ist
    aClassindex: integer; // Der Index der Klasse die gerade geparst wird in fFileInfo.aClasses
    LoockBack: TLookBack;
    fFileInfo: TFileInfo;
    Infos: TProcInfos;
    aProcInfo: TProcInfo;
    Counter: integer;
    PrevState: TState;
    State: TState;
    lastCodeLine, lastCommentLine: integer;
    BelowImplementation: Boolean;
    InClassDefinition: Boolean;
    Filename: String;
    ParseInternal: Integer;
    InCaseCounter: integer;
    Procedure OnHandleToken(Sender: TObject; Const Token: TToken);
    Procedure AddClassToClassList(Const aToken: TToken);

    Procedure InitLookback();
    Procedure PushLookback(Value: TToken);
    Function Last(Index: Integer): TToken; // 0 = der Letzte, 1 = der Vorletzte ...
  public
    Property ProcInfos: TProcInfos read Infos;
    Property FileInfo: TFileInfo read fFileInfo;
    Function ParseFile(aFilename: String): Boolean;
  End;

Implementation

Uses Dialogs;

Procedure Nop();
Begin

End;

Procedure TFPCParser.OnHandleToken(Sender: TObject; Const Token: TToken);
Var
  i: Integer;
  found: Boolean;
Begin
  If (lastCodeLine <> Token.Line) And (Token.Kind <> tkComment) Then Begin
    lastCodeLine := Token.Line;
    inc(fFileInfo.NumberOfCodeLines);
  End;
  If (lastCommentLine <> token.Line) And (token.Kind = tkComment) Then Begin
    lastCommentLine := token.Line;
    For i := 1 To length(token.Value) Do Begin
      If token.value[i] = #10 Then inc(fFileInfo.NumberofCommentLines);
    End;
    inc(fFileInfo.NumberofCommentLines);
  End;
  (* filter everything irrelevant out ;) *)
  If (Token.Kind = tkComment) Or (Token.Kind = tkString) Or (Token.Kind = tkCompilerDirective) Then Exit;
  (*
   * Strategie:
   * 1. Suchen nach Schlüsselwort "Procedur" / "Funktion", dabei wissen ob wir gerade in einer Klassendeklaration sind und über oder unter "implementation"
   *
   *)
  If (token.Kind = tkKeyWord) And (lowercase(Token.Value) = 'implementation') Then Begin
    BelowImplementation := true;
  End;
  Case State Of
    sIdle: Begin
        (*
         * Wir suchen nach einem Text der Art
         * "= class"
         * -> Das ist dann die Deklaration einer Klasse
         * es gillt 2 Fälle zu unterscheiden
         * 1. Eine Klassen Forwärts deklaration -> Gleich wieder "Raus"
         * 2. Eine echte Klassen Deklaration -> Alles überlesen bis zum passenden "End;"
         *)
        If (Last(0).Value = '=') And (lowercase(Token.Value) = 'class') Then Begin
          State := sInClassStart;
          If last(1).Value = '>' Then Begin
            found := false;
            For i := 2 To LookbackCnt - 1 Do Begin
              If lowercase(Last(i).Value) = 'generic' Then Begin
                AddClassToClassList(Last(i - 1));
                found := true;
                break;
              End;
            End;
            If Not found Then Begin
              Raise exception.create('Error unable to determine the Name of class declared in (' + inttostr(Token.Line) + ')');
            End;
          End
          Else Begin
            AddClassToClassList(Last(1))
          End;
        End;
        If (Last(0).Value = '=') And (lowercase(Token.Value) = 'interface') Then Begin
          Raise exception.create('Error interfaces are not supported yet (' + inttostr(Token.Line) + ')');
        End;
        If BelowImplementation Then Begin
          If (lowercase(Token.Value) = 'procedure') Or (lowercase(Token.Value) = 'function') Or
            (lowercase(Token.Value) = 'constructor') Or (lowercase(Token.Value) = 'destructor') Then Begin
            // We actually found a real Declaration of a Procedure / Function ;)
            state := sParseFunProcName;
            aProcInfo.LineInFile := Token.Line;
            aProcInfo.CC := 1;
            aProcInfo.CaseCC := 0;
            aProcInfo.ClassName := '';
            aProcInfo.Name := '';
            aProcInfo.Filename := Filename;
            aProcInfo.Method := Token.Value;
            InCaseCounter := 0;
          End;
        End;
        If lowercase(token.Value) = 'uses' Then Begin
          UsesMerge := false;
          state := sScanUses;
        End;
      End;
    sScanUses: Begin
        Case token.value Of
          ';': State := sIdle;
          ',', 'in': Begin
              // Ignorieren wir, wir sind ja kein Syntaxchecker ;)
            End;
          '.': Begin // Es gibt so dinger wie Uses blub.blab;
              UsesMerge := true;
            End
        Else Begin
            If (token.Kind = tkKeyWord) Or (token.Kind = tkCompilerDirective) Or (token.Kind = tkOperator) Then Begin
              State := sIdle;
            End
            Else Begin
              If token.Kind <> tkString Then Begin // Der Dateiname zum "in" teil von "uses foo in 'Bar.pas';
                If BelowImplementation Then Begin
                  i := high(fFileInfo.aImplementationUses);
                  If UsesMerge Then Begin
                    fFileInfo.aImplementationUses[i] := fFileInfo.aImplementationUses[i] + '.';
                  End
                  Else Begin
                    setlength(fFileInfo.aImplementationUses, high(fFileInfo.aImplementationUses) + 2);
                    i := high(fFileInfo.aImplementationUses);
                    fFileInfo.aImplementationUses[i] := '';
                  End;
                  fFileInfo.aImplementationUses[i] := fFileInfo.aImplementationUses[i] + Token.Value;
                  UsesMerge := false;
                End
                Else Begin
                  i := high(fFileInfo.aUses);
                  If UsesMerge Then Begin
                    fFileInfo.aUses[i] := fFileInfo.aUses[i] + '.';
                  End
                  Else Begin
                    setlength(fFileInfo.aUses, high(fFileInfo.aUses) + 2);
                    i := high(fFileInfo.aUses);
                    fFileInfo.aUses[i] := '';
                  End;
                  fFileInfo.aUses[i] := fFileInfo.aUses[i] + Token.Value;
                  UsesMerge := false;
                End;
              End;
            End;
          End;
        End;
      End;
    sInClassStart: Begin
        If lowercase(token.Value) = 'helper' Then Begin
          // siehe: https://www.freepascal.org/docs-html/ref/refse65.html#x125-14900010.2
          Raise exception.Create('Class helper not yet supported by parser.');
        End;
        If (Token.Value = '(') Then Begin // ggf. überlesen von "Parent"
          PrevState := State;
          State := sInClassParents;
        End;
        If (token.Value = ';') Then State := sIdle; // War nur ne Classen Forwärts Deklaration
        If (token.Kind = tkKeyWord) Or (token.Kind = tkIdentifier) Then Begin
          State := sInClassInterface;
          Counter := 1;
        End;
      End;
    sInClassParents: Begin
        If (Token.Kind = tkOperator) And (Token.Value = ')') Then Begin
          State := sInClassStart;
        End;
        If (token.Kind <> tkOperator) Then Begin // Es kommen nur "," und ")" for.
          setlength(fFileInfo.aClasses[aClassindex].Parents, high(fFileInfo.aClasses[aClassindex].Parents) + 2);
          fFileInfo.aClasses[aClassindex].Parents[high(fFileInfo.aClasses[aClassindex].Parents)] := Token.Value;
        End;
      End;
    sInParams: Begin // Alles ignorieren bis ")" und dann wieder zurück zu dem was wir waren.
        If (Token.Kind = tkOperator) And (Token.Value = ')') Then Begin
          State := PrevState;
        End;
      End;
    sInClassInterface: Begin
        If (Last(0).Value = '=') And (lowercase(token.Value) = 'record') Then Begin // Eine Record Definition innerhalb des Classen Interfaces ..
          inc(Counter);
        End;
        If (Last(0).Value = '=') And (lowercase(token.Value) = 'class') Then Begin // Eine weitere Klassen Definition innerhalb eines Klassen Interfaces
          Raise exception.create('Error in ' + Filename + LineEnding +
            'Line: ' + inttostr(Token.Line) + ' found nested class definition, is not supported by scanner !');
        End;
        If (Last(0).Value = '=') And (lowercase(token.Value) = 'object') Then Begin // Eine weitere Klassen Definition innerhalb eines Klassen Interfaces
          Raise exception.create('Error in ' + Filename + LineEnding +
            'Line: ' + inttostr(Token.Line) + ' found nested object definition, is not supported by scanner !');
        End;
        If (lowercase(Last(0).Value) = 'end') And (Token.Value = ';') Then Begin
          dec(Counter);
          If Counter = 0 Then Begin
            State := sIdle;
          End;
        End;
      End;
    sParseFunProcName: Begin
        (*
         * Nach dem Schlüsselwort Function / Procedure kann es wie folgt aussehen
           Procedure Blub;
           Procedure TForm.Blub;
           Procedure Blub();
           function Blub:;
         *)
        Case Token.Value Of
          '.': aProcInfo.ClassName := aProcInfo.Name;
          '(': Begin
              PrevState := sParseFunProcName;
              State := sInParams;
            End;
          ':', ';': Begin
              State := sInFunProc;
              Counter := 0;
              ParseInternal := 0;
            End;
        Else Begin
            aProcInfo.Name := Token.Value;
          End;
        End;
      End;
    sInFunProc: Begin
        (*
         * alles was sein eigenes "End" hat muss den Counter natürlich erhöhen ;)
         *)
        If (Last(0).Value = '=') And (lowercase(token.Value) = 'record') Then Begin
          inc(ParseInternal);
          inc(Counter);
        End;
        If (LowerCase(token.Value) = 'procedure') Or (LowerCase(token.Value) = 'function') Then Begin
          inc(ParseInternal);
          inc(aProcInfo.CC); // Eine Genestete Methode erhöht auch !
        End;
        If lowercase(Token.Value) = 'begin' Then Begin
          If (counter = 0) And (ParseInternal = 0) Then Begin
            aProcInfo.BeginLineInFile := Token.Line;
          End;
          inc(Counter);
          If InCaseCounter > 0 Then inc(InCaseCounter);
        End;
        If lowercase(Token.Value) = 'case' Then Begin
          inc(InCaseCounter);
          inc(Counter);
        End;

        If (lowercase(Token.Value) = 'if') Then Begin
          inc(aProcInfo.CC);
          state := sScanIfComplexity;
        End;
        If (lowercase(Token.Value) = 'case') Then inc(aProcInfo.CC);
        (*
         * Wir dürfen nur die : zählen die zwischen Begin und End Stehen,
         * Da Counter mit dem 1, Begin steigt passt das dann. Und um die Genesteten auch korrekt zu
         * Berücksichtigen muss es ParseInternal sein.!
         *)
        If (lowercase(Token.Value) = ':') And (InCaseCounter > 0) Then Begin
          inc(aProcInfo.CaseCC);
          inc(aProcInfo.CC);
        End;
        If (lowercase(Token.Value) = 'while') Then inc(aProcInfo.CC);
        If (lowercase(Token.Value) = 'repeat') Then inc(aProcInfo.CC);
        If (lowercase(Token.Value) = 'for') Then inc(aProcInfo.CC);
        If (lowercase(Token.Value) = 'end') Then Begin
          If InCaseCounter > 0 Then dec(InCaseCounter);
          dec(Counter);
          If (Counter = 0) Then Begin
            If (ParseInternal = 0) Then Begin
              // Geschafft speichern der Ergebnisse ;)
              setlength(Infos, high(Infos) + 2);
              Infos[high(Infos)] := aProcInfo;
              State := sIdle;
            End;
            dec(ParseInternal);
          End;
        End;
      End;
    sScanIfComplexity: Begin
        If (lowercase(Token.Value) = 'then') Then State := sInFunProc;
        If (lowercase(Token.Value) = 'and') Then inc(aProcInfo.CC);
        If (lowercase(Token.Value) = 'or') Then inc(aProcInfo.CC);
      End;
  End;
  PushLookback(Token);
End;

Procedure TFPCParser.AddClassToClassList(Const aToken: TToken);
Var
  Name: String;
  i: Integer;
Begin
  name := LowerCase(aToken.Value);
  For i := 0 To high(fFileInfo.aClasses) Do Begin
    If lowercase(fFileInfo.aclasses[i].Name) = name Then Begin
      aClassindex := i;
      exit;
    End;
  End;
  setlength(fFileInfo.aClasses, high(fFileInfo.aClasses) + 2);
  fFileInfo.aClasses[high(fFileInfo.aClasses)].Name := aToken.Value;
  fFileInfo.aClasses[high(fFileInfo.aClasses)].LineInFile := aToken.Line;
  fFileInfo.aClasses[high(fFileInfo.aClasses)].Parents := Nil;
  aClassindex := high(fFileInfo.aClasses);
End;

Procedure TFPCParser.InitLookback;
Var
  i: integer;
Begin
  LoockBack.LookbackPtr := 0;
  //  LoockBack.LookbackStr := ''; // Debug
  For i := 0 To high(LoockBack.Lookback) Do Begin
    LoockBack.Lookback[i].Value := '';
    LoockBack.Lookback[i].Line := 0;
    LoockBack.Lookback[i].Kind := tkComment;
  End;
End;

Procedure TFPCParser.PushLookback(Value: TToken);
//Var
//  i: Integer;
Begin
  LoockBack.Lookback[LoockBack.LookbackPtr] := Value;
  LoockBack.LookbackPtr := (LoockBack.LookbackPtr + 1) Mod LookbackCnt;
  //  LoockBack.LookbackStr := ''; // Debug
  //  For i := 1 To LookbackCnt Do Begin // Debug
  //    LoockBack.LookbackStr := LoockBack.Lookback[(LoockBack.LookbackPtr - i + LookbackCnt) Mod LookbackCnt].Value + ' ' + LoockBack.LookbackStr; // Debug
  //  End; // Debug
End;

Function TFPCParser.Last(Index: Integer): TToken;
Begin
  result := LoockBack.Lookback[(LoockBack.LookbackPtr - Index - 1 + 2 * LookbackCnt) Mod LookbackCnt];
End;

Function TFPCParser.ParseFile(aFilename: String): Boolean;
Var
  Lexer: TpascalLexer;
Begin
  result := false;
  Infos := Nil;
  fFileInfo.NumberofCommentLines := 0;
  fFileInfo.NumberOfCodeLines := 0;
  fFileInfo.NumberOfEmptyLines := 0;
  fFileInfo.NumberOfTotalLines := 0;
  fFileInfo.aClasses := Nil;
  fFileInfo.aUses := Nil;
  fFileInfo.aImplementationUses := Nil;
  Try
    lexer := TpascalLexer.Create();
    lexer.OnHandleToken := @OnHandleToken;
    BelowImplementation := false;
    InClassDefinition := false;
    State := sIdle;
    Filename := aFilename;
    InitLookback();
    lastCodeLine := 0;
    lastCommentLine := 0;
    lexer.LexFile(aFilename);
    fFileInfo.NumberOfTotalLines := lexer.TotalLineCount - 1;
    fFileInfo.NumberOfEmptyLines := lexer.EmptyLineCount - 1;
    lexer.free;
    result := true;
  Except
    On e: Exception Do Begin
      showmessage(E.Message + LineEnding + 'File:' + aFilename);
    End;
  End;
End;

End.

