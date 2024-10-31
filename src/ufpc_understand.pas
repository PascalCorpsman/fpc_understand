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
Unit ufpc_understand;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Graphics, uDOMXML, ufpcparser;

Const
  Default_CCLevelGood = 10;
  Default_CCLevelGoodColor = clGreen;
  Default_CCLevelModerate = 20;
  Default_CCLevelModerateColor = clYellow;
  Default_CCLevelComplex = 50;
  Default_CCLevelComplexColor = clMaroon;
  Default_CCLevelUnstableColor = clred;
  Default_BoarderForLargestFunction = 100;
  Default_BoarderForLargestFiles = 500;
  Default_BoarderForAverageMostComplexFiles = 1;

Type

  TRemoveResult = (rrRemoved, rrdeactivated, rrError);

  TProjectFileInfo = Record
    Filename: String; // Relativer Dateiname
    FileInfo: TFileInfo;
    Methods: TProcInfos;
  End;

  TProjectFilesInfo = Array Of TProjectFileInfo;

  TGeneral = Record
    ProjectName: String; // Zum Anzeigen in der Caption
  End;

  TProjectFile = Record
    FromLPI: Boolean;
    Enabled: Boolean;
    FileName: String; // Relativer Dateiname
  End;

  TFileList = Array Of TProjectFile;

  TProjectSearchPath = Record
    FromLPI: Boolean;
    Path: String;
  End;

  TPathList = Array Of TProjectSearchPath;

  TFiles = Record
    LPISource: String;
    RootFolder: String;
    Files: TFileList;
  End;

  // https://en.wikipedia.org/wiki/Cyclomatic_complexity
  TCCColors = Record
    LevelGood: integer; // 1 - 10 Simple procedure, little risk
    ColorGood: TColor;
    LevelModerate: Integer; // 11 - 20 More complex, moderate risk
    ColorModerate: TColor;
    LevelComplex: Integer; // 21 - 50 Complex, high risk
    ColorComplex: TColor;
    ColorUnstable: TColor; // > 50 Untestable code, very high risk
  End;

  TChartStatisticSettings = Record
    BoarderForMostComplexFunction: integer; // Eine Funktion muss eine größere CC als X haben um in die Statistik mit aufgenommen zu werden
    BoarderForLargestFunction: Integer; // Eine Funktion muss mehr als X Zeilen haben um in die Statistik mit aufgenommen zu werden
    BoarderForLargestFiles: integer; // Eine Datei muss mehr als X Zeilen haben um in die Statistik mit aufgenommen zu werden
    BoarderForAverageMostComplexFiles: integer; // Der Durchschnitt aller Methoden in einer Datei muss größer diesem Wert liegen um Berücksichtigt zu werden
  End;

  TLineComment = Record
    Line: integer;
    Filename: String;
    Comment: String;
  End;

  TLineComments = Array Of TLineComment;

  { TProject }

  TProject = Class
  private
    fLineComments: TLineComments;

    fGeneral: TGeneral;
    fFiles: TFiles;
    FSearchPaths: TPathList;
    (*
     * Wird nicht gespeichert.
     *)
    fFilename: String; // Der Dateiname in dem die Projekteinstellungen gespeichert wurden
    fChanged: Boolean;
    fCCColors: TCCColors;
    Procedure SetCCColors(AValue: TCCColors);
    Procedure SetFiles(AValue: TFileList);
    Procedure SetLPISource(AValue: String);
    Procedure SetName(AValue: String);
    Procedure SetRootFolder(AValue: String);
  public
    ChartStatisticSettings: TChartStatisticSettings;
    Property CCColors: TCCColors read fCCColors write SetCCColors;
    (*
     * General
     *)
    Property Name: String read fGeneral.ProjectName write SetName;

    (*
     * Files
     *)
    Property RootFolder: String read fFiles.RootFolder write SetRootFolder;
    Property LPISource: String read fFiles.LPISource write SetLPISource;
    Property Files: TFileList read fFiles.Files write SetFiles;
    Property SearchPaths: TPathList read FSearchPaths write FSearchPaths; // relativ zu Rootfolder

    (*
     * For Internal Use
     *)
    Property FileName: String read fFilename;
    Property Changed: Boolean read fChanged;

    Constructor Create(); virtual;
    Destructor Destroy(); override;

    Procedure Clear;

    Procedure RemoveLineComment(aFilename: String; line: Integer);
    Procedure SetLineComment(aFilename: String; line: Integer; Comment: String);
    Function GetLineComments(): TLineComments;

    Function LoadFromFile(aFilename: String): boolean;
    Procedure SaveToFile(aFilename: String);
    Procedure Change(); // Setzen des Changed Flags
  End;

  (*
   * In:
   *    RootFolder:
   *    LPIFile   :
   *    FileList  : Absolute Dateiliste
   * Out:
   *    Absolute" Dateliste
   *)
Function GetFileList(RootFolder, LPIFile: String; FileList: TFileList): TFileList;

(*
 * In:
 *    RootFolder:
 *    LPIFile   :
 *    PathList  : Absolute Pfadliste
 * Out:
 *    Absolute" Pfadliste
 *)
Function GetSearchPathList(RootFolder, LPIFile: String; PathList: TPathList): TPathList;

Function ConcatRelativePath(Const BaseName: String; Const RelativeName: String): String; // Gegenteil zu ExtractRelativePath

Function RelativeFileListToAbsoluteFileList(RootFolder: String; FileList: TFileList): TFileList;
Function RelativePathListToAbsolutePathList(RootFolder: String; PathList: TPathList): TPathList;

Function NameInList(Const List: TProjectFilesInfo; aName: String): integer; // Wenn nicht enthalten = -1, sonst der Index ni List

Procedure Nop();

(*
 * Geht alle SearchPaths (die bereits OS-Absolut sein müssen) durch
 * und schaut ob Filename dort enthalten ist.
 * Bei Erfolg wird der erste Gültige Dateiname zurück gegeben, sonst ''
 *
 * Hat Filename keine Dateiendung werden die typischen FPC Dateieendungen "probiert"
 *)
Function SearchFileInSearchPaths(Const SearchPaths: TPathList; Filename: String): String;

// ----- Routinen zum Auslesen / verarbeiten einer Lazarus .lpi Datei ----------

(*
 * Repariert in einem Alten .lpi File die Durchnummerierung der "Units"
 *)
Procedure FixLPIUnitCounts(Const LPIFile: TDOMXML);

(*
 * 1.
 * Filename = Absoluter OS-Passender Dateiname -> wird passend in das .lpi File convertiert.
 * -1 = Fehler
 * sonst Index der Datei die eingefügt wurde
 * 2. ggf erweitern des Suchpfades
 *)
Function AddFileToLPIFile(Const LPIRoot: String; Const LPIFile: TDOMXML; Filename: String): integer;
(*
* 1.
* Filename = Absoluter OS-Passender Dateiname -> wird passend in das .lpi File convertiert.
* -1 = Fehler
* sonst Index der Datei die eingefügt wurde
* 2. ggf entfernen des Suchpfades
 *)
Function RemoveFileFromLPIFile(Const LPIRoot: String; Const LPIFile: TDOMXML; Filename: String): TRemoveResult;

(*
 * Gibt den Pathdelim zurück, oder OS.Pathdelim wenn keiner vorhanden ist.
 *)
Function GetPathDelimFromLPIFile(Const LPIFile: TDOMXML): String;

Function ExportCommentsAsCSV(Comments: TLineComments; Filename: String): Boolean;

Implementation

Uses IniFiles, Dialogs, LazFileUtils, LCLType, forms, Math;

Procedure Nop;
Begin

End;

Function DeSerialize(Value: String): String;
Var
  i, j: integer;
Begin
  // Optionales Entfernen der Anführenden "
  If length(Value) > 0 Then Begin
    If Value[1] = '"' Then Begin
      delete(Value, 1, 1);
      delete(Value, length(Value), 1);
    End;
  End;
  setlength(result, length(Value) * 2);
  j := 1;
  i := 1;
  While i <= length(value) Do Begin
    If value[i] = '#' Then Begin
      inc(i);
      If i > length(value) Then Begin
        Raise exception.create('Error invalid deserialize String "' + value + '"');
      End;
      Case Value[i] Of
        '#': Begin
            result[j] := '#';
            inc(j);
          End;
        '-': Begin
            result[j] := '"';
            inc(j);
          End;
        '+': Begin
{$IFDEF Windows}
            result[j] := LineEnding[1];
            result[j + 1] := LineEnding[2];
            inc(j, 2);
{$ELSE}
            result[j] := LineEnding;
            inc(j);
{$ENDIF}
          End
      Else Begin
          Raise exception.Create('Error "' + value[i] + '" Not known as deserialize param.');
        End;
      End;
      inc(i);
    End
    Else Begin
      result[j] := value[i];
      inc(i);
      inc(j);
    End;
  End;
  setlength(result, j - 1);
End;

Function Serialize(Const Value: String): String;
Var
  i: Integer;
Begin
  result := '"';
  For i := 1 To length(value) Do Begin
    Case value[i] Of
{$IFDEF Windows}
      #10: Begin // Das Muss Geschluckt werden
        End;
      #13: Begin
{$ELSE}
      LineEnding: Begin
{$ENDIF}
          result := result + '#+';
        End;
      '#': Begin
          result := result + '##';
        End;
      '"': Begin
          result := result + '#-';
        End
    Else Begin
        result := result + value[i];
      End;
    End;
  End;
  result := result + '"';
End;

Function GetOSIdentiferString(): String;
Begin
  Result := '';
{$IFDEF Linux}
  Result := 'Linux';
{$ENDIF}
{$IFDEF darwin}
  Result := 'Darwin';
{$ENDIF}
{$IFDEF Windows}
  Result := 'Windows';
{$ENDIF}
End;

Function SearchFileInSearchPaths(Const SearchPaths: TPathList;
  Filename: String): String;
Var
  i, j: Integer;
  fn: String;
  exts: TStringArray;
Begin
  result := '';
  fn := ExtractFileExt(Filename);
  exts := Nil;
  If fn = '' Then Begin
    setlength(exts, 3);
    exts[0] := '.pas';
    exts[1] := '.pp';
    exts[2] := '.lpr';
  End
  Else Begin
    setlength(exts, 1);
    exts[0] := fn;
    filename := ExtractFileNameWithoutExt(Filename);
  End;
  For i := 0 To high(SearchPaths) Do Begin
    For j := 0 To high(exts) Do Begin
      fn := IncludeTrailingPathDelimiter(SearchPaths[i].Path) + Filename + exts[j];
      If FileExists(fn) Then Begin
        result := fn;
        exit;
      End;
    End;
  End;
End;

Function GetSearchPathsFromLPIFile(Const LPIFile: TDOMXML): TStringArray;
Var
  OtherUnitFiles: TDomNode;
  pDelim: String;
  i: Integer;
Begin
  OtherUnitFiles := lpiFile.DocumentElement.FindPath('CONFIG.CompilerOptions.SearchPaths.OtherUnitFiles');
  If assigned(OtherUnitFiles) Then Begin
    result := OtherUnitFiles.AttributeValue['Value'].Split(';');
  End;
  pDelim := GetPathDelimFromLPIFile(LPIFile);
  If PathDelim <> pDelim Then Begin
    For i := 0 To high(Result) Do Begin
      result[i] := StringReplace(result[i], pDelim, PathDelim, [rfReplaceAll]);
    End;
  End;
End;

Function GetSearchPathsFromLPIFile(Const LPIFileName: String): TStringArray;
Var
  lpiFile: TDOMXML;
Begin
  result := Nil;
  lpiFile := TDOMXML.Create;
  If Not lpiFile.LoadFromFile(LPIFileName) Then Begin
    lpiFile.free;
    exit;
  End;
  result := GetSearchPathsFromLPIFile(lpiFile);
  lpiFile.free;
End;

Function IsOldLPIFile(Const LPIFile: TDOMXML): Boolean;
Var
  units: TDomNode;
  i: Integer;
Begin
  result := false;
  units := lpiFile.DocumentElement.FindPath('CONFIG.ProjectOptions.units');
  If Not assigned(units) Then Begin
    Raise exception.create('Not a .lpi file.');
  End;
  For i := 0 To units.AttributeCount - 1 Do Begin
    If Lowercase(units.Attribute[i].AttributeName) = 'count' Then Begin
      result := true;
      break;
    End;
  End;
End;

Procedure FixLPIUnitCounts(Const LPIFile: TDOMXML);
Var
  index: Integer;
  units, unitfile: TDomNode;
Begin
  If Not IsOldLPIFile(LPIFile) Then exit;
  // Neu Durchnummerieren der Units und am Ende den Count Aktualisieren
  units := lpiFile.DocumentElement.FindPath('CONFIG.ProjectOptions.units');
  unitfile := units.FirstChild;
  index := 0;
  While assigned(unitfile) Do Begin
    unitfile.NodeName := 'Unit' + inttostr(index);
    unitfile := units.NextSibling;
    inc(index);
  End;
  units.AttributeValue['Count'] := inttostr(index);
End;

Function AddFileToLPIFile(Const LPIRoot: String; Const LPIFile: TDOMXML;
  Filename: String): integer;
Var
  PDelim, FilePath, Value: String;
  units, unitfile, IsPartOf, FilenameNode, CompilerOptions, Config,
    SearchPaths, OtherUnitFiles: TDomNode;
  spa: TStringArray;
  found: Boolean;
  i: Integer;
Begin
  // 1. Eintragen in die Liste der Dateien
  result := -1;
  If Not FileExists(Filename) Then Begin
    exit; // Da Stimmt was nicht
  End;
  Filename := ExtractRelativePath(IncludeTrailingPathDelimiter(LPIRoot), Filename);
  PDelim := GetPathDelimFromLPIFile(LPIFile);
  If PathDelim <> PDelim Then Begin
    Filename := StringReplace(Filename, PathDelim, PDelim, [rfReplaceAll]);
  End;
  units := lpiFile.DocumentElement.FindPath('CONFIG.ProjectOptions.units');
  If Not assigned(units) Then Begin
    Raise exception.create('Not a .lpi file.');
  End;
  // Prüfen ob die Datei schon teil des Projectes ist, dann nicht einfügen, sondern nur "Freischalten"
  unitfile := units.FirstChild;
  result := 0;
  While assigned(unitfile) Do Begin
    If unitfile.FindNode('Filename').AttributeValue['Value'] = Filename Then Begin
      IsPartOf := unitfile.AddChild('IsPartOfProject', '');
      IsPartOf.AddAttribute('Value', 'True');
      exit;
    End;
    unitfile := units.NextSibling;
    inc(result);
  End;
  // Noch nicht Teil -> Einfügen
  unitfile := units.AddChild('Unit', '');
  FilenameNode := unitfile.AddChild('Filename', '');
  FilenameNode.AddAttribute('Value', Filename);
  IsPartOf := unitfile.AddChild('IsPartOfProject', '');
  IsPartOf.AddAttribute('Value', 'True');
  FixLPIUnitCounts(LPIFile);
  // 2. ggf. in die Suchpfade aufnehmen.
  FilePath := ExcludeTrailingPathDelimiter(ExtractFilePath(Filename));
  If FilePath <> '' Then Begin
    CompilerOptions := lpiFile.DocumentElement.FindPath('CONFIG.CompilerOptions');
    If Not assigned(CompilerOptions) Then Begin
      Config := lpiFile.DocumentElement.FindPath('CONFIG');
      If Not assigned(Config) Then Begin
        Raise exception.create('Is this a valid .lpi file ?');
      End;
      CompilerOptions := config.AddChild('CompilerOptions', '');
    End;
    SearchPaths := CompilerOptions.FindPath('SearchPaths');
    If Not assigned(SearchPaths) Then Begin
      SearchPaths := CompilerOptions.AddChild('SearchPaths', '');
    End;
    OtherUnitFiles := SearchPaths.FindPath('OtherUnitFiles');
    If Not assigned(OtherUnitFiles) Then Begin
      OtherUnitFiles := SearchPaths.AddChild('OtherUnitFiles', '');
    End;
    Value := OtherUnitFiles.AttributeValue['Value'];
    spa := value.Split(';');
    found := false;
    For i := 0 To high(spa) Do Begin
      If spa[i] = FilePath Then Begin
        found := true;
        break;
      End;
    End;
    If (Not found) And (id_yes = Application.MessageBox(
      pchar('Searchpath of "' + ExtractFileNameOnly(Filename) + '" is not in project, do you want to add it?'),
      'Question', MB_ICONQUESTION Or MB_YESNO)) Then Begin
      If value <> '' Then Begin
        value := value + ';';
      End;
      value := value + FilePath;
      OtherUnitFiles.AttributeValue['Value'] := Value;
    End;
  End;
End;

Function RemoveFileFromLPIFile(Const LPIRoot: String; Const LPIFile: TDOMXML;
  Filename: String): TRemoveResult;
Var
  units, unitfile, fn, OtherUnitFiles, IsPartOfProject: TDomNode;
  PDelim, FilePath, fp, s: String;
  Found: Boolean;
  spa: TStringArray;
  index, i: Integer;
Begin
  result := rrError;
  // 0. Umrechnen des Dateinamens so wie er im .lpi File steht
  Filename := ExtractRelativePath(IncludeTrailingPathDelimiter(LPIRoot), Filename);
  PDelim := GetPathDelimFromLPIFile(LPIFile);
  If PathDelim <> PDelim Then Begin
    Filename := StringReplace(Filename, PathDelim, PDelim, [rfReplaceAll]);
  End;
  // 1. Suchen ob die Datei überhaupt im File ist
  units := lpiFile.DocumentElement.FindPath('CONFIG.ProjectOptions.units');
  If Not assigned(units) Then Begin
    Raise exception.create('Not a .lpi file.');
  End;
  // Prüfen ob die Datei schon Teil des Projectes ist, dann nicht einfügen, sondern nur "Freischalten"
  unitfile := units.FirstChild;
  While assigned(unitfile) Do Begin
    If unitfile.FindNode('Filename').AttributeValue['Value'] = Filename Then Begin
      IsPartOfProject := unitfile.FindNode('IsPartOfProject');
      If assigned(IsPartOfProject) Then Begin
        IsPartOfProject.free; // Wir löschen nur den "isPartOfPart" -> Es bleibt drin, wird aber deaktiviert
        result := rrdeactivated;
      End
      Else Begin
        // Wir löschen das File aus dem Projekt weil es eh schon nicht mehr drin war
        unitfile.Free;
        result := rrRemoved;
      End;
      unitfile := Nil;
    End
    Else Begin
      unitfile := units.NextSibling;
    End;
  End;
  If result = rrError Then exit; // Die Datei war nicht drin -> Raus
  FixLPIUnitCounts(LPIFile);
  FilePath := ExcludeTrailingPathDelimiter(ExtractFilePath(Filename));
  If FilePath <> '' Then Begin
    // 1. Schauen ob dieser Pfad noch mal in irgend einer Datei vorkommt, wenn nein, dann die Frage auf entfernen.
    unitfile := units.FirstChild;
    Found := false;
    While assigned(unitfile) Do Begin
      fn := unitfile.FindNode('Filename');
      fp := ExcludeTrailingPathDelimiter(ExtractFilePath(fn.AttributeValue['Value']));
      unitfile := units.NextSibling;
      If FilePath = fp Then Begin
        found := true;
        unitfile := Nil;
      End;
    End;
    OtherUnitFiles := lpiFile.DocumentElement.FindPath('CONFIG.CompilerOptions.SearchPaths.OtherUnitFiles');
    If (Not found) And assigned(OtherUnitFiles) Then Begin
      //  Schauen ob FilePath in den Suchpfaden ist, wenn ja fragen ob er entfernt werden soll
      spa := OtherUnitFiles.AttributeValue['Value'].Split(';');
      index := -1;
      For i := 0 To high(spa) Do Begin
        If spa[i] = FilePath Then Begin
          index := i;
          break;
        End;
      End;
      If (index <> -1) And (id_yes = Application.MessageBox(pchar('Searchpath of "' +
        ExtractFileNameOnly(FileName) +
        '" not used anymore, remove it?'), 'Question', MB_ICONQUESTION Or MB_YESNO)) Then Begin
        s := '';
        For i := 0 To high(spa) - 1 Do Begin
          If i <> index Then Begin
            If s <> '' Then s := s + ';';
            s := s + spa[i];
          End;
        End;
        OtherUnitFiles.AttributeValue['Value'] := s;
      End;
    End;
  End;
End;

Function GetPathDelimFromLPIFile(Const LPIFile: TDOMXML): String;
Var
  PDelim: TDomNode;
Begin
  result := '/'; // Lazarus intern Default ist "/" = Linux Style
  PDelim := lpiFile.DocumentElement.FindPath('CONFIG.CompilerOptions.PathDelim');
  If assigned(PDelim) Then result := PDelim.AttributeValue['Value'];
End;

Function ExportCommentsAsCSV(Comments: TLineComments; Filename: String
  ): Boolean;
  Procedure QuickFilename(li, re: integer);
  Var
    l, r: Integer;
    p: String;
    h: TLineComment;
  Begin
    If Li < Re Then Begin
      // Achtung, das Pivotelement darf nur einam vor den While schleifen ausgelesen werden, danach nicht mehr !!
      p := Comments[Trunc((li + re) / 2)].Filename; // Auslesen des Pivo Elementes
      l := Li;
      r := re;
      While l < r Do Begin
        While CompareStr(Comments[l].Filename, p) < 0 Do
          inc(l);
        While CompareStr(Comments[r].Filename, p) > 0 Do
          dec(r);
        If L <= R Then Begin
          h := Comments[l];
          Comments[l] := Comments[r];
          Comments[r] := h;
          inc(l);
          dec(r);
        End;
      End;
      QuickFilename(li, r);
      QuickFilename(l, re);
    End;
  End;

  Procedure QuickLines(li, re: integer);
  Var
    l, r, p: Integer;
    h: TLineComment;
  Begin
    If Li < Re Then Begin
      // Achtung, das Pivotelement darf nur einam vor den While schleifen ausgelesen werden, danach nicht mehr !!
      p := Comments[Trunc((li + re) / 2)].Line; // Auslesen des Pivo Elementes
      l := Li;
      r := re;
      While l < r Do Begin
        While Comments[l].Line < p Do
          inc(l);
        While Comments[r].Line > p Do
          dec(r);
        If L <= R Then Begin
          h := Comments[l];
          Comments[l] := Comments[r];
          Comments[r] := h;
          inc(l);
          dec(r);
        End;
      End;
      QuickLines(li, r);
      QuickLines(l, re);
    End;
  End;

Var
  sl: TStringList;

  Procedure Export(aFrom, aTo: integer);
  Var
    i: Integer;
    s: String;
  Begin
    QuickLines(aFrom, aTo);
    (*
    A1;B1;"C1
    2. Zeile";"D1;Bl""ub"
    A2;B2;C2;D1

    *)
    For i := aFrom To aTo Do Begin
      s := Comments[i].Filename + ';' + inttostr(Comments[i].Line) + ';"' + StringReplace(Comments[i].Comment, '"', '""', [rfReplaceAll]) + '"';
      sl.add(s);
    End;
  End;

Var
  startindex, Endindex: Integer;
Begin
  result := false;
  sl := TStringList.Create;
  sl.add('Filename;Line;Comment');
  // 1. Sortieren nach Dateinamen
  QuickFilename(0, high(Comments));
  startindex := 0;
  Endindex := 1;
  While Endindex <= high(Comments) Do Begin
    If Comments[startindex].Filename <> Comments[Endindex].Filename Then Begin
      Export(startindex, Endindex);
      startindex := Endindex + 1;
    End;
    Endindex := Endindex + 1;
  End;
  Endindex := min(Endindex, high(Comments));
  Export(startindex, Endindex);
  // 2. Exportieren Nach Dateinamen
  sl.SaveToFile(Filename);
  sl.free;
  result := true;
End;

Function FilenameIsPascalUnit(Filename: String): Boolean;
(*
 * gibt den Index von Value in Values wieder, -1 wenn nicht enthalten
 *)
  Function StringInStrings(Value: String; Values: TStringArray): integer;
  Var
    i: Integer;
  Begin
    result := -1;
    For i := 0 To high(Values) Do Begin
      If value = values[i] Then Begin
        result := i;
        break;
      End;
    End;
  End;
Begin
  result := StringInStrings(lowercase(ExtractFileExt(Filename)), ['.lpr', '.pp', '.pas']) <> -1;
End;

Function NameInList(Const List: TProjectFilesInfo; aName: String): integer;
Var
  i: Integer;
  fn: String;
Begin
  result := -1;
  aName := lowercase(aName);
  For i := 0 To high(List) Do Begin
    fn := ExtractFileName(List[i].FileName);
    fn := LowerCase(fn);
    fn := ExtractFileNameWithoutExt(fn);
    If {List[i].Enabled And}(aName = fn) Then Begin
      result := i;
      break;
    End;
  End;
End;

Function FixPathDelims(FilePath: String): String;
Var
  i: Integer;
Begin
  result := FilePath;
  For i := 1 To length(result) Do Begin
    If result[i] In AllowDirectorySeparators Then result[i] := PathDelim;
  End;
End;

Function ConcatRelativePath(Const BaseName: String; Const RelativeName: String
  ): String;
Var
  pre, suf: String;
  i: integer;
Begin
  (*
   * BaseName = d:\asdasd\asdas
   * RelativeName = ..\..\temp
   * Erg: d:\Temp
   *)
  result := IncludeTrailingPathDelimiter(BaseName) + RelativeName;
  i := pos('..', result);
  (*
   * Die Idee wir suchen immer nach "..\" blöcken und ersetzen diese Inplace
   * Dies ermöglicht verschieden "Krüppelige" Verzeichnisse Korrekt auf zu lösen
   *)
  While i <> 0 Do Begin
    Pre := copy(result, 1, i - 1); // d:\asdasd\asdas\
    Suf := copy(result, i + 2, length(result)); //\..\temp
    If (Length(suf) = 0) Or (length(pre) = 0) Then exit; // Irgendwas ist hier Falsch !
    If (Not (pre[length(pre)] In AllowDirectorySeparators)) Then exit; // Irgendwas ist hier Falsch !
    If (Not (Suf[1] In AllowDirectorySeparators)) Then exit; // Irgendwas ist hier Falsch !
    Pre := ExcludeTrailingPathDelimiter(pre);
    pre := ExtractFilePath(pre);
    result := pre + copy(suf, 2, length(suf));
    i := pos('..', result);
  End;
End;

Function RelativeFileListToAbsoluteFileList(RootFolder: String;
  FileList: TFileList): TFileList;
Var
  i: Integer;
Begin
  result := Nil;
  setlength(result, length(FileList));
  For i := 0 To high(FileList) Do Begin
    result[i] := FileList[i];
    result[i].FileName := ConcatRelativePath(RootFolder, result[i].FileName);
  End;
End;

Function RelativePathListToAbsolutePathList(RootFolder: String;
  PathList: TPathList): TPathList;
Var
  i: Integer;
Begin
  result := Nil;
  setlength(result, length(PathList));
  For i := 0 To high(PathList) Do Begin
    result[i] := PathList[i];
    result[i].Path := ConcatRelativePath(RootFolder, IncludeTrailingPathDelimiter(result[i].Path));
  End;
End;

Function GetFileList(RootFolder, LPIFile: String; FileList: TFileList
  ): TFileList;

  Procedure Quick(li, re: integer);
  Var
    l, r: Integer;
    p: String;
    h: TProjectFile;
  Begin
    If Li < Re Then Begin
      p := result[Trunc((li + re) / 2)].FileName; // Auslesen des Pivo Elementes
      l := Li;
      r := re;
      While l < r Do Begin
        While CompareStr(result[l].FileName, p) < 0 Do
          inc(l);
        While CompareStr(result[r].FileName, p) > 0 Do
          dec(r);
        If L <= R Then Begin
          h := result[l];
          result[l] := result[r];
          result[r] := h;
          inc(l);
          dec(r);
        End;
      End;
      quick(li, r);
      quick(l, re);
    End;
  End;

Var
  i, j: Integer;
  lpi: TDOMXML;
  uf, lpiRootFolder, f, nf: String;
  ip, fn, units, unitfile: TDomNode;
Begin
  result := Nil;
  // 1. Alle Dateien die nicht aus dem LPIFile kommen direkt übernehmen
  For i := 0 To high(FileList) Do Begin
    If Not FileList[i].FromLPI Then Begin
      SetLength(Result, high(Result) + 2);
      Result[high(Result)] := FileList[i];
      Result[high(Result)].Enabled := FileExists(FileList[i].FileName) And FileList[i].Enabled;
    End;
  End;
  // 2. Alle Dateien aus dem LPIfile übernehmen und ggf die weg streichen, die in FileList deaktiviert sind
  If (LPIFile <> '') Then Begin
    nf := ConcatRelativePath(RootFolder, LPIFile);
    If Not FileExists(nf) Then exit;
    lpi := TDOMXML.Create;
    lpi.LoadFromFile(nf);
    lpiRootFolder := IncludeTrailingPathDelimiter(ExtractFilePath(nf));
    // 2. Die Units extrahieren
    units := lpi.DocumentElement.FindPath('CONFIG.ProjectOptions.units');
    unitfile := Nil;
    If assigned(units) Then unitfile := units.FirstChild;
    While assigned(unitfile) Do Begin
      fn := unitfile.FindNode('Filename');
      f := '';
      If assigned(fn) Then f := fn.AttributeValue['Value'];
      ip := unitfile.FindNode('IsPartOfProject');
      If assigned(ip) Then Begin // Wenn die Datei nicht Teil des Projektes ist, Platt machen.
        If lowercase(ip.AttributeValue['Value']) <> 'true' Then f := '';
      End
      Else Begin
        f := '';
      End;
      (*
       * Nur offensichtliche Code Files zu lassen
       *)
      If Not FilenameIsPascalUnit(f) Then f := '';
      If f <> '' Then Begin
        uf := ConcatRelativePath(lpiRootFolder, f);
        setlength(result, high(Result) + 2);
        result[high(Result)].FromLPI := true;
        result[high(Result)].FileName := FixPathDelims(uf);
        result[high(Result)].Enabled := FileExists(result[high(Result)].FileName);
      End;
      unitfile := units.NextSibling;
    End;
    lpi.free;
  End;
  // 3. GGF Sperren der unerwünschten .lpi Dateien
  For i := 0 To high(FileList) Do Begin
    If FileList[i].FromLPI And (Not FileList[i].Enabled) Then Begin
      For j := 0 To high(Result) Do Begin
        If Result[j].FileName = FileList[i].FileName Then Begin
          Result[j].Enabled := false;
          break;
        End;
      End;
    End;
  End;
  // Sortieren der Liste Alphabetisch für Deterministische Ergebnisse ;)
  Quick(0, high(result));
End;

Function GetSearchPathList(RootFolder, LPIFile: String; PathList: TPathList
  ): TPathList;
Var
  sa: TStringArray;
  i, cnt: Integer;
  nf: String;
Begin
  // 1. die Händischen Übernehmen
  result := Nil;
  For i := 0 To high(PathList) Do Begin
    If Not PathList[i].FromLPI Then Begin
      setlength(result, length(result) + 1);
      result[high(result)].FromLPI := false;
      result[high(result)].Path := PathList[i].Path;
    End;
  End;
  // 2. die aus dem Projekt übernehmen
  nf := ConcatRelativePath(RootFolder, LPIFile);
  If Not FileExists(nf) Then exit;
  sa := GetSearchPathsFromLPIFile(nf);
  cnt := length(result);
  setlength(result, cnt + length(sa));
  For i := 0 To high(sa) Do Begin
    result[cnt + i].FromLPI := true;
    result[cnt + i].Path := ConcatRelativePath(RootFolder, IncludeTrailingPathDelimiter(sa[i]));
  End;
End;

{ TProject }

Constructor TProject.Create;
Begin
  Inherited Create;
  fLineComments := Nil;
  Clear;
End;

Destructor TProject.Destroy;
Begin
  Clear;
End;

Procedure TProject.Clear;
Begin
  fFilename := '';
  fChanged := false;
  // General
  fGeneral.ProjectName := 'New Project';
  // FFIles
  fFiles.LPISource := '';
  fFiles.RootFolder := '';
  fFiles.Files := Nil;
  fCCColors.LevelGood := Default_CCLevelGood;
  fCCColors.ColorGood := Default_CCLevelGoodColor;
  fCCColors.LevelModerate := Default_CCLevelModerate;
  fCCColors.ColorModerate := Default_CCLevelModerateColor;
  fCCColors.LevelComplex := Default_CCLevelComplex;
  fCCColors.ColorComplex := Default_CCLevelComplexColor;
  fCCColors.ColorUnstable := Default_CCLevelUnstableColor;
  ChartStatisticSettings.BoarderForLargestFunction := Default_BoarderForLargestFunction;
  ChartStatisticSettings.BoarderForLargestFiles := Default_BoarderForLargestFiles;
  ChartStatisticSettings.BoarderForMostComplexFunction := fCCColors.LevelGood;
  ChartStatisticSettings.BoarderForAverageMostComplexFiles := Default_BoarderForAverageMostComplexFiles;
  FSearchPaths := Nil;
  setlength(fLineComments, 0);
End;

Procedure TProject.RemoveLineComment(aFilename: String; line: Integer);
Var
  i, j: Integer;
Begin
  For i := 0 To high(fLineComments) Do Begin
    If (fLineComments[i].Line = line) And (fLineComments[i].Filename = aFilename) Then Begin
      For j := i To high(fLineComments) - 1 Do Begin
        fLineComments[j] := fLineComments[j + 1];
      End;
      setlength(fLineComments, high(fLineComments));
      fChanged := true;
      exit;
    End;
  End;
End;

Procedure TProject.SetLineComment(aFilename: String; line: Integer;
  Comment: String);
Begin
  RemoveLineComment(aFilename, line);
  setlength(fLineComments, high(fLineComments) + 2);
  fLineComments[high(fLineComments)].Line := line;
  fLineComments[high(fLineComments)].Filename := aFilename;
  fLineComments[high(fLineComments)].Comment := Comment;
  fChanged := true;
End;

Function TProject.GetLineComments(): TLineComments;
Var
  i: Integer;
Begin
  setlength(result, length(fLineComments));
  For i := 0 To high(Result) Do Begin
    result[i] := fLineComments[i];
  End;
End;

Procedure TProject.SetName(AValue: String);
Begin
  If AValue <> fGeneral.ProjectName Then Begin
    fChanged := true;
    fGeneral.ProjectName := AValue;
  End;
End;

Procedure TProject.SetLPISource(AValue: String);
Begin
  If AValue <> fFiles.LPISource Then Begin
    fChanged := true;
    fFiles.LPISource := AValue;
  End;
End;

Procedure TProject.SetFiles(AValue: TFileList);
Var
  i: integer;
Begin
  If length(AValue) <> length(fFiles.Files) Then Begin
    fFiles.Files := AValue;
    fChanged := true;
    exit;
  End;
  // Die Längen sind Gleich, da müssen wir genauer hin sehen
  For i := 0 To high(AValue) Do Begin
    If fFiles.Files[i].FileName <> AValue[i].FileName Then Begin
      fFiles.Files[i].FileName := AValue[i].FileName;
      fChanged := true;
    End;
    If fFiles.Files[i].Enabled <> AValue[i].Enabled Then Begin
      fFiles.Files[i].Enabled := AValue[i].Enabled;
      fChanged := true;
    End;
    If fFiles.Files[i].FromLPI <> AValue[i].FromLPI Then Begin
      fFiles.Files[i].FromLPI := AValue[i].FromLPI;
      fChanged := true;
    End;
  End;
End;

Procedure TProject.SetCCColors(AValue: TCCColors);
Begin
  fCCColors := AValue;
  fChanged := true;
End;

Procedure TProject.SetRootFolder(AValue: String);
Begin
  If AValue <> fFiles.RootFolder Then Begin
    fChanged := true;
    fFiles.RootFolder := AValue;
  End;
End;

Function TProject.LoadFromFile(aFilename: String): boolean;
Var
  ini: TIniFile;
  cnt, i: Integer;
  OSstring: String;
Begin
  result := false;
  Clear;
  If Not FileExists(aFilename) Then Begin
    showmessage('Error, could not find: ' + aFilename);
  End;
  ini := TIniFile.Create(aFilename);
  // General
  fGeneral.ProjectName := ini.ReadString('General', 'Name', '');

  fCCColors.LevelGood := ini.ReadInteger('General', 'CCLevelGood', fCCColors.LevelGood);
  fCCColors.LevelModerate := ini.ReadInteger('General', 'CCLevelModerate', fCCColors.LevelModerate);
  fCCColors.LevelComplex := ini.ReadInteger('General', 'CCLevelComplex', fCCColors.LevelComplex);
  fCCColors.ColorGood := StringToColor(ini.ReadString('General', 'CCColorGood', ColorToString(fCCColors.ColorGood)));
  fCCColors.ColorModerate := StringToColor(ini.ReadString('General', 'CCColorModerate', ColorToString(fCCColors.ColorModerate)));
  fCCColors.ColorComplex := StringToColor(ini.ReadString('General', 'CCColorComplex', ColorToString(fCCColors.ColorComplex)));
  fCCColors.ColorUnstable := StringToColor(ini.ReadString('General', 'CCColorUnstable', ColorToString(fCCColors.ColorUnstable)));
  ChartStatisticSettings.BoarderForLargestFunction := ini.ReadInteger('ChartStatistiks', 'BoarderForLargestFunction', ChartStatisticSettings.BoarderForLargestFunction);
  ChartStatisticSettings.BoarderForLargestFiles := ini.ReadInteger('ChartStatistiks', 'BoarderForLargestFiles', ChartStatisticSettings.BoarderForLargestFiles);
  ChartStatisticSettings.BoarderForMostComplexFunction := ini.ReadInteger('ChartStatistiks', 'BoarderForMostComplexFunction', ChartStatisticSettings.BoarderForMostComplexFunction);
  ChartStatisticSettings.BoarderForAverageMostComplexFiles := ini.ReadInteger('ChartStatistiks', 'BoarderForAverageMostComplexFiles', ChartStatisticSettings.BoarderForAverageMostComplexFiles);

  // Files
  OSstring := GetOSIdentiferString();
  fFiles.RootFolder := ini.ReadString('Files', 'RootFolder' + OSstring, '');
  If fFiles.RootFolder = '' Then Begin
    // Retry with "old" Style..
    fFiles.RootFolder := ini.ReadString('Files', 'RootFolder', '');
  End;
  fFiles.LPISource := ini.ReadString('Files', 'LPISource', '');
  cnt := ini.readInteger('Files', 'Count', 0);

  (*
   * Check if Files exist, but Rootfolder is invalid
   *)
  fChanged := false;
  If (cnt <> 0) Or (fFiles.LPISource <> '') Then Begin
    If (fFiles.RootFolder = '') Or ((fFiles.RootFolder <> '') And Not DirectoryExistsUTF8(fFiles.RootFolder)) Then Begin
      ShowMessage('Your project root folder seems to be invalid (old value was: "' + fFiles.RootFolder + '"), please select a correct one for: ' + fGeneral.ProjectName);
      fFiles.RootFolder := '';
      If SelectDirectory('Root folder for: ' + fGeneral.ProjectName, '', fFiles.RootFolder) Then
        fChanged := true;
    End;
  End;

  setlength(fFiles.Files, cnt);
  For i := 0 To high(fFiles.Files) Do Begin
    ffiles.Files[i].FileName := FixPathDelims(ini.ReadString('Files', 'File' + inttostr(i), ''));
    ffiles.Files[i].Enabled := ini.ReadBool('Files', 'Enabled' + inttostr(i), false);
    ffiles.Files[i].FromLPI := ini.ReadBool('Files', 'FromLPI' + inttostr(i), false);
  End;

  cnt := ini.readInteger('Files', 'SearchpathCount', 0);
  setlength(FSearchPaths, cnt);
  For i := 0 To high(FSearchPaths) Do Begin
    FSearchPaths[i].Path := FixPathDelims(ini.ReadString('Files', 'SearchPath' + inttostr(i), ''));
    FSearchPaths[i].FromLPI := false;
  End;

  // LineComments
  cnt := ini.readInteger('LineComments', 'Count', 0);
  setlength(fLineComments, cnt);
  For i := 0 To high(fLineComments) Do Begin
    fLineComments[i].Line := ini.ReadInteger('LineComments', 'Comment' + inttostr(i) + 'Line', -1);
    fLineComments[i].Filename := ini.ReadString('LineComments', 'Comment' + inttostr(i) + 'File', '');
    fLineComments[i].Comment := DeSerialize(ini.ReadString('LineComments', 'Comment' + inttostr(i) + 'Comment', ''));
  End;

  // Weiter

  fFilename := aFilename;
  ini.Free;

  result := true;
End;

Procedure TProject.SaveToFile(aFilename: String);
Var
  ini: TIniFile;
  cnt, i: integer;
  RootLinux, RootWindows, RootDarwin: String;
Begin
  ini := TIniFile.Create(aFilename);
  ini.CacheUpdates := true;
  fFilename := aFilename;
  // General
  ini.WriteString('General', 'Name', fGeneral.ProjectName);
  ini.WriteInteger('General', 'CCLevelGood', fCCColors.LevelGood);
  ini.WriteInteger('General', 'CCLevelModerate', fCCColors.LevelModerate);
  ini.WriteInteger('General', 'CCLevelComplex', fCCColors.LevelComplex);
  ini.WriteString('General', 'CCColorGood', ColorToString(fCCColors.ColorGood));
  ini.WriteString('General', 'CCColorModerate', ColorToString(fCCColors.ColorModerate));
  ini.WriteString('General', 'CCColorComplex', ColorToString(fCCColors.ColorComplex));
  ini.WriteString('General', 'CCColorUnstable', ColorToString(fCCColors.ColorUnstable));
  ini.WriteInteger('ChartStatistiks', 'BoarderForLargestFunction', ChartStatisticSettings.BoarderForLargestFunction);
  ini.WriteInteger('ChartStatistiks', 'BoarderForLargestFiles', ChartStatisticSettings.BoarderForLargestFiles);
  ini.WriteInteger('ChartStatistiks', 'BoarderForMostComplexFunction', ChartStatisticSettings.BoarderForMostComplexFunction);
  ini.WriteInteger('ChartStatistiks', 'BoarderForAverageMostComplexFiles', ChartStatisticSettings.BoarderForAverageMostComplexFiles);
  // Files
  // Retten der OS-Root folders
  RootLinux := ini.ReadString('Files', 'RootFolderLinux', '');
  RootWindows := ini.ReadString('Files', 'RootFolderWindows', '');
  RootDarwin := ini.ReadString('Files', 'RootFolderDarwin', '');
  ini.EraseSection('Files'); // Sonst überleben die "Obsoleten" Teile
  // Wieder herstellen der OS-Root folders
  If RootLinux <> '' Then ini.WriteString('Files', 'RootFolderLinux', RootLinux);
  If RootWindows <> '' Then ini.WriteString('Files', 'RootFolderWindows', RootWindows);
  If RootDarwin <> '' Then ini.WriteString('Files', 'RootFolderDarwin', RootDarwin);
  ini.WriteString('Files', 'RootFolder' + GetOSIdentiferString(), fFiles.RootFolder);
  ini.WriteString('Files', 'LPISource', fFiles.LPISource);
  cnt := 0;
  For i := 0 To high(fFiles.Files) Do Begin
    If (fFiles.Files[i].FromLPI And (Not fFiles.Files[i].Enabled)) Or
      (Not fFiles.Files[i].FromLPI) Then Begin
      ini.WriteString('Files', 'File' + inttostr(cnt), fFiles.Files[i].FileName);
      ini.WriteBool('Files', 'Enabled' + inttostr(cnt), fFiles.Files[i].Enabled);
      ini.WriteBool('Files', 'FromLPI' + inttostr(cnt), fFiles.Files[i].FromLPI);
      inc(cnt);
    End;
  End;
  ini.WriteInteger('Files', 'Count', cnt);
  cnt := 0;
  For i := 0 To high(FSearchPaths) Do Begin
    If Not FSearchPaths[i].FromLPI Then Begin
      ini.WriteString('Files', 'SearchPath' + inttostr(cnt), FSearchPaths[i].Path);
      inc(cnt);
    End;
  End;
  ini.WriteInteger('Files', 'SearchpathCount', cnt);

  // LineComments
  ini.WriteInteger('LineComments', 'Count', length(fLineComments));
  For i := 0 To high(fLineComments) Do Begin
    ini.WriteInteger('LineComments', 'Comment' + inttostr(i) + 'Line', fLineComments[i].Line);
    ini.WriteString('LineComments', 'Comment' + inttostr(i) + 'File', fLineComments[i].Filename);
    ini.WriteString('LineComments', 'Comment' + inttostr(i) + 'Comment', Serialize(fLineComments[i].Comment));
  End;

  // weiter

  ini.UpdateFile;
  ini.Free;
  fChanged := false;
End;

Procedure TProject.Change;
Begin
  fChanged := true;
End;

End.

