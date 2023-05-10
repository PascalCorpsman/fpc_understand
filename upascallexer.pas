(******************************************************************************)
(* upascallexer                                                    19.04.2023 *)
(*                                                                            *)
(* Version     : 0.03                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : Lexes a inputstream or file into pascaltokens                *)
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
(*               0.02 - Add Total Line Counter, Empty Line Counter            *)
(*               0.03 - Start with unittests, fix "invalid" ( * Parsing       *)
(*                                                                            *)
(******************************************************************************)
Unit upascallexer;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils;

Type

  TTokenKind =
    (
    // Theoretically this could be extended with tkSpace, tkLinebreak, tkTab (would be needed if TPascalLexer would be used for a code formater)
    tkIdentifier,
    tkString,
    tkCompilerDirective,
    tkKeyWord,
    tkComment,
    tkOperator
    );

  TToken = Record
    Value: String;
    Line: integer;
    Kind: TTokenKind;
  End;

  TOnHandleToken = Procedure(Sender: TObject; Const Token: TToken) Of Object;

  { TPascalLexer }

  TPascalLexer = Class
  private
    aToken: String;
    aLine: Integer;
    aEmptyLine: integer;
    Procedure HandleToken();
    Procedure DoLex(Const Stream: TStream);
  public

    OnHandleToken: TOnHandleToken;

    Property TotalLineCount: integer read aLine;
    Property EmptyLineCount: integer read aEmptyLine;

    Constructor Create(); virtual;

    Procedure LexFile(Const Filename: String);
    Procedure LexStream(Const Stream: TStream);
  End;

Function TokenKindToString(Const Kind: TTokenKind): String;
Function IsKeyWord(Value: String): Boolean;

Implementation

Const
  Separators = [#0..' '];
  Operators = [';', ',', '[', ']', ')', '=', '+', '-', '*', '^', '@']; // tkOperator (missing the ones that are handled directly in the satemachine
{$IFDEF Darvin}
  LB = #13;
{$ELSE}
  LB = #10;
{$ENDIF}

  (*
   * Perfectly sorted list of FPC-Keywords, otherwise IsKeyWord will fail !
   *)
  KeyWords: Array[0..87] Of String =
  (
    'absolute',
    'alias',
    'and',
    'array',
    'as',
    'asm',
    'assembler',
    'begin',
    'break',
    'case',
    'cdecl',
    'class',
    'const',
    'constructor',
    'continue',
    'destructor',
    'div',
    'do',
    'downto',
    'else',
    'end',
    'except',
    'export',
    'exports',
    'external',
    'file',
    'finalization',
    'finally',
    'for',
    'forward',
    'function',
    'generic',
    'goto',
    'if',
    'implementation',
    'in',
    'inherited',
    'initialization',
    'inline',
    'interface',
    'is',
    'label',
    'library',
    'mod',
    'nil',
    'nostackframe',
    'not',
    'object',
    'of',
    'on',
    'operator',
    'or',
    'out',
    'override',
    'packed',
    'pascal',
    'private',
    'procedure',
    'program',
    'property',
    'protected',
    'public',
    'published',
    'raise',
    'record',
    'reintroduce',
    'repeat',
    'register',
    'safecall',
    'set',
    'shl',
    'shr',
    'specialize',
    'stdcall',
    'string',
    'then',
    'threadvar',
    'to',
    'try',
    'type',
    'unit',
    'until',
    'uses',
    'var',
    'virtual',
    'while',
    'with',
    'xor'
    );

Procedure Nop();
Begin

End;

Function IsKeyWord(Value: String): Boolean;
Var
  lborder, uborder: Integer;
  index: SizeInt;
Begin
  // Binary Search
  result := false;
  value := lowercase(Value);
  lborder := 0;
  index := 1; // Egal hauptsache <> lborder;
  uborder := high(KeyWords);
  While lborder <> uborder Do Begin
    index := (lborder + uborder) Div 2;
    If KeyWords[index] = Value Then exit(True);
    If CompareStr(value, KeyWords[index]) > 0 Then Begin
      lborder := index + 1;
    End
    Else Begin
      uborder := index;
    End;
  End;
  // if lborder = uborder, then we need to test this last keyword !
  result := KeyWords[lborder] = Value;
End;

Function TokenKindToString(Const Kind: TTokenKind): String;
Begin
  result := '';
  Case Kind Of
    tkIdentifier: result := 'Identifier';
    tkString: result := 'String';
    tkCompilerDirective: result := 'CompilerDirective';
    tkKeyWord: result := 'KeyWord';
    tkComment: result := 'Comment';
    tkOperator: result := 'Operator';
  Else Begin
      Raise exception.create('TokenKindToString: error missing implementation.');
    End;
  End;
End;

{ TPascalLexer }

Constructor TPascalLexer.Create;
Begin
  Inherited Create();
  OnHandleToken := Nil;
End;

Procedure TPascalLexer.HandleToken;
Const
  FullOperators = Operators + [':', '(', '/', '.', '<', '>']; // Missing all 2 Char wide operators
Var
  Token: TToken;
Begin
  Token.Value := aToken;
  Token.Line := aLine;
  Token.Kind := tkIdentifier;
  If pos('{$', aToken) = 1 Then Begin
    Token.Kind := tkCompilerDirective;
  End
  Else Begin
    If atoken[1] = '''' Then Begin
      // a String
      Token.Kind := tkString;
    End
    Else Begin
      If (pos('//', aToken) = 1) Or (pos('(*', aToken) = 1) Or (aToken[1] = '{') Then Begin
        Token.Kind := tkComment;
      End
      Else Begin
        // All the other things (identifier, keyword, ...)
        If (aToken[1] In FullOperators)
          Or (aToken = ':=')
          Or (aToken = '..') // Der Tritt Praktisch nicht auf weil atoken[1] = '.' und dass ist ja in FullOperators
        Or (aToken = '<=') Or (aToken = '<>') Or (aToken = '>=')
          Then Begin
          Token.Kind := tkOperator;
        End
        Else Begin
          If IsKeyWord(aToken) Then Begin
            Token.Kind := tkKeyWord;
          End;
        End;
      End;
    End;
  End;
  OnHandleToken(self, token);
End;

(*
 * This Lexer goes through a FreePascal SourceCode Stream and
 * Splits it into Tokens and pass them to "OnHandleToken"
 * it supports nested comments like { { } }
 *
 * It's simplified statemachine (only states and transmissions not the reason for the transmissions) looks like this:
 ' make visible with: https://plantuml.com/de/
 @startuml
hide empty description

[*] -right-> sCollectToken

sCollectToken -down-> sBraceComment: '{'
sCollectToken -down-> sFirstDash: '/'
sCollectToken -down-> sFirstBrace: '('
sCollectToken -up-> sColon: ':'
sCollectToken -up-> sString: '''
sCollectToken -left-> sCollectToken: *
sCollectToken -up-> sDot: .
sCollectToken -up-> sLT: <
sCollectToken -up-> sGT: >

sBraceComment -> sBraceComment: '{', '}', *
sBraceComment -> sCollectToken : '}'
sBraceComment -down-> sCompilerDirective: '$'

sCompilerDirective -> sCollectToken : '}'
sCompilerDirective -> sCompilerDirective :*

sFirstDash -down-> sDashComment: '/'
sFirstDash -> sCollectToken: *

sDashComment -> sDashComment: *
sDashComment -> sCollectToken: NewLine

sFirstBrace -down-> sBraceStarComment: '*'
sFirstBrace -> sCollectToken: *

sBraceStarComment -down-> sFirstStar: '*'
sBraceStarComment -left-> sBraceStarCommentBrace: '('
sBraceStarComment -> sBraceStarComment: *

sBraceStarCommentBrace -> sBraceStarComment: '*', *

sFirstStar -> sCollectToken: ')'
sFirstStar -> sBraceStarComment : ')',*

sColon -> sCollectToken: '=', *

sString -left-> sStringTerm: '''
sString -> sString: *

sStringTerm -> sString: '''
sStringTerm -> sCollectToken: *

sDot -> sCollectToken: '.',*

sLT -> sCollectToken: '>','=',*

sGT -> sCollectToken: '=',*

@enduml
 *)

Procedure TPascalLexer.DoLex(Const Stream: TStream);

  Procedure HToken()Inline;
  Begin
    If atoken <> '' Then Begin // Der Token vor dem ersten erkannten Token ist in der Regel leer -> Raus werfen.
      HandleToken();
      aToken := '';
    End;
  End;

Type
  tstate =
    (
    sCollectToken // Sammelt die Zeichenkette für das nächste Token auf
    , sBraceComment // der { Kommentar
    , sFirstDash // Auf dem Weg zum //
    , sDashComment // der // Kommentar
    , sFirstBrace // auf dem Weg zum (*
    , sBraceStarComment // der (* Kommentar
    , sBraceStarCommentBrace // eine weitere ( in einem (* kommentar
    , sFirstStar // Raus aus sBraceStarComment
    , sString // In einem String
    , sStringTerm // Ein Abgeschlossener String
    , sColon // Entweder : als Token oder der Begin von :=
    , sCompilerDirective // {$ ** }
    , sDot // Zum Erkennen von ..
    , slt // Zum Erkennen < oder <>, <=,
    , sgt // zum Erkennen > oder >=
    );

Var
  State: tstate;
  ppc, pc, c: Char;
  CommentDetphCounter: Integer;
  i, mSize: Int64;
Begin
  aToken := '';
  State := sCollectToken;
  CommentDetphCounter := 0;
  pc := #0;
  c := #0;
  aLine := 1; // Textdateien sind 1 Basiert
  aEmptyLine := 0;
  i := 0;
  mSize := Stream.Size;
  While i < mSize Do Begin
    ppc := pc;
    pc := c;
    Stream.Read(c, sizeof(c));
    Case State Of
      sCollectToken: Begin
          Case c Of
            '{': Begin
                HToken();
                CommentDetphCounter := 1;
                aToken := c;
                State := sBraceComment;
              End;
            '/': State := sFirstDash;
            '(': Begin
                HToken;
                state := sFirstBrace;
              End;
            ':': Begin
                HToken;
                state := sColon;
              End;
            '.': Begin
                HToken;
                state := sDot;
              End;
            '<': Begin
                HToken;
                state := slt;
              End;
            '>': Begin
                HToken;
                state := sgt;
              End;
            '''': Begin
                HToken;
                State := sString;
              End;
          Else Begin
              If c In Separators Then Begin
                HToken;
              End
              Else Begin
                If c In Operators Then Begin
                  HToken;
                  aToken := c;
                  HToken;
                End
                Else Begin
                  aToken := aToken + c;
                End;
              End;
            End;
          End;
        End;
      sBraceComment: Begin
          aToken := aToken + c;
          Case c Of
            '{': Inc(CommentDetphCounter);
            '$': Begin
                If (CommentDetphCounter = 1) And (pc = '{') Then Begin
                  State := sCompilerDirective;
                  CommentDetphCounter := 0;
                  aToken := '{$';
                End;
              End;
            '}': Begin
                dec(CommentDetphCounter);
                If (CommentDetphCounter = 0) Then Begin
                  HToken;
                  state := sCollectToken;
                End;
              End;
          End;
        End;
      sCompilerDirective: Begin
          aToken := aToken + c;
          If c = '}' Then Begin
            HToken;
            State := sCollectToken;
          End;
        End;
      sFirstDash: Begin
          If c = '/' Then Begin
            State := sDashComment;
            aToken := '//';
          End
          Else Begin
            aToken := '/';
            HToken;
            // Put the "lookahead" back to the Readqueue and treat it as not "readed"
            Stream.Position := Stream.Position - 1;
            dec(i);
            State := sCollectToken;
          End;
        End;
      sDashComment: Begin
          If c = LB Then Begin
            HToken;
            State := sCollectToken;
          End
          Else Begin
            aToken := aToken + c;
          End;
        End;
      sFirstBrace: Begin
          If c = '*' Then Begin
            CommentDetphCounter := 1;
            State := sBraceStarComment;
            aToken := '(*';
          End
          Else Begin
            aToken := '(';
            HToken;
            // Put the "lookahead" back to the Readqueue and treat it as not "readed"
            Stream.Position := Stream.Position - 1;
            dec(i);
            State := sCollectToken;
          End;
        End;
      sBraceStarComment: Begin
          aToken := aToken + c;
          Case c Of
            '*': State := sFirstStar;
            '(': State := sBraceStarCommentBrace;
          End;
        End;
      sBraceStarCommentBrace: Begin
          aToken := aToken + c;
          If c = '*' Then Begin
            inc(CommentDetphCounter);
          End;
          If c <> '(' Then Begin // if we have a case like (* .. ((((((* **) we need to stay in this case ;)
            State := sBraceStarComment;
          End;
        End;
      sFirstStar: Begin
          aToken := aToken + c;
          If c = ')' Then Begin
            dec(CommentDetphCounter);
            If CommentDetphCounter = 0 Then Begin
              HToken;
              State := sCollectToken;
            End
            Else Begin
              State := sBraceStarComment;
            End;
          End
          Else Begin
            If c <> '*' Then Begin // If we have a case like (******) then stay in First star as each star could be the last !
              State := sBraceStarComment;
            End;
          End;
        End;
      sColon: Begin
          If c = '=' Then Begin
            aToken := ':=';
            HToken;
            State := sCollectToken;
          End
          Else Begin
            aToken := ':';
            HToken;
            // Put the "lookahead" back to the Readqueue and treat it as not "readed"
            Stream.Position := Stream.Position - 1;
            dec(i);
            State := sCollectToken;
          End;
        End;
      slt: Begin
          If c In ['>', '='] Then Begin
            aToken := '<' + c;
            HToken;
            State := sCollectToken;
          End
          Else Begin
            aToken := '<';
            HToken;
            // Put the "lookahead" back to the Readqueue and treat it as not "readed"
            Stream.Position := Stream.Position - 1;
            dec(i);
            State := sCollectToken;
          End;
        End;
      sgt: Begin
          If c = '=' Then Begin
            aToken := '>=';
            HToken;
            State := sCollectToken;
          End
          Else Begin
            aToken := '>';
            HToken;
            // Put the "lookahead" back to the Readqueue and treat it as not "readed"
            Stream.Position := Stream.Position - 1;
            dec(i);
            State := sCollectToken;
          End;
        End;
      sDot: Begin
          If c = '.' Then Begin
            aToken := '..';
            HToken;
            State := sCollectToken;
          End
          Else Begin
            aToken := '.';
            HToken;
            // Put the "lookahead" back to the Readqueue and treat it as not "readed"
            Stream.Position := Stream.Position - 1;
            dec(i);
            State := sCollectToken;
          End;
        End;
      sString: Begin
          (*
           * String handling is a bit tricky
           * a normal String like 'Hello World' is easy
           * But FPC supports ' to be insid a string
           * this is done with a double ' within the string.
           * To recognize this we need a further "Leaving" state
           *)
          If c = '''' Then Begin
            State := sStringTerm;
          End
          Else Begin
            If c = LB Then Begin
              Raise exception.Create('Error, not terminated string in line: ' + inttostr(aLine));
            End;
            aToken := aToken + c;
          End;
        End;
      sStringTerm: Begin
          If c = '''' Then Begin // The case with a ' inside a string
            atoken := atoken + c;
            State := sString;
          End
          Else Begin
            // The String terminated regularly
            aToken := '''' + aToken + '''';
            HToken;
            // Put the "lookahead" back to the Readqueue and treat it as not "readed"
            Stream.Position := Stream.Position - 1;
            dec(i);
            State := sCollectToken;
          End;
        End;
    Else Begin
        Raise exception.create('Error, missing state.');
      End;
    End;
    (*
     * This accepts lb, lb and lb,[#10,#13],lb as "empty" line
     *)
    If (c = lb) And ((pc = lb) Or ((ppc = lb) And (pc In [#10, #13]))) Then Begin
      inc(aEmptyLine);
    End;
    If c = LB Then Begin
      inc(aLine);
    End;
    inc(i);
  End;
  (* Wir haben die Datei zu ende gelesen, aber ggf müssen wir noch "Verbleibende" rest arbeiten aus geben.*)
  If aToken <> '' Then HToken; // Was auch immer da noch so steht, raus..
  Case State Of
    sDot: Begin // nach dem Letzten Punkt kam nix mehr.
        aToken := '.';
        HToken;
      End;
  End;
End;

Procedure TPascalLexer.LexFile(Const Filename: String);
Var
  m: TMemoryStream;
Begin
  m := TMemoryStream.Create;
  Try
    m.LoadFromFile(Filename);
    m.Position := 0;
    LexStream(m);
  Except
    m.free;
    Raise;
    exit;
  End;
  m.free;
End;

Procedure TPascalLexer.LexStream(Const Stream: TStream);
Begin
  If Not assigned(OnHandleToken) Then Begin
    Raise Exception.Create('Error, no handler defined.');
  End;
  DoLex(stream);
End;

End.

