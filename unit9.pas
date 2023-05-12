Unit Unit9;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

Type

  { TForm9 }

  TForm9 = Class(TForm)
    Button1: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Procedure Button1Click(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
  private

  public
    Procedure LoadNode(aFileName, aClassName, aName: String);

  End;

Var
  Form9: TForm9;

Implementation

{$R *.lfm}

{ TForm9 }

Procedure TForm9.FormCreate(Sender: TObject);
Begin
  Constraints.MinHeight := Height;
  Constraints.MaxHeight := Height;
  Constraints.MinWidth := width;
End;

Procedure TForm9.Button1Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm9.LoadNode(aFileName, aClassName, aName: String);
Begin
  Edit1.Text := aFileName;
  Edit2.Text := aClassName;
  Edit3.Text := aName;
End;

End.

