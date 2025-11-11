unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, UpdaterUtils;

type
  TForm1 = class(TForm)
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  if ParamCount < 2 then
  begin
    ShowMessage('Uso: ' + ParamStr(0) + ' <exe_original> <exe_nuevo>');
    Exit;
  end;

  UpdateExecutable(ParamStr(1), ParamStr(2));
  Application.Terminate; // Cierra si se ejecutó desde línea de comandos
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  ShowMessage('¡Botón pulsado! Iniciando prueba...');

  UpdateExecutable(
    'C:\Apps\SHP\ActualizarEjecutable\Win32\Debug\1_OLD.exe',
    'C:\Apps\SHP\ActualizarEjecutable\Win32\Debug\1.exe',
    True, True
  );
end;

end.
