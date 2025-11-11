unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}


procedure TForm1.FormCreate(Sender: TObject);
var
  Size, Handle: DWORD;
  Buffer: Pointer;
  FileInfo: PVSFixedFileInfo;
  Ver: string;
begin
  // Obtiene el tamaño de la información de versión
  Size := GetFileVersionInfoSize(PChar(ParamStr(0)), Handle);
  if Size > 0 then
  begin
    GetMem(Buffer, Size);
    try
      // Obtiene la información de versión
      if GetFileVersionInfo(PChar(ParamStr(0)), Handle, Size, Buffer) then
        if VerQueryValue(Buffer, '\', Pointer(FileInfo), Size) then
          with FileInfo^ do
          begin
            Ver := Format('%d.%d.%d.%d',
              [HiWord(dwFileVersionMS), LoWord(dwFileVersionMS),
               HiWord(dwFileVersionLS), LoWord(dwFileVersionLS)]);
            Label1.Caption := 'Versión: ' + Ver;
          end;
    finally
      FreeMem(Buffer);
    end;
  end
  else
    Label1.Caption := 'Versión: desconocida';
end;

end.
