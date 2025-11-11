unit UpdaterUtils;

interface

uses
  Winapi.Windows, System.SysUtils, Winapi.TlHelp32, Winapi.ShellAPI,
  Vcl.Dialogs;

{==============================================================================}
{  UpdaterUtils - Utilidades para actualización segura de ejecutables         }
{==============================================================================}

/// <summary>
///   Verifica si un proceso está en ejecución (sin devolver PID).
/// </summary>
function ProcessExists(const ExeName: string): Boolean; overload;

/// <summary>
///   Verifica si un proceso está en ejecución y devuelve su PID.
/// </summary>
function ProcessExists(const ExeName: string; var PID: DWORD): Boolean; overload;

/// <summary>
///   Termina un proceso por PID.
/// </summary>
function TerminateProcessByPID(PID: DWORD): Boolean;

/// <summary>
///   Reemplaza un ejecutable en uso por uno nuevo.
/// </summary>
function ReplaceExecutable(const OldExePath, NewExePath: string): Boolean;

/// <summary>
///   Actualización completa con confirmaciones opcionales.
/// </summary>
function UpdateExecutable(const OldExePath, NewExePath: string;
  AskClose: Boolean = True; AskReplace: Boolean = True): Boolean;

implementation


{==============================================================================}

function ProcessExists(const ExeName: string): Boolean;
var
  DummyPID: DWORD;
begin
  Result := ProcessExists(ExeName, DummyPID);
end;

function ProcessExists(const ExeName: string; var PID: DWORD): Boolean;
var
  ContinueLoop: BOOL;
  FSnapshotHandle: THandle;
  FProcessEntry32: TProcessEntry32;
begin
  Result := False;
  FSnapshotHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  if FSnapshotHandle = INVALID_HANDLE_VALUE then
    Exit;

  try
    FProcessEntry32.dwSize := SizeOf(FProcessEntry32);
    ContinueLoop := Process32First(FSnapshotHandle, FProcessEntry32);

    while Integer(ContinueLoop) <> 0 do
    begin
      if SameText(ExtractFileName(FProcessEntry32.szExeFile), ExeName) then
      begin
        PID := FProcessEntry32.th32ProcessID;
        Result := True;
        Break;
      end;
      ContinueLoop := Process32Next(FSnapshotHandle, FProcessEntry32);
    end;
  finally
    CloseHandle(FSnapshotHandle);
  end;
end;

function TerminateProcessByPID(PID: DWORD): Boolean;
var
  hProcess: THandle;
begin
  Result := False;
  hProcess := OpenProcess(PROCESS_TERMINATE, False, PID);
  if hProcess = 0 then
    Exit;

  try
    Result := Winapi.Windows.TerminateProcess(hProcess, 0);
  finally
    CloseHandle(hProcess);
  end;
end;

function ReplaceExecutable(const OldExePath, NewExePath: string): Boolean;
var
  TempOldName: string;
begin
  Result := False;
  TempOldName := ChangeFileExt(OldExePath, '.old');

  try
    if not RenameFile(OldExePath, TempOldName) then
      raise Exception.Create('No se pudo renombrar el archivo original.');

    if not CopyFile(PWideChar(NewExePath), PWideChar(OldExePath), False) then
      raise Exception.Create('No se pudo copiar el nuevo ejecutable.');

    DeleteFile(TempOldName);
    Result := True;
  except
    on E: Exception do
    begin
      if FileExists(TempOldName) then
        RenameFile(TempOldName, OldExePath);
      raise;
    end;
  end;
end;

function UpdateExecutable(const OldExePath, NewExePath: string;
  AskClose, AskReplace: Boolean): Boolean;
var
  PID: DWORD;
  ExeName: string;
  Confirmed: Boolean;
begin
  Result := False;

  if not FileExists(OldExePath) then
  begin
    MessageDlg('No existe: ' + OldExePath, mtError, [mbOK], 0);
    Exit;
  end;

  if not FileExists(NewExePath) then
  begin
    MessageDlg('No existe: ' + NewExePath, mtError, [mbOK], 0);
    Exit;
  end;

  ExeName := ExtractFileName(OldExePath);

  if ProcessExists(ExeName, PID) then
  begin
    if AskClose then
    begin
      Confirmed := MessageDlg(Format('"%s" está en ejecución. ¿Cerrarlo?', [ExeName]),
                              mtConfirmation, [mbYes, mbNo], 0) = IDYES;
      if not Confirmed then Exit;
    end;

    if not TerminateProcessByPID(PID) then
    begin
      MessageDlg('Error al cerrar el proceso. Ejecuta como administrador.', mtError, [mbOK], 0);
      Exit;
    end;

    if AskClose then
      MessageDlg('Proceso cerrado.', mtInformation, [mbOK], 0);
  end;

  if AskReplace then
  begin
    Confirmed := MessageDlg('¿Reemplazar y ejecutar la nueva versión?', mtConfirmation, [mbYes, mbNo], 0) = IDYES;
    if not Confirmed then Exit;
  end;

  if ReplaceExecutable(OldExePath, NewExePath) then
  begin
    MessageDlg('Actualización exitosa.', mtInformation, [mbOK], 0);
    ShellExecute(0, 'open', PChar(OldExePath), nil, nil, SW_SHOW);
    Result := True;
  end
  else
    MessageDlg('Error al reemplazar.', mtError, [mbOK], 0);
end;

end.
