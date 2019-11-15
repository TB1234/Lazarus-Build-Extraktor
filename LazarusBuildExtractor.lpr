program LazarusBuildExtractor;

{ <description>

  Copyright (C) 2019 Tobias Bauer <contact>

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 51 Franklin Street - Fifth Floor,
  Boston, MA 02110-1335, USA.
}


{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, CustApp, SysUtils, FileUtil, xmlconf;

type

  { TLazarusBuildExtractor }

  TLazarusBuildExtractor = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

{ TLazarusBuildExtractor }

procedure TLazarusBuildExtractor.DoRun;
var
  ErrorMsg: String;
  FileList: TStringList;
  lpiFileName: String;
  cfg: TXMLConfig;
  verFile: TextFile;
begin
  // falsche Anzahl von Parameter
  if ParamCount <> 1 then
  begin
    ErrorMsg := 'Falsche Anzahl an Parametern!' + LineEnding + LineEnding +
             ParamStr(0) + ' <Projektpfad>';
  end;

  // ungültiger Pfad
  if not DirectoryExists(ParamStr(1)) then
  begin
    ErrorMsg := 'Der angegebene Pfad ist ungültig!';
  end;

  // Verzeichnis nach .lpi-Datei scannen
  try
    FileList := FindAllFiles(ParamStr(1), '*.lpi', false);
    // keine .lpi-Datei gefunden
    if FileList.Count = 0 then
    begin
      ErrorMsg := 'Es wurde keine .lpi-Datei gefunden!';
    end
    // mehrere .lpi-Dateien gefunden
    else if FileList.Count > 1 then
    begin
      ErrorMsg := 'Es wurden mehrere .lpi-Dateien gefunden. (' +
               FileList.Count.ToString() + ')';
    end
    // Dateiname übernehmen
    else begin
      lpiFileName := FileList[0];
    end;
  finally
    FileList.Free;
  end;

  // Fehlerbehandlung
  if ErrorMsg <> '' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // Versionsdatei öffnen
  AssignFile(verFile, ExtractFileDir(lpiFileName) + PathDelim + 'version.txt');
  Rewrite(verFile);

  // Konfiguration einlesen und in Datei speichern
  cfg := TXMLConfig.Create(nil);
  cfg.Filename := lpiFileName;
  WriteLn(verFile,
    cfg.GetValue('ProjectOptions/VersionInfo/MajorVersionNr/Value', '0') +
    '.' + cfg.GetValue('ProjectOptions/VersionInfo/MinorVersionNr/Value', '0') +
    '.' + cfg.GetValue('ProjectOptions/VersionInfo/RevisionNr/Value', '0') +
    '.' + cfg.GetValue('ProjectOptions/VersionInfo/BuildNr/Value', '0'));
  cfg.Free;

  // Versionsdatei schließen
  Close(verFile);

  // Programm beenden
  Terminate;
end;

constructor TLazarusBuildExtractor.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException := True;
end;

destructor TLazarusBuildExtractor.Destroy;
begin
  inherited Destroy;
end;

var
  Application: TLazarusBuildExtractor;

{$R *.res}


begin
  Application := TLazarusBuildExtractor.Create(nil);
  Application.Title := 'Lazarus Build Extractor';
  Application.Run;
  Application.Free;
end.

