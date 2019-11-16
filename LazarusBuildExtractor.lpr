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
  Classes, SysUtils, FileUtil, xmlconf;

type
  EParameterException = class(Exception);

var
  FileList: TStringList;
  lpiFileName: String;
  cfg: TXMLConfig;
  verFile: TextFile;

{$R *.res}

begin
  try
    // falsche Anzahl von Parameter
    if ParamCount <> 1 then
    begin
      raise EParameterException.Create('Invalid parameter Count');
    end;
    // ungültiger Pfad
    if not DirectoryExists(ParamStr(1)) then
    begin
      raise EParameterException.Create('Invalid parameter Directory');
    end;

    // Verzeichnis nach .lpi-Datei scannen
    try
      FileList := FindAllFiles(ParamStr(1), '*.lpi', False);
      // keine .lpi-Datei gefunden
      if FileList.Count = 0 then
      begin
        raise EParameterException.Create('Directory does not contain lpi');
      end
      // mehrere .lpi-Dateien gefunden
      else if FileList.Count > 1 then
      begin
        raise EParameterException.Create('Directory contains multiple lpi');
      end
      // Dateiname übernehmen
      else
      begin
        lpiFileName := FileList[0];
      end;
    finally
      FileList.Free;
    end;
  except
    on E: EParameterException do
    begin
      WriteLn('Usage: ', ParamStr(0), ' target');
      WriteLn('target: directory containing the .lpr file');
      Exit;
    end;
  end;

  // Versionsdatei öffnen
  AssignFile(verFile, ExtractFileDir(lpiFileName) + PathDelim + 'version.txt');
    try
    Rewrite(verFile);

    // Konfiguration einlesen und in Datei speichern
    cfg := TXMLConfig.Create(nil);
    cfg.Filename := lpiFileName;
    WriteLn(verFile,
      cfg.GetValue('ProjectOptions/VersionInfo/MajorVersionNr/Value', '0'),
      '.', cfg.GetValue('ProjectOptions/VersionInfo/MinorVersionNr/Value', '0'),
      '.' + cfg.GetValue('ProjectOptions/VersionInfo/RevisionNr/Value', '0'),
      '.', cfg.GetValue('ProjectOptions/VersionInfo/BuildNr/Value', '0'));
  finally
    cfg.Free;

    // Versionsdatei schließen
    Close(verFile);
  end;
end.
