{
License XRechnung-for-Delphi

Copyright (C) 2026 Landrix Software GmbH & Co. KG
Sven Harazim, info@landrix.de
Version 3.0.2

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program. If not, see <https://www.gnu.org/licenses/>.
}

{ FreePascal-Schreib-Shim fuer den XRechnung-Schreibpfad.

  Bildet exakt die im Schreibcode (intf.XRechnung.pas / intf.XRechnung_3_0.pas)
  genutzte Teilmenge der Delphi-Schnittstellen Xml.XMLIntf/Xml.XMLDoc auf
  fcl-xml (DOM/xmlwrite) ab:
    IXMLNode:  AddChild, Text (get/set), Attributes[] (set/get), DeclareNamespace
    IXMLDocument: Options/Version/Encoding/StandAlone/Active, AddChild,
                  DocumentElement, SaveToXML, SaveToStream, SaveToFile
    NewXMLDocument

  Diese Unit wird ausschliesslich unter IFDEF FPC eingebunden; unter Delphi
  kommen die Original-VCL-Units zum Einsatz. }

unit intf.XRechnungXmlShim;

{$IFDEF FPC}
  {$MODE DELPHIUNICODE}
  {$H+}
{$ENDIF}

interface

uses
  SysUtils, Classes,
  DOM, XMLWrite;

type
  // Aus Xml.XMLIntf uebernommene Optionsmenge - im Shim ohne Wirkung
  // (fcl-xml legt Knoten stets explizit an), aber fuer die Zuweisung
  //   _Xml.Options := [doNodeAutoCreate, doNodeAutoIndent, doAttrNull]
  // muessen die Bezeichner existieren.
  TXMLDocOption = (doNodeAutoCreate, doNodeAutoIndent, doAttrNull,
                   doAutoPrefix, doNamespaceDecl, doAutoSave);
  TXMLDocOptions = set of TXMLDocOption;

  IXMLNode = interface
    ['{4B2E0A10-7C2D-4E55-9C2C-2B0D8A1F1A01}']
    function AddChild(const AName: string): IXMLNode;
    function GetText: string;
    procedure SetText(const AValue: string);
    function GetAttribute(const AName: string): string;
    procedure SetAttribute(const AName, AValue: string);
    procedure DeclareNamespace(const APrefix, AURI: string);
    property Text: string read GetText write SetText;
    property Attributes[const AName: string]: string read GetAttribute write SetAttribute;
  end;

  IXMLDocument = interface
    ['{4B2E0A10-7C2D-4E55-9C2C-2B0D8A1F1A02}']
    function AddChild(const AName: string): IXMLNode;
    function GetDocumentElement: IXMLNode;
    function GetOptions: TXMLDocOptions;
    procedure SetOptions(const AValue: TXMLDocOptions);
    function GetVersion: string;
    procedure SetVersion(const AValue: string);
    function GetEncoding: string;
    procedure SetEncoding(const AValue: string);
    function GetStandAlone: string;
    procedure SetStandAlone(const AValue: string);
    function GetActive: Boolean;
    procedure SetActive(const AValue: Boolean);
    procedure SaveToXML(out AXML: string);
    procedure SaveToStream(AStream: TStream);
    procedure SaveToFile(const AFileName: string);
    property DocumentElement: IXMLNode read GetDocumentElement;
    property Options: TXMLDocOptions read GetOptions write SetOptions;
    property Version: string read GetVersion write SetVersion;
    property Encoding: string read GetEncoding write SetEncoding;
    property StandAlone: string read GetStandAlone write SetStandAlone;
    property Active: Boolean read GetActive write SetActive;
  end;

function NewXMLDocument: IXMLDocument;

implementation

type
  // Leichter, NICHT besitzender Wrapper um einen fcl-xml-Elementknoten.
  // Der Knoten gehoert dem TXMLDocument; der Wrapper wird per Interface-
  // Refcounting verwaltet und gibt den DOM-Knoten NICHT frei.
  TXMLNodeShim = class(TInterfacedObject, IXMLNode)
  private
    FDoc: TXMLDocument;
    FElem: TDOMElement;
  public
    constructor Create(ADoc: TXMLDocument; AElem: TDOMElement);
    function AddChild(const AName: string): IXMLNode;
    function GetText: string;
    procedure SetText(const AValue: string);
    function GetAttribute(const AName: string): string;
    procedure SetAttribute(const AName, AValue: string);
    procedure DeclareNamespace(const APrefix, AURI: string);
  end;

  TXMLDocumentShim = class(TInterfacedObject, IXMLDocument)
  private
    FDoc: TXMLDocument;
    FOptions: TXMLDocOptions;
    FVersion: string;
    FEncoding: string;
    FStandAlone: string;
    FActive: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function AddChild(const AName: string): IXMLNode;
    function GetDocumentElement: IXMLNode;
    function GetOptions: TXMLDocOptions;
    procedure SetOptions(const AValue: TXMLDocOptions);
    function GetVersion: string;
    procedure SetVersion(const AValue: string);
    function GetEncoding: string;
    procedure SetEncoding(const AValue: string);
    function GetStandAlone: string;
    procedure SetStandAlone(const AValue: string);
    function GetActive: Boolean;
    procedure SetActive(const AValue: Boolean);
    procedure SaveToXML(out AXML: string);
    procedure SaveToStream(AStream: TStream);
    procedure SaveToFile(const AFileName: string);
  end;

{ TXMLNodeShim }

constructor TXMLNodeShim.Create(ADoc: TXMLDocument; AElem: TDOMElement);
begin
  inherited Create;
  FDoc := ADoc;
  FElem := AElem;
end;

function TXMLNodeShim.AddChild(const AName: string): IXMLNode;
var
  child: TDOMElement;
begin
  child := FDoc.CreateElement(DOMString(AName));
  FElem.AppendChild(child);
  Result := TXMLNodeShim.Create(FDoc, child);
end;

function TXMLNodeShim.GetText: string;
begin
  Result := string(FElem.TextContent);
end;

procedure TXMLNodeShim.SetText(const AValue: string);
begin
  // Entspricht IXMLNode.Text := ... : ersetzt den Knoteninhalt durch genau
  // einen Textknoten (im Schreibcode wird Text stets genau einmal gesetzt).
  FElem.TextContent := DOMString(AValue);
end;

function TXMLNodeShim.GetAttribute(const AName: string): string;
begin
  Result := string(FElem.GetAttribute(DOMString(AName)));
end;

procedure TXMLNodeShim.SetAttribute(const AName, AValue: string);
begin
  FElem.SetAttribute(DOMString(AName), DOMString(AValue));
end;

procedure TXMLNodeShim.DeclareNamespace(const APrefix, AURI: string);
begin
  if APrefix = '' then
    FElem.SetAttribute('xmlns', DOMString(AURI))
  else
    FElem.SetAttribute(DOMString('xmlns:' + APrefix), DOMString(AURI));
end;

{ TXMLDocumentShim }

constructor TXMLDocumentShim.Create;
begin
  inherited Create;
  FDoc := TXMLDocument.Create;
  FVersion := '1.0';
  FEncoding := 'UTF-8';
  FStandAlone := '';
  FActive := False;
end;

destructor TXMLDocumentShim.Destroy;
begin
  FDoc.Free;
  inherited Destroy;
end;

function TXMLDocumentShim.AddChild(const AName: string): IXMLNode;
var
  root: TDOMElement;
begin
  root := FDoc.CreateElement(DOMString(AName));
  FDoc.AppendChild(root);
  Result := TXMLNodeShim.Create(FDoc, root);
end;

function TXMLDocumentShim.GetDocumentElement: IXMLNode;
begin
  if FDoc.DocumentElement <> nil then
    Result := TXMLNodeShim.Create(FDoc, FDoc.DocumentElement)
  else
    Result := nil;
end;

function TXMLDocumentShim.GetOptions: TXMLDocOptions;
begin
  Result := FOptions;
end;

procedure TXMLDocumentShim.SetOptions(const AValue: TXMLDocOptions);
begin
  FOptions := AValue;
end;

function TXMLDocumentShim.GetVersion: string;
begin
  Result := FVersion;
end;

procedure TXMLDocumentShim.SetVersion(const AValue: string);
begin
  FVersion := AValue;
  FDoc.XMLVersion := DOMString(AValue);
end;

function TXMLDocumentShim.GetEncoding: string;
begin
  Result := FEncoding;
end;

procedure TXMLDocumentShim.SetEncoding(const AValue: string);
begin
  FEncoding := AValue;
end;

function TXMLDocumentShim.GetStandAlone: string;
begin
  Result := FStandAlone;
end;

procedure TXMLDocumentShim.SetStandAlone(const AValue: string);
begin
  FStandAlone := AValue;
end;

function TXMLDocumentShim.GetActive: Boolean;
begin
  Result := FActive;
end;

procedure TXMLDocumentShim.SetActive(const AValue: Boolean);
begin
  FActive := AValue;
end;

procedure TXMLDocumentShim.SaveToXML(out AXML: string);
var
  ms: TMemoryStream;
  raw: RawByteString;
begin
  // WriteXML schreibt UTF-8-Bytes; diese explizit (ohne Codepage-Umweg ueber
  // TStringStream.DataString) nach UnicodeString dekodieren.
  ms := TMemoryStream.Create;
  try
    WriteXML(FDoc, ms);
    SetLength(raw, ms.Size);
    if ms.Size > 0 then
      Move(ms.Memory^, raw[1], ms.Size);
    AXML := UTF8ToString(raw);
  finally
    ms.Free;
  end;
end;

procedure TXMLDocumentShim.SaveToStream(AStream: TStream);
begin
  WriteXML(FDoc, AStream);
end;

procedure TXMLDocumentShim.SaveToFile(const AFileName: string);
begin
  WriteXMLFile(FDoc, AFileName);
end;

function NewXMLDocument: IXMLDocument;
begin
  Result := TXMLDocumentShim.Create;
end;

end.
