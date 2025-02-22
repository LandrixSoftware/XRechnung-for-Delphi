{
License XRechnung-for-Delphi

Copyright (C) 2025 Landrix Software GmbH & Co. KG
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

unit intf.XRechnungHelper;

interface

uses
  System.SysUtils, System.Classes,System.StrUtils
  ,Xml.xmldom,Xml.XMLDoc,Xml.XMLIntf,Xml.XMLSchema
  ,Xml.Win.msxmldom, Winapi.MSXMLIntf, Winapi.msxml
  ;

type
  TXRechnungXMLHelper = class(TObject)
  public
    class function FindChild(_Node : IXMLNode; const _NodeName : String; out _Result : IXMLNode) : Boolean;
    class function SelectNode(_XnRoot: IXMLDOMNode; const _NodePath: String; out _Result : IXMLDOMNode): Boolean;
    class function SelectNodes(_XnRoot: IXMLDOMNode; const _NodePath: String; out _Result : IXMLDOMNodeList): Boolean;
    class function SelectNodeText(_XnRoot: IXMLDOMNode; const _NodePath: String): String;
    class function SelectAttributeText(_XnRoot: IXMLDOMNode; const _Attribute: String): String;
    class function FindNode(_XnRoot: IXMLDOMNode; const _NodePath: String): Boolean;
    class function PrepareDocumentForXPathQuerys(_Xml : IXMLDocument) : IXMLDOMDocument2;
  end;

implementation

{ TXRechnungXMLHelper }

class function TXRechnungXMLHelper.SelectNodes(_XnRoot: IXMLDOMNode;
  const _NodePath: String; out _Result : IXMLDOMNodeList): Boolean;
begin
  Result := false;
  _Result := nil;
  if not Assigned(_XnRoot) then
    exit;
  _Result := _XnRoot.selectNodes(_NodePath);
  Result := _Result <> nil;
  if Result then
    Result := _Result.length > 0;
end;

class function TXRechnungXMLHelper.SelectNodeText(_XnRoot: IXMLDOMNode; const _NodePath: String): String;
var
  node : IXMLDOMNode;
begin
  Result := '';
  if TXRechnungXMLHelper.SelectNode(_XnRoot,_NodePath,node) then
    Result := node.Text;
end;

class function TXRechnungXMLHelper.SelectAttributeText(_XnRoot: IXMLDOMNode;
  const _Attribute: String): String;
begin
  Result := '';
  if _XnRoot = nil then
    exit;
  if _Attribute = '' then
    exit;
  if _XnRoot.attributes.getNamedItem(_Attribute) <> nil then
    Result := _XnRoot.attributes.getNamedItem(_Attribute).Text;
end;

class function TXRechnungXMLHelper.SelectNode(_XnRoot: IXMLDOMNode;
  const _NodePath: String; out _Result : IXMLDOMNode): Boolean;
begin
  Result := false;
  _Result := nil;
  if not Assigned(_XnRoot) then
    exit;
  _Result := _XnRoot.selectSingleNode(_NodePath);
  Result := _Result <> nil;
end;

class function TXRechnungXMLHelper.FindChild(_Node: IXMLNode; const _NodeName: String;
  out _Result: IXMLNode): Boolean;
begin
  Result := false;
  if _Node = nil then
    exit;
  _Result := _Node.ChildNodes.FindNode(_NodeName,'');
  Result := _Result <> nil;
end;

class function TXRechnungXMLHelper.PrepareDocumentForXPathQuerys(_Xml: IXMLDocument): IXMLDOMDocument2;
var
//  hList: IDOMNodeList;
//  i: Integer;
//  s, sNSN, sNSUri: string;
  sNsLine: string;
begin
  Result := nil;
  if not _Xml.Active then
    exit;

  sNsLine := 'xmlns:qdt="urn:un:unece:uncefact:data:standard:QualifiedDataType:100" ' +
             'xmlns:ram="urn:un:unece:uncefact:data:standard:ReusableAggregateBusinessInformationEntity:100" ' +
             'xmlns:rsm="urn:un:unece:uncefact:data:standard:CrossIndustryInvoice:100" ' +
             'xmlns:udt="urn:un:unece:uncefact:data:standard:UnqualifiedDataType:100" ' +
             'xmlns:cac="urn:oasis:names:specification:ubl:schema:xsd:CommonAggregateComponents-2" ' +
             'xmlns:cbc="urn:oasis:names:specification:ubl:schema:xsd:CommonBasicComponents-2"';

//  if SameText(_XML.DocumentElement.FindNamespaceURI('udt'), '') then
//    _XML.DocumentElement.DeclareNamespace('udt', 'urn:un:unece:uncefact:data:standard:UnqualifiedDataType:100');
//  if SameText(_XML.DocumentElement.FindNamespaceURI('qdt'), '') then
//    _XML.documentElement.DeclareNamespace('qdt', 'urn:un:unece:uncefact:data:standard:QualifiedDataType:100');
//
//  for i := 0 to _XML.DocumentElement.AttributeNodes.Count - 1 do
//  begin
//    sNSN := StringReplace(_XML.DocumentElement.AttributeNodes[I].NodeName, 'xmlns:', '', []);
//    if sNSN = 'xml' then
//    begin  // wenn es als xmlns:xml hinzugefuegt wird bekommt man die meldung das der Namespacename xml nicht verwendet werden darf...
//      sNSN := 'xmlns:MyXml';
//      sNSUri := _XML.DocumentElement.AttributeNodes[I].nodeValue;
//    end
//    else
//    if sNSN = 'xmlns' then
//    begin  // den Default Namespace mit einem Namen versehen, damit XPath drauf zugreifen kann.
//      sNSN := 'xmlns:dn';
//      sNSUri := _XML.DocumentElement.AttributeNodes[I].nodeValue;
//    end
//    else
//    begin  // alle anderen Namespace auch fuer XPath bekannt machen
//      sNSN := _XML.DocumentElement.AttributeNodes[I].nodeName;
//      sNSUri := _XML.DocumentElement.AttributeNodes[I].nodeValue;
//    end;
//    s := sNSN + '="'+sNSUri+'"';
//    if Pos(AnsiUpperCase(sNsLine), AnsiUpperCase(s)) > 0 then
//      continue;
//    if Pos(AnsiUpperCase(sNsLine), AnsiUpperCase(sNSN+'="')) > 0 then
//      continue;
//    if SameText(sNSN,'xsi:schemaLocation') then
//     continue;
//    sNsLine := ' '+s + sNsLine;
//  end;
//  sNsLine := trim(sNsLine);

  Result := CoDOMDocument60.Create;
  Result.loadXML(_Xml.XML.Text);
  Result.setProperty('SelectionLanguage', 'XPath');  // ab 4.0 ist SelectionLanguage eh immer XPath
  Result.setProperty('SelectionNamespaces', sNsLine) ;
end;

class function TXRechnungXMLHelper.FindNode(_XnRoot: IXMLDOMNode; const _NodePath: String): Boolean;
var
  Node : IXMLDOMNode;
begin
  Result := TXRechnungXMLHelper.SelectNode(_XnRoot,_NodePath,Node);
end;

end.
