{
License XRechnung-for-Delphi

Copyright (C) 2024 Landrix Software GmbH & Co. KG
Sven Harazim, info@landrix.de
Version 3.0.1

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
}

unit intf.XRechnungHelper;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes
  ,System.IOUtils,System.Win.COMObj,System.UITypes,System.StrUtils
  ,Xml.xmldom,Xml.XMLDoc,Xml.XMLIntf,Xml.XMLSchema,intf.XRechnungMSXML2_TLB
  ;

type
  TXRechnungXMLHelper = class(TObject)
  public type
    TXMLLoadCallback = procedure(Node : IXMLNode) of object;
  public
    class procedure LoadFromChilds(const _NodeName : String; _Node : IXMLNode; _Callback : TXMLLoadCallback);
    class function FindChild(_Node : IXMLNode; const _NodeName : String; out _Result : IXMLNode) : Boolean;
    class function SelectNode(_XnRoot: IXMLDOMNode; const _NodePath: String; out _Result : IXMLDOMNode): Boolean;
    class function SelectNodes(_XnRoot: IXMLDOMNode; const _NodePath: String; out _Result : IXMLDOMNodeList): Boolean;
    class function SelectNodeText(_XnRoot: IXMLDOMNode; const _NodePath: String): String;
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

class procedure TXRechnungXMLHelper.LoadFromChilds(const _NodeName: String; _Node: IXMLNode;
  _Callback: TXMLLoadCallback);
var
  Node : IXMLNode;
begin
  Node := _Node.ChildNodes.FindNode(_NodeName,'');
  if Node = nil then
    exit;
  _Callback(Node);
end;

class function TXRechnungXMLHelper.PrepareDocumentForXPathQuerys(_Xml: IXMLDocument): IXMLDOMDocument2;
var
  hList: IDOMNodeList;
  i: Integer;
  s, sNSN, sNSUri: string;
  sNsLine: string;
begin
  Result := nil;
  if not _Xml.Active then
    exit;

  hList := (_Xml.DOMDocument as IDOMNodeSelect).selectNodes('//namespace::*');
  try
    for i := 0 to hList.length - 1 do
    begin
      sNSN := StringReplace(hList.item[i].nodeName, 'xmlns:', '', []);
      if sNSN = 'xml' then
      begin  // wenn es als xmlns:xml hinzugefuegt wird bekommt man die meldung das der Namespacename xml nicht verwendet werden darf...
        sNSN := 'xmlns:MyXml';
        sNSUri := hList.item[i].nodeValue;
      end
      else
      if sNSN = 'xmlns' then
      begin  // den Default Namespace mit einem Namen versehen, damit XPath drauf zugreifen kann.
        sNSN := 'xmlns:dn';
        sNSUri := hList.item[i].nodeValue;
      end
      else
      begin  // alle anderen Namespace auch fuer XPath bekannt machen
        sNSN := hList.item[i].nodeName;
        sNSUri := hList.item[i].nodeValue;
      end;
      s := sNSN + '="'+sNSUri+'"';
      if ContainsText(sNsLine, s) then
        continue;
      if ContainsText(sNsLine,sNSN+'="') then
        continue;
      sNsLine := ' '+s + sNsLine;
    end;
    sNsLine := trim(sNsLine);
  finally
    hList := nil;
  end;

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
