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

{ FreePascal-XML-Shim fuer XRechnung.

  Bildet die im Schreib- UND Lesecode (intf.XRechnung.pas /
  intf.XRechnung_3_0.pas / intf.XRechnungHelper.pas) genutzte Teilmenge der
  Delphi-Schnittstellen Xml.XMLIntf/Xml.XMLDoc (VCL-DOM) und Winapi.MSXMLIntf
  (MSXML) auf fcl-xml (DOM/XMLRead/XMLWrite/XPath) ab.

  Schreibpfad (Xml.XMLIntf-Nachbau):
    IXMLNode:     AddChild, Text (get/set), Attributes[] (set/get),
                  DeclareNamespace, NodeName, ChildNodes
    IXMLNodeList: FindNode
    IXMLDocument: Options/Version/Encoding/StandAlone/Active, AddChild,
                  DocumentElement, SaveToXML/SaveToStream/SaveToFile
    NewXMLDocument, TXMLDocument.Create(nil)

  Lesepfad (MSXML-Nachbau):
    IXMLDocument:       LoadFromFile/LoadFromStream/LoadFromXML
    IXMLDOMNode:        text, nodeName, attributes,
                        selectSingleNode, selectNodes
    IXMLDOMNodeList:    length, item[] (Default-Property)
    IXMLDOMNamedNodeMap:getNamedItem
    IXMLDOMDocument2:   Dokumentsicht auf dieselben Knoten

  Die XPath-Praefixe cbc/cac/ram/rsm/udt/qdt sind - wie unter MSXML ueber
  SelectionNamespaces - fest an ihre Namespace-URIs gebunden. Gematcht wird
  gegen die URI, nicht gegen das Praefix im Dokument; eine Rechnung darf ihre
  Namespaces also beliebig benennen.

  Diese Unit wird ausschliesslich unter IFDEF FPC eingebunden; unter Delphi
  kommen die Original-VCL-/MSXML-Units zum Einsatz. }

unit intf.XRechnungXmlShim;

{$IFDEF FPC}
  {$MODE DELPHIUNICODE}
  {$H+}
{$ENDIF}

interface

uses
  SysUtils, Classes,
  DOM, XMLRead, XMLWrite, XPath;

type
  // Aus Xml.XMLIntf uebernommene Optionsmenge - im Shim ohne Wirkung
  // (fcl-xml legt Knoten stets explizit an), aber fuer die Zuweisung
  //   _Xml.Options := [doNodeAutoCreate, doNodeAutoIndent, doAttrNull]
  // muessen die Bezeichner existieren.
  TXMLDocOption = (doNodeAutoCreate, doNodeAutoIndent, doAttrNull,
                   doAutoPrefix, doNamespaceDecl, doAutoSave);
  TXMLDocOptions = set of TXMLDocOption;

  IXMLNode = interface;
  IXMLNodeList = interface;
  IXMLDOMNode = interface;
  IXMLDOMNodeList = interface;
  IXMLDOMNamedNodeMap = interface;

  IXMLNodeList = interface
    ['{4B2E0A10-7C2D-4E55-9C2C-2B0D8A1F1A03}']
    // Entspricht Xml.XMLIntf: sucht per NodeName, wenn ANamespaceURI leer ist.
    function FindNode(const AName: string; const ANamespaceURI: string = ''): IXMLNode;
    function GetCount: Integer;
    function GetNode(AIndex: Integer): IXMLNode;
    property Count: Integer read GetCount;
    property Nodes[AIndex: Integer]: IXMLNode read GetNode; default;
  end;

  IXMLNode = interface
    ['{4B2E0A10-7C2D-4E55-9C2C-2B0D8A1F1A01}']
    function AddChild(const AName: string): IXMLNode;
    function GetText: string;
    procedure SetText(const AValue: string);
    function GetAttribute(const AName: string): string;
    procedure SetAttribute(const AName, AValue: string);
    procedure DeclareNamespace(const APrefix, AURI: string);
    function GetNodeName: string;
    function GetChildNodes: IXMLNodeList;
    property Text: string read GetText write SetText;
    property Attributes[const AName: string]: string read GetAttribute write SetAttribute;
    property NodeName: string read GetNodeName;
    property ChildNodes: IXMLNodeList read GetChildNodes;
  end;

  // --- MSXML-Nachbau fuer den Lesepfad -------------------------------------

  IXMLDOMNode = interface
    ['{4B2E0A10-7C2D-4E55-9C2C-2B0D8A1F1A10}']
    function Get_text: string;
    function Get_nodeName: string;
    function Get_attributes: IXMLDOMNamedNodeMap;
    function selectSingleNode(const AQuery: string): IXMLDOMNode;
    function selectNodes(const AQuery: string): IXMLDOMNodeList;
    property text: string read Get_text;
    property nodeName: string read Get_nodeName;
    property attributes: IXMLDOMNamedNodeMap read Get_attributes;
  end;

  IXMLDOMNodeList = interface
    ['{4B2E0A10-7C2D-4E55-9C2C-2B0D8A1F1A11}']
    function Get_item(AIndex: Integer): IXMLDOMNode;
    function Get_length: Integer;
    property item[AIndex: Integer]: IXMLDOMNode read Get_item; default;
    property length: Integer read Get_length;
  end;

  IXMLDOMNamedNodeMap = interface
    ['{4B2E0A10-7C2D-4E55-9C2C-2B0D8A1F1A12}']
    function getNamedItem(const AName: string): IXMLDOMNode;
  end;

  // Unter MSXML eine eigene Dokumentklasse; hier lediglich eine Sicht auf
  // dieselben Knoten, damit die Signaturen des Lesecodes unveraendert bleiben.
  IXMLDOMDocument2 = interface(IXMLDOMNode)
    ['{4B2E0A10-7C2D-4E55-9C2C-2B0D8A1F1A13}']
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
    procedure LoadFromFile(const AFileName: string);
    procedure LoadFromStream(AStream: TStream);
    procedure LoadFromXML(const AXML: string);
    // Sicht auf dasselbe Dokument fuer XPath-Abfragen (siehe
    // TXRechnungXMLHelper.PrepareDocumentForXPathQuerys).
    function AsDOMDocument: IXMLDOMDocument2;
    property DocumentElement: IXMLNode read GetDocumentElement;
    property Options: TXMLDocOptions read GetOptions write SetOptions;
    property Version: string read GetVersion write SetVersion;
    property Encoding: string read GetEncoding write SetEncoding;
    property StandAlone: string read GetStandAlone write SetStandAlone;
    property Active: Boolean read GetActive write SetActive;
  end;

  // Namensgleicher Ersatz fuer Xml.XMLDoc.TXMLDocument, damit der gemeinsame
  // Quelltext weiterhin  xml := TXMLDocument.Create(nil)  schreiben kann.
  // Das fcl-xml-Dokument heisst ebenfalls TXMLDocument und wird in dieser
  // Unit daher durchgaengig als DOM.TXMLDocument qualifiziert.
  TXMLDocument = class(TInterfacedObject, IXMLDocument)
  private
    FOwner: IInterface;      // haelt das fcl-xml-Dokument am Leben
    FOptions: TXMLDocOptions;
    FVersion: string;
    FEncoding: string;
    FStandAlone: string;
    FActive: Boolean;
    function Doc: DOM.TXMLDocument;
  public
    constructor Create(AOwner: TObject = nil);
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
    procedure LoadFromFile(const AFileName: string);
    procedure LoadFromStream(AStream: TStream);
    procedure LoadFromXML(const AXML: string);
    function AsDOMDocument: IXMLDOMDocument2;
  end;

function NewXMLDocument: IXMLDocument;

// Namespace-URIs der XRechnung-relevanten XPath-Praefixe. Oeffentlich, damit
// Tests und Fremdcode dieselbe Bindung verwenden koennen.
function XRechnungNamespaceURI(const APrefix: string): string;

implementation

const
  NS_CBC = 'urn:oasis:names:specification:ubl:schema:xsd:CommonBasicComponents-2';
  NS_CAC = 'urn:oasis:names:specification:ubl:schema:xsd:CommonAggregateComponents-2';
  NS_RAM = 'urn:un:unece:uncefact:data:standard:ReusableAggregateBusinessInformationEntity:100';
  NS_RSM = 'urn:un:unece:uncefact:data:standard:CrossIndustryInvoice:100';
  NS_UDT = 'urn:un:unece:uncefact:data:standard:UnqualifiedDataType:100';
  NS_QDT = 'urn:un:unece:uncefact:data:standard:QualifiedDataType:100';

function XRechnungNamespaceURI(const APrefix: string): string;
begin
  if APrefix = 'cbc' then Result := NS_CBC else
  if APrefix = 'cac' then Result := NS_CAC else
  if APrefix = 'ram' then Result := NS_RAM else
  if APrefix = 'rsm' then Result := NS_RSM else
  if APrefix = 'udt' then Result := NS_UDT else
  if APrefix = 'qdt' then Result := NS_QDT else
  Result := '';
end;

type
  TDomOwner = class;

  // Resolver: bildet die XRechnung-Praefixe auf ihre URIs ab und faellt sonst
  // auf die im Dokument deklarierten Namespaces zurueck. Entspricht MSXML
  // setProperty('SelectionNamespaces', ...). Sieht die vom Owner vergebenen
  // Kurz-Aliase (siehe RewriteQuery) und loest sie auf das Originalpraefix auf.
  TXRechnungNSResolver = class(TXPathNSResolver)
  private
    FOwnerObj: TDomOwner;   // rohe Referenz - der Owner besitzt den Resolver
  public
    constructor Create(aNode: TDOMNode; AOwnerObj: TDomOwner);
    function LookupNamespaceURI(const aPrefix: DOMString): DOMString; override;
  end;

  // Besitzt das fcl-xml-Dokument. Dokument- und Knoten-Wrapper halten eine
  // Interface-Referenz darauf, damit kein Wrapper ein bereits freigegebenes
  // DOM sieht - die Lesefunktionen geben Knoten nach aussen, die den
  // IXMLDocument-Wrapper ueberleben koennen.
  IDomOwner = interface
    ['{4B2E0A10-7C2D-4E55-9C2C-2B0D8A1F1A20}']
    function GetDoc: DOM.TXMLDocument;
    function GetResolver: TXPathNSResolver;
    procedure ReplaceDoc(ADoc: DOM.TXMLDocument);
    function RewriteQuery(const AQuery: string): string;
  end;

  TDomOwner = class(TInterfacedObject, IDomOwner)
  private
    FDoc: DOM.TXMLDocument;
    FResolver: TXRechnungNSResolver;
    FAliasOf: TStringList;   // 'ram=ra' - Originalpraefix -> Alias
    FPrefixOf: TStringList;  // 'ra=ram' - Alias -> Originalpraefix
    function AliasFor(const APrefix: string): string;
  public
    constructor Create;
    destructor Destroy; override;
    function GetDoc: DOM.TXMLDocument;
    function GetResolver: TXPathNSResolver;
    procedure ReplaceDoc(ADoc: DOM.TXMLDocument);
    function RewriteQuery(const AQuery: string): string;
    function PrefixOfAlias(const AAlias: string): string;
  end;

  TXMLNodeShim = class(TInterfacedObject, IXMLNode)
  private
    FOwner: IDomOwner;
    FElem: TDOMElement;
  public
    constructor Create(AOwner: IDomOwner; AElem: TDOMElement);
    function AddChild(const AName: string): IXMLNode;
    function GetText: string;
    procedure SetText(const AValue: string);
    function GetAttribute(const AName: string): string;
    procedure SetAttribute(const AName, AValue: string);
    procedure DeclareNamespace(const APrefix, AURI: string);
    function GetNodeName: string;
    function GetChildNodes: IXMLNodeList;
  end;

  TXMLNodeListShim = class(TInterfacedObject, IXMLNodeList)
  private
    FOwner: IDomOwner;
    FParent: TDOMNode;
  public
    constructor Create(AOwner: IDomOwner; AParent: TDOMNode);
    function FindNode(const AName: string; const ANamespaceURI: string = ''): IXMLNode;
    function GetCount: Integer;
    function GetNode(AIndex: Integer): IXMLNode;
  end;

  TDOMNodeShim = class(TInterfacedObject, IXMLDOMNode, IXMLDOMDocument2)
  private
    FOwner: IDomOwner;
    FNode: TDOMNode;
  public
    constructor Create(AOwner: IDomOwner; ANode: TDOMNode);
    function Get_text: string;
    function Get_nodeName: string;
    function Get_attributes: IXMLDOMNamedNodeMap;
    function selectSingleNode(const AQuery: string): IXMLDOMNode;
    function selectNodes(const AQuery: string): IXMLDOMNodeList;
  end;

  TDOMNodeListShim = class(TInterfacedObject, IXMLDOMNodeList)
  private
    FOwner: IDomOwner;
    FItems: TFPList;         // TDOMNode-Zeiger, Kopie des XPath-Node-Sets
  public
    constructor Create(AOwner: IDomOwner; ANodes: TNodeSet);
    destructor Destroy; override;
    function Get_item(AIndex: Integer): IXMLDOMNode;
    function Get_length: Integer;
  end;

  TDOMNamedNodeMapShim = class(TInterfacedObject, IXMLDOMNamedNodeMap)
  private
    FOwner: IDomOwner;
    FMap: TDOMNamedNodeMap;
  public
    constructor Create(AOwner: IDomOwner; AMap: TDOMNamedNodeMap);
    function getNamedItem(const AName: string): IXMLDOMNode;
  end;

{ TXRechnungNSResolver }

constructor TXRechnungNSResolver.Create(aNode: TDOMNode; AOwnerObj: TDomOwner);
begin
  inherited Create(aNode);
  FOwnerObj := AOwnerObj;
end;

function TXRechnungNSResolver.LookupNamespaceURI(const aPrefix: DOMString): DOMString;
var
  orig: string;
begin
  // Die Abfragen laufen mit Kurz-Aliasen (RewriteQuery); hier zurueck auf das
  // Originalpraefix, damit Tabelle und Dokument-Fallback unveraendert greifen.
  orig := string(aPrefix);
  if FOwnerObj <> nil then
    orig := FOwnerObj.PrefixOfAlias(orig);
  Result := DOMString(XRechnungNamespaceURI(orig));
  if Result = '' then
    Result := inherited LookupNamespaceURI(DOMString(orig));
end;

{ TDomOwner }

constructor TDomOwner.Create;
begin
  inherited Create;
  FDoc := DOM.TXMLDocument.Create;
  FAliasOf := TStringList.Create;
  FPrefixOf := TStringList.Create;
  FResolver := TXRechnungNSResolver.Create(FDoc, Self);
end;

destructor TDomOwner.Destroy;
begin
  FResolver.Free;
  FPrefixOf.Free;
  FAliasOf.Free;
  FDoc.Free;
  inherited Destroy;
end;

function TDomOwner.GetDoc: DOM.TXMLDocument;
begin
  Result := FDoc;
end;

function TDomOwner.GetResolver: TXPathNSResolver;
begin
  Result := FResolver;
end;

procedure TDomOwner.ReplaceDoc(ADoc: DOM.TXMLDocument);
begin
  if ADoc = FDoc then
    exit;
  FResolver.Free;
  FDoc.Free;
  FDoc := ADoc;
  // Die Aliastabelle bleibt bestehen - sie haengt nur an den Praefixen.
  FResolver := TXRechnungNSResolver.Create(FDoc, Self);
end;

function TDomOwner.PrefixOfAlias(const AAlias: string): string;
begin
  Result := FPrefixOf.Values[AAlias];
  if Result = '' then
    Result := AAlias;
end;

// Vergibt zu einem Praefix einen stabilen, genau ZWEI Zeichen langen Alias.
function TDomOwner.AliasFor(const APrefix: string): string;
var
  cand: string;
  i: Integer;
begin
  Result := FAliasOf.Values[APrefix];
  if Result <> '' then
    exit;

  if Length(APrefix) = 2 then
    cand := APrefix                       // schon zweistellig, nichts zu tun
  else
  begin
    cand := Copy(APrefix,1,2);
    if Length(cand) < 2 then
      cand := cand + '0';
    i := 0;
    // Kollision mit einem anderen Praefix aufloesen: c0, c1, ... a0, a1, ...
    while (FPrefixOf.Values[cand] <> '') and (FPrefixOf.Values[cand] <> APrefix) do
    begin
      if i < 10 then
        cand := Copy(APrefix,1,1) + IntToStr(i)
      else
        cand := Chr(Ord('a') + ((i-10) div 10) mod 26) + IntToStr((i-10) mod 10);
      Inc(i);
    end;
  end;

  FAliasOf.Values[APrefix] := cand;
  FPrefixOf.Values[cand] := APrefix;
  Result := cand;
end;

function IsNCNameStart(_Ch: Char): Boolean;
begin
  Result := ((_Ch >= 'a') and (_Ch <= 'z')) or
            ((_Ch >= 'A') and (_Ch <= 'Z')) or (_Ch = '_');
end;

function IsNCNameChar(_Ch: Char): Boolean;
begin
  Result := IsNCNameStart(_Ch) or ((_Ch >= '0') and (_Ch <= '9')) or
            (_Ch = '.') or (_Ch = '-');
end;

// Ersetzt in einem XPath-Ausdruck jedes Namespace-Praefix durch seinen
// zweistelligen Alias: './/ram:LineID' -> './/ra:LineID'.
//
// Grund ist ein Fehler in FPCs XPath-Scanner (packages/fcl-xml/src/xpathkw.inc,
// noch in FPC 3.2.2 und im aktuellen main):
//
//   MaxHash = 55;
//   KeywordIndex: array[0..MaxHash-1] of TXPathKeyword;   // gueltig 0..54
//   if (hash >= 0) and (hash <= MaxHash) then             // laesst 55 zu
//     p1 := XPathKeywords[KeywordIndex[hash]];            // liest hinter dem Array
//
// LookupXPathKeyword bildet hash aus Tokenlaenge sowie erstem und DRITTEM
// Zeichen. Trifft ein Token die 55, wird hinter der Tabelle gelesen, das
// Ergebnis als Enum gedeutet und damit ein Zeigerarray indiziert - unter
// x86_64-Windows unauffaellig, unter aarch64-Linux eine Access Violation.
// Betroffen waren fuenf Elementnamen mit 'ram:'-Praefix, z.B.
// 'ram:AssociatedDocumentLineDocument' (34 + 17 + 4 = 55).
//
// Bei einem zweistelligen Praefix ist das dritte Zeichen stets ':', worauf die
// Hashfunktion sofort aussteigt - unabhaengig von der Elementnamenlaenge. Der
// Lesecode bleibt dadurch unveraendert und muss die Aliase nicht kennen.
function TDomOwner.RewriteQuery(const AQuery: string): string;
var
  i, n, start: Integer;
  name: string;
  quote: Char;
begin
  Result := '';
  i := 1;
  n := Length(AQuery);
  while i <= n do
  begin
    if (AQuery[i] = '''') or (AQuery[i] = '"') then
    begin
      // Stringliteral (z.B. in local-name()="Invoice") unveraendert uebernehmen
      quote := AQuery[i];
      start := i;
      Inc(i);
      while (i <= n) and (AQuery[i] <> quote) do
        Inc(i);
      if i <= n then
        Inc(i);
      Result := Result + Copy(AQuery, start, i-start);
    end
    else
    if IsNCNameStart(AQuery[i]) then
    begin
      start := i;
      while (i <= n) and IsNCNameChar(AQuery[i]) do
        Inc(i);
      name := Copy(AQuery, start, i-start);
      // Nur ein echtes Praefix ersetzen: genau EIN ':' dahinter. '::' ist eine
      // Achse (descendant::x), '(' ein Funktionsaufruf (local-name()).
      if (i <= n) and (AQuery[i] = ':') and
         not ((i < n) and (AQuery[i+1] = ':')) then
        Result := Result + AliasFor(name)
      else
        Result := Result + name;
    end
    else
    begin
      Result := Result + AQuery[i];
      Inc(i);
    end;
  end;
end;

{ TXMLNodeShim }

constructor TXMLNodeShim.Create(AOwner: IDomOwner; AElem: TDOMElement);
begin
  inherited Create;
  FOwner := AOwner;
  FElem := AElem;
end;

function TXMLNodeShim.AddChild(const AName: string): IXMLNode;
var
  child: TDOMElement;
begin
  child := FOwner.GetDoc.CreateElement(DOMString(AName));
  FElem.AppendChild(child);
  Result := TXMLNodeShim.Create(FOwner, child);
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

function TXMLNodeShim.GetNodeName: string;
begin
  Result := string(FElem.NodeName);
end;

function TXMLNodeShim.GetChildNodes: IXMLNodeList;
begin
  Result := TXMLNodeListShim.Create(FOwner, FElem);
end;

{ TXMLNodeListShim }

constructor TXMLNodeListShim.Create(AOwner: IDomOwner; AParent: TDOMNode);
begin
  inherited Create;
  FOwner := AOwner;
  FParent := AParent;
end;

function TXMLNodeListShim.FindNode(const AName: string;
  const ANamespaceURI: string): IXMLNode;
var
  n: TDOMNode;
begin
  Result := nil;
  if FParent = nil then
    exit;
  n := FParent.FirstChild;
  while n <> nil do
  begin
    if n.NodeType = ELEMENT_NODE then
    begin
      if ANamespaceURI = '' then
      begin
        // Xml.XMLIntf sucht ohne Namespace-Angabe ueber den vollen Knotennamen.
        if n.NodeName = DOMString(AName) then
        begin
          Result := TXMLNodeShim.Create(FOwner, TDOMElement(n));
          exit;
        end;
      end
      else
      if (n.NamespaceURI = DOMString(ANamespaceURI)) and
         (n.LocalName = DOMString(AName)) then
      begin
        Result := TXMLNodeShim.Create(FOwner, TDOMElement(n));
        exit;
      end;
    end;
    n := n.NextSibling;
  end;
end;

function TXMLNodeListShim.GetCount: Integer;
var
  n: TDOMNode;
begin
  Result := 0;
  if FParent = nil then
    exit;
  n := FParent.FirstChild;
  while n <> nil do
  begin
    if n.NodeType = ELEMENT_NODE then
      Inc(Result);
    n := n.NextSibling;
  end;
end;

function TXMLNodeListShim.GetNode(AIndex: Integer): IXMLNode;
var
  n: TDOMNode;
  i: Integer;
begin
  Result := nil;
  if FParent = nil then
    exit;
  i := 0;
  n := FParent.FirstChild;
  while n <> nil do
  begin
    if n.NodeType = ELEMENT_NODE then
    begin
      if i = AIndex then
      begin
        Result := TXMLNodeShim.Create(FOwner, TDOMElement(n));
        exit;
      end;
      Inc(i);
    end;
    n := n.NextSibling;
  end;
end;

{ TDOMNodeShim }

constructor TDOMNodeShim.Create(AOwner: IDomOwner; ANode: TDOMNode);
begin
  inherited Create;
  FOwner := AOwner;
  FNode := ANode;
end;

function TDOMNodeShim.Get_text: string;
begin
  // MSXML: IXMLDOMNode.text liefert den konkatenierten Text aller Nachfahren.
  Result := string(FNode.TextContent);
end;

function TDOMNodeShim.Get_nodeName: string;
begin
  Result := string(FNode.NodeName);
end;

function TDOMNodeShim.Get_attributes: IXMLDOMNamedNodeMap;
begin
  Result := TDOMNamedNodeMapShim.Create(FOwner, FNode.Attributes);
end;

function TDOMNodeShim.selectSingleNode(const AQuery: string): IXMLDOMNode;
var
  res: TXPathVariable;
  ns: TNodeSet;
begin
  Result := nil;
  if FNode = nil then
    exit;
  try
    res := EvaluateXPathExpression(DOMString(FOwner.RewriteQuery(AQuery)),
                                   FNode, FOwner.GetResolver);
  except
    // Unaufloesbares Praefix oder Syntaxfehler: MSXML liefert in diesem Fall
    // ebenfalls keinen Treffer statt den Lesevorgang abzubrechen.
    exit;
  end;
  try
    ns := res.AsNodeSet;
    if (ns <> nil) and (ns.Count > 0) then
      Result := TDOMNodeShim.Create(FOwner, TDOMNode(ns[0]));
  finally
    res.Release;
  end;
end;

function TDOMNodeShim.selectNodes(const AQuery: string): IXMLDOMNodeList;
var
  res: TXPathVariable;
begin
  Result := nil;
  if FNode = nil then
    exit;
  try
    res := EvaluateXPathExpression(DOMString(FOwner.RewriteQuery(AQuery)),
                                   FNode, FOwner.GetResolver);
  except
    exit;
  end;
  try
    Result := TDOMNodeListShim.Create(FOwner, res.AsNodeSet);
  finally
    res.Release;
  end;
end;

{ TDOMNodeListShim }

constructor TDOMNodeListShim.Create(AOwner: IDomOwner; ANodes: TNodeSet);
var
  i: Integer;
begin
  inherited Create;
  FOwner := AOwner;
  // Das Node-Set gehoert der TXPathVariable und wird mit ihr freigegeben -
  // daher die Zeigerliste kopieren. Die Knoten selbst gehoeren dem Dokument.
  FItems := TFPList.Create;
  if ANodes <> nil then
    for i := 0 to ANodes.Count - 1 do
      FItems.Add(ANodes[i]);
end;

destructor TDOMNodeListShim.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

function TDOMNodeListShim.Get_item(AIndex: Integer): IXMLDOMNode;
begin
  if (AIndex < 0) or (AIndex >= FItems.Count) then
    Result := nil
  else
    Result := TDOMNodeShim.Create(FOwner, TDOMNode(FItems[AIndex]));
end;

function TDOMNodeListShim.Get_length: Integer;
begin
  Result := FItems.Count;
end;

{ TDOMNamedNodeMapShim }

constructor TDOMNamedNodeMapShim.Create(AOwner: IDomOwner; AMap: TDOMNamedNodeMap);
begin
  inherited Create;
  FOwner := AOwner;
  FMap := AMap;
end;

function TDOMNamedNodeMapShim.getNamedItem(const AName: string): IXMLDOMNode;
var
  n: TDOMNode;
begin
  Result := nil;
  if FMap = nil then
    exit;
  n := FMap.GetNamedItem(DOMString(AName));
  if n <> nil then
    Result := TDOMNodeShim.Create(FOwner, n);
end;

{ TXMLDocument }

constructor TXMLDocument.Create(AOwner: TObject);
begin
  inherited Create;
  FOwner := TDomOwner.Create;
  FVersion := '1.0';
  FEncoding := 'UTF-8';
  FStandAlone := '';
  FActive := False;
end;

function TXMLDocument.Doc: DOM.TXMLDocument;
begin
  Result := (FOwner as IDomOwner).GetDoc;
end;

function TXMLDocument.AddChild(const AName: string): IXMLNode;
var
  root: TDOMElement;
begin
  root := Doc.CreateElement(DOMString(AName));
  Doc.AppendChild(root);
  Result := TXMLNodeShim.Create(FOwner as IDomOwner, root);
end;

function TXMLDocument.GetDocumentElement: IXMLNode;
begin
  if Doc.DocumentElement <> nil then
    Result := TXMLNodeShim.Create(FOwner as IDomOwner, Doc.DocumentElement)
  else
    Result := nil;
end;

function TXMLDocument.GetOptions: TXMLDocOptions;
begin
  Result := FOptions;
end;

procedure TXMLDocument.SetOptions(const AValue: TXMLDocOptions);
begin
  FOptions := AValue;
end;

function TXMLDocument.GetVersion: string;
begin
  Result := FVersion;
end;

procedure TXMLDocument.SetVersion(const AValue: string);
begin
  FVersion := AValue;
  Doc.XMLVersion := DOMString(AValue);
end;

function TXMLDocument.GetEncoding: string;
begin
  Result := FEncoding;
end;

procedure TXMLDocument.SetEncoding(const AValue: string);
begin
  FEncoding := AValue;
end;

function TXMLDocument.GetStandAlone: string;
begin
  Result := FStandAlone;
end;

procedure TXMLDocument.SetStandAlone(const AValue: string);
begin
  FStandAlone := AValue;
end;

function TXMLDocument.GetActive: Boolean;
begin
  Result := FActive;
end;

procedure TXMLDocument.SetActive(const AValue: Boolean);
begin
  FActive := AValue;
end;

procedure TXMLDocument.SaveToXML(out AXML: string);
var
  ms: TMemoryStream;
  raw: RawByteString;
begin
  // WriteXML schreibt UTF-8-Bytes; diese explizit (ohne Codepage-Umweg ueber
  // TStringStream.DataString) nach UnicodeString dekodieren.
  ms := TMemoryStream.Create;
  try
    WriteXML(Doc, ms);
    SetLength(raw, ms.Size);
    if ms.Size > 0 then
      Move(ms.Memory^, raw[1], ms.Size);
    AXML := UTF8ToString(raw);
  finally
    ms.Free;
  end;
end;

procedure TXMLDocument.SaveToStream(AStream: TStream);
begin
  WriteXML(Doc, AStream);
end;

procedure TXMLDocument.SaveToFile(const AFileName: string);
begin
  WriteXMLFile(Doc, AFileName);
end;

procedure TXMLDocument.LoadFromStream(AStream: TStream);
var
  parser: TDOMParser;
  src: TXMLInputSource;
  newDoc: DOM.TXMLDocument;
begin
  newDoc := nil;
  parser := TDOMParser.Create;
  src := TXMLInputSource.Create(AStream);
  try
    // Beides ist zwingend: ohne Namespaces bleiben NamespaceURI/LocalName leer
    // und jede praefixbehaftete XPath-Abfrage laeuft ins Leere; ohne
    // PreserveWhitespace=False landet die Einrueckung eingerueckter Dokumente
    // im TextContent. MSXML verhaelt sich per Default wie hier eingestellt.
    parser.Options.Namespaces := True;
    parser.Options.PreserveWhitespace := False;
    parser.Parse(src, newDoc);
  finally
    src.Free;
    parser.Free;
  end;
  (FOwner as IDomOwner).ReplaceDoc(newDoc);
  FActive := True;
end;

procedure TXMLDocument.LoadFromFile(const AFileName: string);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(fs);
  finally
    fs.Free;
  end;
end;

procedure TXMLDocument.LoadFromXML(const AXML: string);
var
  ms: TMemoryStream;
  raw: RawByteString;
begin
  raw := UTF8Encode(AXML);
  ms := TMemoryStream.Create;
  try
    if System.Length(raw) > 0 then
      ms.Write(raw[1], System.Length(raw));
    ms.Position := 0;
    LoadFromStream(ms);
  finally
    ms.Free;
  end;
end;

function TXMLDocument.AsDOMDocument: IXMLDOMDocument2;
begin
  Result := TDOMNodeShim.Create(FOwner as IDomOwner, Doc);
end;

function NewXMLDocument: IXMLDocument;
begin
  Result := TXMLDocument.Create(nil);
end;

end.
