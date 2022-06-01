unit Net.Data;

interface

uses
  System.Classes, System.Generics.Collections,
  Net.Types, Net.Stream, Net.Svr, Net.Cnst, Net.Date, Net.SvrData,
  Crypts;

type
  TNullEnumerator<T: class> = class(TEnumerator<T>)
  protected
    function DoGetCurrent: T; override;
    function DoMoveNext: Boolean; override;
  end;

  TAccess = class
  private
    FAccess: integer;
    function GetAppend: boolean;
    function GetChangePermissions: boolean;
    function GetDelete: boolean;
    function GetDeleteItems: boolean;
    function GetExecute: boolean;
    function GetRead: boolean;
    function GetReadAttr: boolean;
    function GetReadPermissions: boolean;
    function GetTakeOwnership: boolean;
    function GetWrite: boolean;
    function GetWriteAttr: boolean;
    function Test(AAccess: integer): boolean;
  protected
    property Access: integer read FAccess write FAccess;
  public
    property Append: boolean read GetAppend;
    property ChangePermissions: boolean read GetChangePermissions;
    property Delete: boolean read GetDelete;
    property DeleteItems: boolean read GetDeleteItems;
    property Execute: boolean read GetExecute;
    property Read: boolean read GetRead;
    property ReadAttr: boolean read GetReadAttr;
    property ReadPermissions: boolean read GetReadPermissions;
    property TakeOwnership: boolean read GetTakeOwnership;
    property Write: boolean read GetWrite;
    property WriteAttr: boolean read GetWriteAttr;
  end;

  TListRequest = class(TNetItem)
  private
    FCount: integer;
    FIndex: integer;
    FObjectName: string;
  protected
    function GetSize: integer; override;
  public
    constructor CreateRequest(const AObjectName: string; AIndex, ACount: integer);
    function LoadFromBuffer(ABuffer: TIOBuffer): boolean; override;
    function ValidIndex(AMax: integer): boolean;
    procedure SaveToBuffer(ABuffer: TIOBuffer); override;
    property Count: integer read FCount write FCount;
    property Index: integer read FIndex write FIndex;
    property ObjectName: string read FObjectName write FObjectName;
  end;

  TReadARequest = packed record
    BufferNumber: Word;
    ParamIndex: Word;
    StartDate: TNetDate;
    Count: Cardinal;
  end;
  TReadARequestInfo = class(TNetValue<TReadARequest>);

  TReadAReplyInfo = class(TNetItemList)
  type
    TReadAReplyHeader = packed record
      Date: TNetDate;
      Count: integer;
    end;
    TReadAReplyHeaderInfo = class(TNetValue<TReadAReplyHeader>);
  private
    FHeader: TReadAReplyHeaderInfo;
    FStream: TNetStream;
  public
    constructor Create; override;
    property Header: TReadAReplyHeaderInfo read FHeader;
    property Stream: TNetStream read FStream;
  end;

  TWriteInfo = record
    Mistiming: integer;
    Status: integer;
    constructor Create(AMistiming, AStatus: integer);
  end;

  TWriteReplyInfo = class(TNetValue<TWriteInfo>);

implementation

const
  ValueKey: TAESCipherKey128 = ;

{ TNullEnumerator<T> }

function TNullEnumerator<T>.DoGetCurrent: T;
begin
  Result := nil;
end;

function TNullEnumerator<T>.DoMoveNext: Boolean;
begin
  Result := False;
end;


{ TAccess }

function TAccess.GetAppend: boolean;
begin
  Result := Test(ACCESS_APPEND);
end;

function TAccess.GetChangePermissions: boolean;
begin
  Result := Test(ACCESS_CHANGE_PERMISSIONS);
end;

function TAccess.GetDelete: boolean;
begin
  Result := Test(ACCESS_DELETE);
end;

function TAccess.GetDeleteItems: boolean;
begin
  Result := Test(ACCESS_DELETE_ITEMS);
end;

function TAccess.GetExecute: boolean;
begin
  Result := Test(ACCESS_EXECUTE);
end;

function TAccess.GetRead: boolean;
begin
  Result := Test(ACCESS_READ);
end;

function TAccess.GetReadAttr: boolean;
begin
  Result := Test(ACCESS_READ_ATTR);
end;

function TAccess.GetReadPermissions: boolean;
begin
  Result := Test(ACCESS_READ_PERMISIONS);
end;

function TAccess.GetTakeOwnership: boolean;
begin
  Result := Test(ACCESS_TAKE_OWNERSHIP);
end;

function TAccess.GetWrite: boolean;
begin
  Result := Test(ACCESS_WRITE);
end;

function TAccess.GetWriteAttr: boolean;
begin
  Result := Test(ACCESS_WRITE_ATTR);
end;

function TAccess.Test(AAccess: integer): boolean;
begin
  Result := FAccess and AAccess <> 0;
end;

{ TListRequest }

constructor TListRequest.CreateRequest(const AObjectName: string; AIndex, ACount: integer);
begin
  inherited Create;

  FObjectName := AObjectName;
  FIndex := AIndex;
  FCount := ACount;
end;

function TListRequest.GetSize: integer;
begin
  Result := GetStringSerializeLength(ObjectName) + SizeOf(Index) + SizeOf(Count);
end;

function TListRequest.LoadFromBuffer(ABuffer: TIOBuffer): boolean;
begin
  Result := inherited LoadFromBuffer(ABuffer);
  if not Result then Exit;

  try
    FObjectName := ABuffer.ReadString;
    FCount := ABuffer.ReadInteger;
    FIndex := ABuffer.ReadInteger;
  except
    Result := False;
  end;
end;

procedure TListRequest.SaveToBuffer(ABuffer: TIOBuffer);
begin
  inherited;
  ABuffer.WriteString(ObjectName);
  ABuffer.WriteInteger(Count);
  ABuffer.WriteInteger(Index);
end;

function TListRequest.ValidIndex(AMax: integer): boolean;
begin
  if (Index < 0) or (Count < 0) then
    Exit(False);

  if (Index + Count) > AMax then
  begin
    Count := AMax - Index;

    if Count < 0 then
      Count := 0;
  end;

  Result := True;
end;

{ TReadAReplyInfo }

constructor TReadAReplyInfo.Create;
begin
  inherited Create;

  FHeader := TReadAReplyHeaderInfo.Create;
  FStream := TNetStream.Create;

  Put(Header, True);
  Put(Stream, True);
end;

{ TWriteInfo }

constructor TWriteInfo.Create(AMistiming, AStatus: integer);
begin
  Mistiming := AMistiming;
  Status := AStatus;
end;

end.
